import Parser
import Data.List
import System.Environment
import System.IO
import Data.Char
import System.IO
import qualified Data.Map.Strict as Map

--------------------------------------
-- Data types

data Code = PUSH Value
          | FETCH String
          | ADD
          | MULT
          | SUB
          | DIV
          | TRUE
          | FALSE
          | EQ'
          | LE
          | NEG
          | AND
          | STORE String
          | NOOP
          | PRINT
          | READ_INT
          | BLOCK [Code]
          | BRANCH [Code] [Code]
          | LOOP [Code] [Code]
          | TRY [Code] [Code]
          | CATCH [Code]
            deriving Show

data Value = Z Integer
           | B Bool
           | S String
           | Exception
             deriving Show
type Exceptional = Bool
type Stack = [Value]
type State = (Map.Map String Value, Exceptional)
type AM    = ([Code], Stack, State)

-- Example program for computing abs diff of two arbitrarily large integers.
-- And an ugly hack for breaking once EOF is reached.
program = [ "a := 1"
          , "while a = 1"
          , "  try"
          , "    x := read_int"
          , "    y := read_int"
          , "    if x <= y"
          , "      then print y - x"
          , "      else print x - y"
          , "  catch"
          , "    a := 2"]

--------------------------------------
-- IO

main = do args <- getArgs
          input <- readFile (head args)
          -- let input = unlines program
          do (case (parseProgram input) of
              (Left err) -> putStrLn err -- Parser error
              (Right ast) -> do let code = (fmap cS . parseProgram) input
                                    debug = any (== "--debug") args
                                    byteCode = any (== "--code") args
                                if byteCode
                                   then putStrLn $ show code
                                   else mapM_ (run debug) code)

-- Initial program runs with an empty stack, a non-exceptional state and a
-- sequece of Code instructions
run :: Bool -> [Code] -> IO AM
run debug c = run' (c, [], (Map.empty, False)) debug

run' :: AM -> Bool -> IO AM
run' ([], e, (s, err)) _ = do -- putStrLn $ "Final State: " ++ showState (s, err)
                              -- putStrLn $ showOutput (s, err)
                              return ([], e, (s, err))
run' am debug = do next <- (execIO am)
                   -- putStrLn $ "> " ++ (showAM am)
                   if debug then step else return ()
                   run' next debug

-- Stepping through the program with the --debug flag.
step :: IO ()
step = do hSetEcho stdin False
          c <- getChar
          if (c == '\n') then return () else step

readInteger :: IO Integer
readInteger = do string <- readString
                 return (read string)

readString :: IO String
readString = do first <- readTillNonSpace
                rest  <- readTillSpace ""
                return (first:rest)

readTillSpace :: String -> IO String
readTillSpace acc = do c <- getChar
                       if isSpace c
                          then return (reverse acc)
                          else readTillSpace (c:acc)

readTillNonSpace :: IO Char
readTillNonSpace = do c <- getChar
                      if isSpace c
                         then readTillNonSpace
                         else return c

--------------------------------------
-- AM Compilation

cA :: Aexp -> [Code]
cA (Num n)         = [PUSH (Z n)]
cA (StringLit str) = [PUSH (S str)]
cA (Ident id)      = [FETCH id]
cA (Add a1 a2)     = (cA a2) ++ (cA a1) ++ [ADD]
cA (Mult a1 a2)    = (cA a2) ++ (cA a1) ++ [MULT]
cA (Sub a1 a2)     = (cA a2) ++ (cA a1) ++ [SUB]
cA (Div a1 a2)     = (cA a2) ++ (cA a1) ++ [DIV]
cA (ReadInt)       = [READ_INT]

cB :: Bexp -> [Code]
cB (Val True)   = [TRUE]
cB (Val False)  = [FALSE]
cB (Eq a1 a2)   = (cA a2) ++ (cA a1) ++ [EQ']
cB (Lteq a1 a2) = (cA a2) ++ (cA a1) ++ [LE]
cB (Neg b)      = (cB b) ++ [NEG]
cB (And b1 b2)  = (cB b2) ++ (cB b1) ++ [AND]

cS :: Stm -> [Code]
cS (Assn id a)  = (cA a) ++ [STORE id]
cS (Skip)       = [NOOP]
cS (Comp s1 s2) = (cS s1) ++ (cS s2)
cS (If b s1 s2) = (cB b) ++ [BRANCH (cS s1) (cS s2)]
cS (While b s)  = [LOOP (cB b) (cS s)]
cS (Try s1 s2)  = [TRY (cS s1) (cS s2)]
cS (Print aexp) = (cA aexp) ++ [PRINT]

-- cs (ReadNum )

--------------------------------------
-- AM Execution

execIO :: AM -> IO AM

-- Do not allow exceptional state to propagate
execIO (CATCH c1:c, e, (s, True)) = return (c1 ++ c, e, (s, False))
execIO (_:c, e, (s, True))        = return (c, e, (s, True))

-- Handle IO
execIO (PRINT:c, n:e, s) =
  do putStrLn $ showValue n
     execIO (c, e, s)
execIO (READ_INT:c, e, s) =
  do eof <- isEOF
     if eof
     then return (c, Exception:e, s)
     else do n <- readInteger
             return (c, Z n:e, s)

-- Handle non-io related code
execIO s = return $ exec s

exec :: AM -> AM
exec ([], e, s)                           = ([], e, s)
exec (PUSH n:c, e, s)                     = (c, n:e, s)
exec (ADD:c, Z z1:Z z2:e, s)              = (c, Z (z1 + z2):e, s)
exec (ADD:c, S str:Z z:e, s)              = (c, S (str ++ (show z)):e, s)
exec (ADD:c, Z z:S str:e, s)              = (c, S ((show z) ++ str):e, s)
exec (ADD:c, S s1:S s2:e, s)              = (c, S (s1 ++ s2):e, s)
exec (MULT:c, Z z1:Z z2:e, s)             = (c, Z (z1 * z2):e, s)
exec (MULT:c, S str:Z z:e, s)             = (c, S (concat (take (fromIntegral z) (repeat str))):e, s)
exec (MULT:c, S str1:S str2:e, s)         = (c, S (concat [c1:c2:[] | c1 <- str1, c2 <- str2]):e, s)
exec (SUB:c, Z z1:Z z2:e, s)              = (c, Z (z1 - z2):e, s)
exec (SUB:c, S str:Z z:e, s)              = (c, S (take (length str - (fromIntegral z)) str):e, s)
exec (DIV:c, Z z1:Z 0:e, s)               = (c, Exception:e, s)
exec (DIV:c, Z z1:Z z2:e, s)              = (c, Z (div z1 z2):e, s)
exec (TRUE:c, e, s)                       = (c, B True:e, s)
exec (FALSE:c, e, s)                      = (c, B False:e, s)
exec (EQ':c, Z z1: Z z2:e, s)             = (c, B (z1 == z2):e, s)
exec (LE:c, Z z1: Z z2:e, s)              = (c, B (z1 <= z2):e, s)
exec (AND:c, B True: B True:e, s)         = (c, B True:e, s)
exec (AND:c, B _: B _:e, s)               = (c, B False:e, s)
exec (NEG:c, B b:e, s)                    = (c, B (not b):e, s)
exec (FETCH id:c, e, (s, b))              = (c, (s Map.! id):e, (s,b))
exec (STORE id:c, z:e, (s, True))         = (c, e, (s, True))
exec (STORE id:c, Exception:e, (s, b))    = (c, e, (s, True))
exec (STORE id:c, z:e, (s, b))            = (c, e, (Map.insert id z s, b))
exec (NOOP:c, e, (s, True))               = (c, e, (s, True))
exec (NOOP:c, e, s)                       = (c, e, s)
exec (BRANCH _ _:c, B True:e, (s, True) ) = (c, e, (s, True))
exec (BRANCH c1 _:c, B True:e, s)         = (c1 ++ c, e, s)
exec (BRANCH _ c2:c, B False:e, s)        = (c2 ++ c, e, s)
exec (TRY _ _:c, e, (s, True))            = (c, e, (s, True))
exec (TRY c1 c2:c, e, s)                  = (c1 ++ [CATCH c2] ++ c, e, s)
exec (CATCH c1:c, e, (s, True))           = (c1 ++ c, e, (s, False))
exec (CATCH c1:c, e, s)                   = (c, e, s)
exec (LOOP c1 c2:c, e, (s, True))         = (c, e, (s, True))
exec (LOOP c1 c2:c, e, s) = (expanded ++ c, e, s)
  where expanded = c1 ++ [BRANCH (c2 ++ [LOOP c1 c2]) [NOOP]]
exec (ADD:c, _:_:e, s)  = (c, Exception:e, s)
exec (MULT:c, _:_:e, s) = (c, Exception:e, s)
exec (SUB:c, _:_:e, s)  = (c, Exception:e, s)
exec (DIV:c, _:_:e, s)  = (c, Exception:e, s)
exec (EQ':c, _:_:e, s)  = (c, Exception:e, s)
exec (LE:c, _:_:e, s)   = (c, Exception:e, s)
exec (AND:c, _:_:e, s)  = (c, Exception:e, s)
exec (NEG:c, _:e, s)    = (c, Exception:e, s)

--------------------------------------
-- Pretty Printing

showAM :: AM -> String
showAM (c, e, s) = "⟨" ++ (intercalate ", " [code, (showList' . map showValue) e, showState s]) ++ "⟩"
  where code = case c of
                 [] -> showList' c
                 (c1:_) -> (showCode c1) ++ ":c"

showList' :: Show a => [a] -> String
showList' [] = "𝜀"
showList' l  = (filter (/= '"') . intercalate ":" . map show) l

showState :: State -> String
showState (m, exceptional) = "(s[" ++ (f m) ++ "], " ++ status ++ ")"
  where f = intercalate ", " . map snd . Map.toList . Map.mapWithKey (\k v -> k ++ "↦" ++ (showValue v))
        status = if exceptional then "⊥" else "⊤"

showValue :: Value -> String
showValue (Z n)    = show n
showValue (B b)    = show b
showValue (S str)  = str
showValue (Exception) = "Exception"

showCode :: Code -> String
showCode (LOOP _ _) = "LOOP(..)"
showCode (BRANCH _ _) = "BRANCH(..)"
showCode (TRY _ _) = "TRY(..)"
showCode (CATCH _) = "CATCH(..)"
showCode (STORE id) = "STORE-" ++ id
showCode (FETCH id) = "FETCH-" ++ id
showCode c = show c

showOutput :: State -> String
showOutput (s, _) = if Map.member "output" s then showValue (s Map.! "output") else "No output"
