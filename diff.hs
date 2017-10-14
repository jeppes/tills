main =
  do inp <- fmap (map show . map solve . map (map read') . map words . lines) getContents
     mapM_ putStrLn inp

read' :: String -> Integer
read' = read

solve (a:b:[]) = abs (a - b)
