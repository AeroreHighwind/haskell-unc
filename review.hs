{-# LANGUAGE NPlusKPatterns #-}

soloPositivos :: [Int] -> [Int]
soloPositivos [] = []
soloPositivos (x : xs)
  | x >= 0 = x : soloPositivos xs
  | otherwise = soloPositivos xs

mayoresQue10 :: [Int] -> [Int]
mayoresQue10 [] = []
mayoresQue10 (x : xs)
  | x > 10 = x : mayoresQue10 xs
  | otherwise = mayoresQue10 xs

soloPares :: [Int] -> [Int]
soloPares [] = []
soloPares (x : xs)
  | x `mod` 2 == 0 = x : soloPares xs
  | otherwise = soloPares xs

cambiarSigno :: [Int] -> [Int]
cambiarSigno [] = []
cambiarSigno (x : xs)
  | x == 0 = x : cambiarSigno xs
  | otherwise = -x : cambiarSigno xs

multiplica :: Int -> [Int] -> [Int]
multiplica z [] = []
multiplica z (x : xs) = z * x : multiplica z xs
