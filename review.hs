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

-- acummulators

prodCuadrados :: [Int] -> Int
prodCuadrados [] = 1
prodCuadrados (x : xs) = (x * x) * prodCuadrados xs

todosMenores10 :: [Int] -> Bool
todosMenores10 [] = True
todosMenores10 (x : xs)
  | x >= 10 = False
  | otherwise = todosMenores10 xs

quitar0s :: [Int] -> [Int]
quitar0s [] = []
quitar0s (x : xs)
  | x == 0 = quitar0s xs
  | otherwise = x : quitar0s xs

maximo :: [Int] -> Int
maximo [x] = x
maximo (x : xs)
  | x > maximo xs = x
  | otherwise = maximo xs

todos0y1 :: [Int] -> Bool
todos0y1 [] = True
todos0y1 (x : xs)
  | x /= 0 && x /= 1 = False
  | otherwise = todos0y1 xs

ultimo :: [a] -> a
ultimo [x] = x
ultimo (_ : xs) = ultimo xs

rev :: [a] -> [a]
rev [] = []
rev (x : xs) = rev xs ++ [x]

-- zip fn
repartir :: [String] -> [String] -> [(String, String)]
repartir [] [] = []
repartir (x : xs) (y : ys) = (x, y) : repartir xs ys

-- unzip
apellidos :: [(String, String, Int)] -> [String]
apellidos [] = []
apellidos ((nombre, apellido, edad) : xs) = apellido : apellidos xs

-- custom

myLength :: [a] -> Int
myLength [] = 0
myLength (x : xs) = 1 + myLength xs

myIndex :: [a] -> Int -> a
myIndex [] _ = error "Index too big"
myIndex (x : _) 0 = x
myIndex (_ : xs) inx = myIndex xs (inx - 1)

myHead :: [a] -> a
myHead [] = error "empty list"
myHead (x : _) = x

myTail :: [a] -> a
myTail [] = error "empty list"
myTail [x] = x
myTail (_ : xs) = myTail xs

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake n _ | n <= 0 = []
myTake n (x : xs) = x : myTake (n - 1) xs

myDrop :: [a] -> Int -> [a]
myDrop [] _ = []
myDrop arr 0 = arr
myDrop (x : xs) n
  | n > 0 = myDrop xs (n - 1)
  | otherwise = xs

myConcat :: [a] -> [a] -> [a]
myConcat [] arr = arr
myConcat (x : xs) arr = x : myConcat xs arr

(<+:) :: [a] -> a -> [a]
[] <+: y = [y]
(x : xs) <+: y = x : (xs <+: y)
