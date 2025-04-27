{-# LANGUAGE NPlusKPatterns #-}

fstr :: (a, b) -> a
fstr (x, y) = x

ordinedPair :: Int -> Int -> (Int, Int)
ordinedPair x y
  | x < y = (x, y)
  | otherwise = (y, x)

processArray :: [Int] -> Int
processArray x = head x

-- length :: [a] -> Int

-- 02
averageFn :: Int -> Int -> Int
averageFn x y = (x + y) `div` 2

-- 07
mySgnFn :: Int -> Int
mySgnFn x
  | x > 0 = 1
  | x < 0 = -1
  | otherwise = 0

-- 08
isBetweenZeroAndNine :: Int -> Bool
isBetweenZeroAndNine x = x <= 9 && x >= 0

-- 09
getPriceRange :: Int -> String
getPriceRange x
  | x < 0 = "That is not possible"
  | x < 2000 = "Too cheap"
  | x > 5000 = "Too expensive"
  | x <= 5000 && x >= 2000 = "Lets see"

-- 10
absVal :: Int -> Int
absVal x
  | x > 0 = x
  | x < 0 = -x

-- 11
isMultipleOf2 :: Int -> Bool
isMultipleOf2 x = x `mod` 2 == 0

-- 12
getCuadrant :: Float -> Float -> Int
getCuadrant x y
  | x > 0 && y > 0 = 1
  | x < 0 && y > 0 = 2
  | x < 0 && y < 0 = 3
  | x > 0 && y < 0 = 4

-- 13
isMultipleOf :: Int -> Int -> Bool
isMultipleOf x y = y `mod` x == 0

-- 14
isLeapYear :: Int -> Bool
isLeapYear days = days `mod` 400 == 0 || (days `mod` 4 == 0 && days `mod` 100 /= 0)

-- 15
dispersion :: Float -> Float -> Float -> Float
dispersion x y z = max3 x y z - min3 x y z

max3 :: Float -> Float -> Float -> Float
max3 x y z = max (max x y) z

min3 :: Float -> Float -> Float -> Float
min3 x y z = min (min x y) z

-- 16
threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent x y z = x /= y && x /= z && y /= z

-- 17
celsiusToFahr :: Float -> Float
celsiusToFahr temp = (temp * 1.8) + 32

-- 18
fahrToCelsius :: Float -> Float
fahrToCelsius temp = (temp - 32) / 1.8

-- 19
isCold :: Float -> Bool
isCold temp = fahrToCelsius temp < 8

-- 20
sphereVolume :: Float -> Float
sphereVolume radius = 4 / 3 * pi * radius * radius * radius

-- 21
addBills :: Int -> Int -> Int -> Int -> Int
addBills a b c d = a * 10 + b * 20 + c * 50 + d * 100

--RECURSIVE FUNCTIONS BEGIN

--22
sumatory :: Int -> Int
sumatory n  | n == 0 = 0
            | n > 0 = n + sumatory (n - 1)

--23
potency :: Int -> Int -> Int
potency base exp    | exp == 0 = 1
                    | exp > 0 = base * potency base (exp - 1)
                    | base < 0 = 0

--24 CHECK IN FORUM
addEvens :: Int -> Int
addEvens 0 = 0
addEvens 1 = 0
addEvens (x+2) = (x+2) + addEvens x

--25 
getMCD :: Int -> Int -> Int
getMCD a b  | b == 0 = a
            | b > 0 = getMCD b (a `mod` b)

--26
getOddProd :: Int -> Int
getOddProd 1 = 1
getOddProd n
    | n <= 0 = 1
    | odd n = n * getOddProd (n - 2)
    | otherwise = getOddProd (n - 1)