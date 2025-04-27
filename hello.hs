{-# LANGUAGE NPlusKPatterns #-}
-- By giving a type we give a restriction, else it would be Number a => a -> a

cuadrado :: Int -> Int
cuadrado x = x * x

maximo :: Int -> Int -> Int
maximo x y  | x < y = y
            | x >= y = x
            | otherwise = x


--PATTERN MATCHING

esTres :: Int -> Bool
esTres 3 = True
esTres x = False

testFunction :: String -> String -> Int

testFunction var1 var2 = length var1 + length var2

sign :: Int -> Int
sign x    | x > 0 = 1
          | x < 0 = -1

bsk :: Double -> Double -> Double -> [Double]
bsk a b c
    | d < 0     = []  -- No real roots
    | otherwise = [(-b + sqrt d) / (2 * a), (-b - sqrt d) / (2 * a)]
  where
    d = b^2 - 4 * a * c  -- Discriminant


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial(n - 1)

sumatoria :: Int -> Int
sumatoria 0 = 0
--sumatoria n = n + sumatoria(n-1)
sumatoria (n + 1) = sumatoria n + n+1


dobleFactorial :: Int -> Int

dobleFactorial x 
                | x <= 0 = 1
                | x == 1 || x == 2 = x
                | otherwise = x* dobleFactorial (x - 2)