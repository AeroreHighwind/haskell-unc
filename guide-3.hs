{-# LANGUAGE NPlusKPatterns #-}
--T-UPLES
addPairs :: (Int,Int) -> (Int,Int) -> (Int,Int)
addPairs (a, b) (c,d) = (a+c, b+d)

second3 :: (Int,Int,Int) -> Int
second3 (a,b,c) = b

paramPriceRange :: Int -> (Int,Int) -> String
paramPriceRange val (minPrice, maxPrice)    | val < minPrice = "Too cheap"
                                            | val >= minPrice && val<= maxPrice = "Lets see" 
                                            | val > maxPrice = "Too expensive"
                                            | val < 0 = "That is not possible"

greaterThan3 :: (Int,Int,Int) -> (Bool, Bool, Bool)
greaterThan3 (a,b,c) = (a>3, b>3, c>3)

areAllTheSame:: (Int,Int,Int) -> Bool
areAllTheSame (a,b,c) = a==b && b==c


getFinalCondition:: (String, Int, Int, Int) -> (String, String)
getFinalCondition (name, t1,t2,t3)  | t1>=7 && t2>=7 && t3>=7 = (name, "Promotion")
getFinalCondition (name, t1,t2,t3)  | (t1<7 && t1>=4) || (t2<7 && t2>= 4) || (t3<7 && t3 >=4) = (name, "Regular")
                                    | otherwise = (name, "Independent")


-- RECURSION FILTER

filterEvens :: [Int] -> [Int]
filterEvens [] = []
filterEvens (x:xs)  | even x = x: filterEvens xs
                    | otherwise = filterEvens xs


filterGreaterThan10 :: [Int] -> [Int]
filterGreaterThan10 [] = []
filterGreaterThan10 (x:xs)      |  x >10 = x: filterGreaterThan10 xs
                                | otherwise = filterGreaterThan10 xs


filterGreaterThanN :: Int -> [Int] -> [Int]

filterGreaterThanN _ [] = []
filterGreaterThanN n (x:xs)     | x > n = x : filterGreaterThanN n xs
                                | otherwise = filterGreaterThanN n xs


-- RECURSION MAPPING

changeSign :: [Int] -> [Int]
changeSign [] = []
changeSign (x:xs) = -x : changeSign xs 


addOne :: [Int] -> [Int]
addOne [] = []
addOne (x:xs) = x+1 : addOne xs

duplicate :: [Int] -> [Int]
duplicate [] = []
duplicate (x:xs) = 2*x : duplicate xs

multiply:: Int -> [Int] -> [Int]
multiply _ [] = []
multiply n (x:xs) = n*x : multiply n xs


-- RECURSION ACCUMULATOR

areAllLesserThan10 :: [Int] -> Bool
areAllLesserThan10 [] = True
areAllLesserThan10 (x:xs)   | x < 10 = areAllLesserThan10 xs 
                            | otherwise = False

containsZero :: [Int] -> Bool
containsZero [] = False
containsZero (x:xs) | x == 0 = True
                    | otherwise = containsZero xs

cuadraticProduct :: [Int] -> Int
cuadraticProduct [] = 1
cuadraticProduct (x:xs) = x*x * cuadraticProduct xs 


--8
getMaximum :: [Int] -> Int
getMaximum [x] = x
getMaximum (x:xs) = max x (getMaximum xs)



addPairsV2 :: [(Int, Int)] -> Int
addPairsV2 [] = 0
addPairsV2 ((x,y):xs) = x + y + addPairsV2 xs

areAllZeroesOrOnes :: [Int] -> Bool
areAllZeroesOrOnes [] = True
areAllZeroesOrOnes (x:xs)   | x == 0 || x == 1 = areAllZeroesOrOnes xs
                            | otherwise = False

removeZeroes :: [Int] -> [Int]
removeZeroes [] = []
removeZeroes (x:xs) | x == 0 = removeZeroes xs
                    | otherwise = x : removeZeroes xs


getLastElement :: [a] -> a
getLastElement [x] = x
getLastElement (x:xs) = getLastElement xs


--FANCY VERSION
--getLastElement :: [a] -> Maybe a
--getLastElement [] = Nothing
--getLastElement [x] = Just x
--getLastElement (x:xs) = getLastElement xs


repeatElement  :: Int -> Int -> [Int]
repeatElement 0 k = []
repeatElement (n + 1) k  = k : repeatElement n k


chainLists :: [[a]] -> [a]
chainLists [] = []
chainLists (x:xs) = x ++ chainLists xs 


soloPares :: [Int] -> [Int]

soloPares [] = []

soloPares (x:xs)
    | even x = x : soloPares xs
    | otherwise = soloPares xs


mayoresQue :: Int -> [Int] -> [Int]

mayoresQue n [] = []

mayoresQue n (x:xs)
        | x > n = x : mayoresQue n xs
        | otherwise = mayoresQue n xs



hay0 :: [Int] -> Bool
hay0 [] = False
hay0 (x:xs)
    | x == 0 = True
    | otherwise = hay0 xs





apellidos :: [(String, String, Int)] -> [String]

apellidos [] = []
apellidos ((nombre,apellido,edad):xs) = apellido : apellidos xs
    
