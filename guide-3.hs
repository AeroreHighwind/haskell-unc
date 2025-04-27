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
    