-- 1
data ArbolInt = Hoja | Nodo Int ArbolInt ArbolInt
  deriving (Show)

arbol1 =
  Nodo
    1
    ( Nodo
        2
        Hoja
        (Nodo 4 Hoja Hoja)
    )
    ( Nodo
        3
        Hoja
        (Nodo 5 Hoja Hoja)
    )

-- 2
treeSize :: ArbolInt -> Int
treeSize Hoja = 0
treeSize (Nodo x izq der) = 1 + treeSize izq + treeSize der

-- treeHeight :: ArbolInt Int -> Int
-- altura :: ArbolInt -> Int
-- altura Hoja = 0
-- altura (Nodo _ izq der) = 1 + max (altura izq) (altura der)

-- 3
altura :: ArbolInt -> Int
altura Hoja = 0
altura (Nodo _ izq der)
  | altura izq >= altura der = 1 + altura izq
  | otherwise = 1 + altura der

-- 4
addNodes :: ArbolInt -> Int
addNodes Hoja = 0
addNodes (Nodo x izq der) = x + addNodes izq + addNodes der

-- 5
data ArbolChar = HojaChar | NodoChar Char ArbolChar ArbolChar
  deriving (Show)

-- 6
arbol2 =
  NodoChar
    'A'
    ( NodoChar
        'B'
        (NodoChar 'D' HojaChar HojaChar)
        (NodoChar 'E' HojaChar HojaChar)
    )
    ( NodoChar
        'C'
        (NodoChar 'F' HojaChar HojaChar)
        HojaChar
    )

arbol22 =
  NodoGen
    'A'
    ( NodoGen
        'B'
        (NodoGen 'D' HojaGen HojaGen)
        (NodoGen 'E' HojaGen HojaGen)
    )
    ( NodoGen
        'C'
        (NodoGen 'F' HojaGen HojaGen)
        HojaGen
    )

-- 7
ocurre :: Char -> ArbolChar -> Bool
ocurre _ HojaChar = False
ocurre t (NodoChar x izq der)
  | t == x = True
  | otherwise = ocurre t izq || ocurre t der

-- 8
arbolChar2String :: ArbolChar -> String
arbolChar2String HojaChar = ""
arbolChar2String (NodoChar x izq der) = [x] ++ arbolChar2String izq ++ arbolChar2String der

-- 9
arbolChar2StringPost :: ArbolChar -> String
arbolChar2StringPost HojaChar = ""
arbolChar2StringPost (NodoChar x izq der) = arbolChar2StringPost izq ++ arbolChar2StringPost der ++ [x]

data Arbol a = HojaGen | NodoGen a (Arbol a) (Arbol a)
  deriving (Show)

-- 10
sizeGen :: Arbol a -> Int
sizeGen HojaGen = 0
sizeGen (NodoGen x izq der) = 1 + sizeGen izq + sizeGen der

alturaGen :: Arbol a -> Int
alturaGen HojaGen = 0
alturaGen (NodoGen _ izq der)
  | alturaGen izq >= alturaGen der = 1 + alturaGen izq
  | otherwise = 1 + alturaGen der

-- 11
preorderGen :: Arbol a -> [a]
preorderGen HojaGen = []
preorderGen (NodoGen x izq der) = [x] ++ preorderGen izq ++ preorderGen der

postOrderGen :: Arbol a -> [a]
postOrderGen HojaGen = []
postOrderGen (NodoGen x izq der) = postOrderGen izq ++ postOrderGen der ++ [x]

-- 12
balanceado :: Arbol a -> Bool
balanceado HojaGen = True
balanceado (NodoGen _ izq der) = alturaGen izq == alturaGen der && balanceado izq && balanceado der

arbol11 =
  NodoGen
    1
    ( NodoGen
        2
        HojaGen
        (NodoGen 4 HojaGen HojaGen)
    )
    ( NodoGen
        3
        HojaGen
        (NodoGen 5 HojaGen HojaGen)
    )

arbolDePares =
  NodoGen
    (3, 7)
    (NodoGen (10, 4) HojaGen HojaGen)
    (NodoGen (2, 9) HojaGen HojaGen)

-- 13
-- Función que toma un árbol de pares y devuelve un árbol con el máximo de cada par.
pairwiseMax :: Arbol (Integer, Integer) -> Arbol Integer
pairwiseMax HojaGen = HojaGen
pairwiseMax (NodoGen (x, y) izq der) = NodoGen (max x y) (pairwiseMax izq) (pairwiseMax der)

-- 14
poda :: Integer -> Arbol Integer -> Arbol Integer
poda _ HojaGen = HojaGen
poda n (NodoGen x izq der)
  | x > n = HojaGen
  | otherwise = NodoGen x (poda n izq) (poda n der)

-- 15
data ArbolH a = HojaH a | NodoH (ArbolH a) (ArbolH a)
  deriving (Show)

-- 16
arbolDeBools =
  NodoH
    ( NodoH
        ( NodoH (HojaH True) (HojaH False)
        )
        ( NodoH
            (HojaH False)
            ( NodoH
                (HojaH True)
                (HojaH False)
            )
        )
    )
    (HojaH True)

arbolBools2 =
  NodoH
    ( NodoH (HojaH True) (HojaH False)
    )
    ( NodoH
        (HojaH False)
        ( NodoH
            (HojaH True)
            (HojaH False)
        )
    )

-- 17
hojas :: ArbolH a -> Int
hojas (HojaH x) = 1
hojas (NodoH izq der) = hojas izq + hojas der

-- 18
balanceadoH :: ArbolH a -> Bool
balanceadoH (HojaH a) = True
balanceadoH (NodoH izq der) =
  (hojas izq == hojas der || ((hojas izq + hojas der) `mod` 2 == 1))
    && (balanceadoH izq && balanceadoH der)

-- 19
fringe :: ArbolH a -> [a]
fringe (HojaH x) = [x]
fringe (NodoH izq der) = fringe izq ++ fringe der

-- 20
arbolOrdenado = NodoH (NodoH (HojaH 1) (HojaH 3)) (NodoH (HojaH 5) (HojaH 8))

arbolDesordenado = NodoH (NodoH (HojaH 4) (HojaH 9)) (HojaH 6)

ordenado :: ArbolH Integer -> Bool
ordenado arbol = estaOrdenada (fringe arbol)

estaOrdenada :: [Integer] -> Bool
estaOrdenada [] = True
estaOrdenada [_] = True
estaOrdenada (x : x1 : xs) = x <= x1 && estaOrdenada (x1 : xs)

-- 21
data ArbolVar a = NodoVar a [ArbolVar a]

-- 22
arbolVarInt =
  NodoVar
    1
    [ NodoVar
        2
        [ NodoVar 5 [],
          NodoVar 6 []
        ],
      NodoVar 3 [],
      NodoVar
        4
        [ NodoVar 7 [],
          NodoVar 8 [],
          NodoVar 9 [],
          NodoVar 10 []
        ]
    ]

{- gradoMax :: ArbolVar a -> Int
gradoMax (NodoVar _ []) = 0
gradoMax (NodoVar _ (x : xs))
  | 1 + length xs > gradoMax x = 1 + length xs
  | otherwise = gradoMax x
 -}

gradoMax :: ArbolVar a -> Int
gradoMax (NodoVar _ []) = 0
gradoMax (NodoVar _ (x : xs)) = gradoMaxLista (length (x : xs)) (x : xs)

gradoMaxLista :: Int -> [ArbolVar a] -> Int
gradoMaxLista m [] = m
gradoMaxLista m (x : xs)
  | gradoMax x > m = gradoMaxLista (gradoMax x) xs
  | otherwise = gradoMaxLista m xs
