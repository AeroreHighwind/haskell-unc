-- Induction in paper
{-# LANGUAGE NPlusKPatterns #-}

data Color = Rojo | Naranja | Amarillo | Verde | Azul | Violeta
  deriving (Show)

-- data Arbol = Hoja | Nodo Int Arbol

data ArbolGenerico a = Hoja | Nodo a (ArbolGenerico a) (ArbolGenerico a)
  deriving (Show)

arbol3 :: ArbolGenerico Int
arbol3 =
  Nodo
    4
    ( Nodo
        2
        (Nodo 1 Hoja Hoja)
        (Nodo 3 Hoja Hoja)
    )
    ( Nodo
        7
        ( Nodo
            5
            Hoja
            (Nodo 6 Hoja Hoja)
        )
        (Nodo 8 Hoja Hoja)
    )

preorder :: ArbolGenerico a -> [a]
preorder Hoja = []
preorder (Nodo x izq der) = [x] ++ preorder izq ++ preorder der

inorder :: ArbolGenerico a -> [a]
inorder Hoja = []
inorder (Nodo x izq der) = inorder izq ++ [x] ++ inorder der

postorder :: ArbolGenerico a -> [a]
postorder Hoja = []
postorder (Nodo x izq der) = postorder izq ++ postorder der ++ [x]

reflejar :: ArbolGenerico a -> ArbolGenerico a
reflejar Hoja = Hoja
reflejar (Nodo x izq der) = Nodo x (reflejar der) (reflejar izq)