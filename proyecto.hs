-- tipos de datos
data Arbol a = Void | Node a [Arbol a] deriving(Eq, Show)
data Prop = 
    Var String |
    Cons Bool |
    Not Prop |
    And Prop Prop |
    Or Prop Prop |
    Impl Prop Prop |
    Syss Prop Prop 
    deriving (Eq)

-- Imprimir el tipo de dato Prop
instance Show Prop where
    show (Cons True) = "Verdadero"
    show (Cons False) = "Falso"
    show (Var p) = p
    show (Not p) = "¬" ++ show p 
    show (Or p q) = "(" ++ show p ++ "v" ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- funcion que cuenta el numero de elementos de un arbol
numeroElementos :: Arbol a -> Int
numeroElementos Void = 0
numeroElementos (Node x []) = 1
numeroElementos (Node x [y]) = 1 + numeroElementos y
numeroElementos (Node x (y:ys)) = 1 + numeroElementos y + contarElemenLista ys
    where
        contarElemenLista [] = 0
        contarElemenLista (x:xs) = numeroElementos x + contarElemenLista xs


-- funcion que toma una proposicion y crea su arbol de sintaxis abstracta
arbolDeSintaxisAbstracta :: Prop -> Arbol String
arbolDeSintaxisAbstracta (Var x) = (Node x []) 
arbolDeSintaxisAbstracta (Cons _) = Void
arbolDeSintaxisAbstracta (Not p) = (Node "~" [arbolDeSintaxisAbstracta p] )
arbolDeSintaxisAbstracta (And p q) = (Node "^" [arbolDeSintaxisAbstracta p, arbolDeSintaxisAbstracta q])
arbolDeSintaxisAbstracta (Or p q) =  (Node "v" [arbolDeSintaxisAbstracta p, arbolDeSintaxisAbstracta q])
arbolDeSintaxisAbstracta (Impl p q) = (Node "=>" [arbolDeSintaxisAbstracta p, arbolDeSintaxisAbstracta q])
arbolDeSintaxisAbstracta (Syss p q) = (Node "<=>" [arbolDeSintaxisAbstracta p, arbolDeSintaxisAbstracta q])


