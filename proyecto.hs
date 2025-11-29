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

-- Fórmulas proposicionales (Variables atómicas)
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"



-- ----------------------------------------------------------------------
-- funcion que toma una proposicion y crea su arbol de sintaxis abstracta
-- ----------------------------------------------------------------------
arbolDeSintaxisAbstracta :: Prop -> Arbol String
arbolDeSintaxisAbstracta (Var x) = (Node x []) 
arbolDeSintaxisAbstracta (Cons _) = Void
arbolDeSintaxisAbstracta (Not p) = (Node "~" [arbolDeSintaxisAbstracta p] )
arbolDeSintaxisAbstracta (And p q) = (Node "^" [arbolDeSintaxisAbstracta p, arbolDeSintaxisAbstracta q])
arbolDeSintaxisAbstracta (Or p q) =  (Node "v" [arbolDeSintaxisAbstracta p, arbolDeSintaxisAbstracta q])
arbolDeSintaxisAbstracta (Impl p q) = (Node "=>" [arbolDeSintaxisAbstracta p, arbolDeSintaxisAbstracta q])
arbolDeSintaxisAbstracta (Syss p q) = (Node "<=>" [arbolDeSintaxisAbstracta p, arbolDeSintaxisAbstracta q])


-- ------------------------------------------------------------------------------------------------------------------------
-- funcion que recibe un arbol de sintaxis abstracta y regresa la formula de la logica proposiiconal asociada a dicho arbol
-- ------------------------------------------------------------------------------------------------------------------------
devuelveFormula :: Arbol String -> Prop
devuelveFormula (Node "Var" [Node x []]) = (Var x)
devuelveFormula Void = Cons True
devuelveFormula (Node "~" [p]) = (Not (devuelveFormula p))
devuelveFormula (Node "^" [p, q]) = (And (devuelveFormula p) (devuelveFormula q))
devuelveFormula (Node "v" [p, q]) = (Or (devuelveFormula p) (devuelveFormula q))
devuelveFormula (Node "=>" [p, q]) = (Impl (devuelveFormula p) (devuelveFormula q))
devuelveFormula (Node "<=>" [p, q]) = (Syss (devuelveFormula p) (devuelveFormula q))


-- -----------------------------------------------------
-- funcion que cuenta el numero de elementos de un arbol
-- -----------------------------------------------------
numeroElementos :: Arbol a -> Int
numeroElementos Void = 0
numeroElementos (Node x []) = 1
numeroElementos (Node x [y]) = 1 + numeroElementos y
numeroElementos (Node x (y:ys)) = 1 + numeroElementos y + contarElemenLista ys
    where
        contarElemenLista [] = 0
        contarElemenLista (x:xs) = numeroElementos x + contarElemenLista xs


-- -----------------------------------------
-- funcion que busca un elemento en un arbol
-- -----------------------------------------
busca :: Eq a => Arbol a -> a -> Bool
busca Void _ = False
busca (Node x ys) y =
                     if x == y 
                          then True
                          else buscaEnLista ys y

-- funcion auxiliar que busca un elemento en una lista de arboles
buscaEnLista :: Eq a => [Arbol a] -> a -> Bool
buscaEnLista [] _ =  False
buscaEnLista (t:ts) y = 
    if busca t y
        then True
        else buscaEnLista ts y


-- -----------------------------------------
-- Funcion que calcula la altura de un arbol
-- -----------------------------------------
altura :: Arbol a -> Int
altura Void = 0
altura (Node x []) = 1
altura (Node x (y:ys)) = 1 + maximo (altura y) (alturaListaArboles ys)
    where 
        alturaListaArboles [] = 0
        alturaListaArboles (x:xs) = maximo (altura x) (alturaListaArboles xs)

-- Funcion auxiliar que calcula el maximo entre 2 numeros
maximo :: Int -> Int -> Int
maximo x y = if x > y then x else y


-- ------------------------------------------
-- Funcion que suma los elementos de un arbol
-- ------------------------------------------
sumaElementos :: Arbol Int -> Int
sumaElementos Void = 0
sumaElementos (Node x []) = x
sumaElementos (Node x (y:ys)) = x + sumaElementos y + sumaElementosLista ys
    where 
        sumaElementosLista [] = 0  
        sumaElementosLista [x] = sumaElementos x
        sumaElementosLista (x:xs) = sumaElementos x + sumaElementosLista xs


-- -------------------------------------
-- (Espejo) funcion que voltea el arbol
-- -------------------------------------
espejo :: Arbol a -> Arbol a
espejo Void = Void
espejo (Node x []) = (Node x [])
espejo (Node x (y:ys)) = (Node x (snoc (espejoEnListaDeArboles ys) y))
    where
        espejoEnListaDeArboles [] = []
        espejoEnListaDeArboles (x:xs) = xs ++ [(espejo x)]   

-- Funcion auxiliar snoc
snoc :: [a] -> a -> [a]
snoc [] x = [x]
snoc xs y = xs ++ [y]
