-- tipos de datos
data Operador = 
    VarOp String |
    VarGenericaOp |
    ConsOp Bool |
    NotOp |
    AndOp |
    OrOp |
    ImplOp |
    SyssOp 
    deriving (Eq)

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
    show (Or p q) = "(" ++ show p ++ " ∨ " ++ show q ++ ")"
    show (And p q) = "(" ++ show p ++ " ∧ " ++ show q ++ ")"
    show (Impl p q) = "(" ++ show p ++ " → " ++ show q ++ ")"
    show (Syss p q) = "(" ++ show p ++ " ↔ " ++ show q ++ ")"

-- Imprimir el tipo de dato Operador
instance Show Operador where
    show (VarOp p) = p 
    show (VarGenericaOp) = "Var" 
    show (ConsOp True) = "Verdadero" 
    show (ConsOp False) = "Falso" 
    show (NotOp) = "¬" 
    show (OrOp) = " ∨ "
    show (AndOp) = " ∧ "
    show (ImplOp) = " → "
    show (SyssOp) = " ↔ " 

-- Fórmulas proposicionales (Variables atómicas)
p, q, r, s, t, u :: Prop
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"

-- Sinonimo para los estados
type Estado = [String]


-- ----------------------------------
--  ARBOLES DE SINTAXIS ABSTRACTA  --
-- ----------------------------------


-- --------------------------------------------------------------------------
-- 1. funcion que toma una proposicion y crea su arbol de sintaxis abstracta
-- -------------------------------------------------------------------------
arbolDeSintaxisAbstracta :: Prop -> Arbol Operador
arbolDeSintaxisAbstracta (Var x) = (Node (VarOp x) []) 
arbolDeSintaxisAbstracta (Cons _) = Void
arbolDeSintaxisAbstracta (Not p) = (Node NotOp [arbolDeSintaxisAbstracta p] )
arbolDeSintaxisAbstracta (And p q) = (Node AndOp [arbolDeSintaxisAbstracta p, arbolDeSintaxisAbstracta q])
arbolDeSintaxisAbstracta (Or p q) =  (Node OrOp [arbolDeSintaxisAbstracta p, arbolDeSintaxisAbstracta q])
arbolDeSintaxisAbstracta (Impl p q) = (Node ImplOp [arbolDeSintaxisAbstracta p, arbolDeSintaxisAbstracta q])
arbolDeSintaxisAbstracta (Syss p q) = (Node SyssOp [arbolDeSintaxisAbstracta p, arbolDeSintaxisAbstracta q])



-- ---------------------------------------------------------------------------------------------------------------------------
-- 2. funcion que recibe un arbol de sintaxis abstracta y regresa la formula de la logica proposiiconal asociada a dicho arbol
-- ---------------------------------------------------------------------------------------------------------------------------
devuelveFormula :: Arbol Operador -> Prop
devuelveFormula Void = Cons True
devuelveFormula (Node (VarOp x) []) = (Var x)
devuelveFormula (Node (ConsOp x) []) = Cons x
devuelveFormula (Node NotOp [p]) = (Not (devuelveFormula p))
devuelveFormula (Node AndOp [p, q]) = (And (devuelveFormula p) (devuelveFormula q))
devuelveFormula (Node OrOp [p, q]) = (Or (devuelveFormula p) (devuelveFormula q))
devuelveFormula (Node ImplOp [p, q]) = (Impl (devuelveFormula p) (devuelveFormula q))
devuelveFormula (Node SyssOp [p, q]) = (Syss (devuelveFormula p) (devuelveFormula q))



-- -- ------------------------------------------------------------------------------
-- -- AUX: Función auxiliar para la funcion auxiliar evaluaArbol e interpretacion --
-- -- ------------------------------------------------------------------------------
isIn :: Eq a => a -> [a] -> Bool
isIn x [] = False 
isIn x [y] = if x == y then True else False
isIn x (y:ys) = if x == y then True else isIn x ys

-- -- ------------------------------------------------------------------------------------------------------------
-- -- AUX: Funcion auxiliar que devuelve la interpretación de la fórmula f bajo i para la funcion devuelveArbol --
-- -- ------------------------------------------------------------------------------------------------------------
interpretacion :: Prop -> Estado -> Bool
interpretacion (Var x) i = isIn x i 
interpretacion (Not p) i = not (interpretacion p i)
interpretacion (Or p q) i =  interpretacion p i || interpretacion q i
interpretacion (And p q) i =  interpretacion p i && interpretacion q i
interpretacion (Impl p q) i  = interpretacion (Not p) i || interpretacion q i
interpretacion (Syss p q) i = interpretacion (Impl p q) i && interpretacion (Impl q p) i

-----------------------------------------------------------------------------------------------------------------------------
-- 3. funcion que recibe un arbol y un estado de las variables para devolver la evaluacion de la formula asociada al arbol --
-----------------------------------------------------------------------------------------------------------------------------
evaluaArbol :: Arbol Operador -> Estado -> Bool
evaluaArbol (Node VarGenericaOp [Node (VarOp x)  []]) i = interpretacion (Var x) i
evaluaArbol (Node (VarOp x) []) i = isIn x i
evaluaArbol Void _ = False

evaluaArbol (Node NotOp [p]) i = not (evaluaArbol p i)
evaluaArbol (Node AndOp [p, q]) i = evaluaArbol p i && evaluaArbol q i
evaluaArbol (Node OrOp [p, q]) i = evaluaArbol p i || evaluaArbol q i 
evaluaArbol (Node ImplOp [p, q]) i = not (evaluaArbol p i ) || (evaluaArbol q i)
evaluaArbol (Node SyssOp [p, q]) i = (not (evaluaArbol p i ) || evaluaArbol q i) && (not (evaluaArbol q i ) || evaluaArbol p i)



-- ----------------------------------
--         OTRAS FUNCIONES         --
-- ----------------------------------


-- -----------------------------------------------------------
-- 1. funcion que cuenta el numero de elementos de un arbol --
-- -----------------------------------------------------------
cantidadElementos :: Arbol a -> Int
cantidadElementos Void = 0
cantidadElementos (Node x []) = 1
cantidadElementos (Node x [y]) = 1 + cantidadElementos y
cantidadElementos (Node x (y:ys)) = 1 + cantidadElementos y + contarElemenLista ys
    where
        contarElemenLista [] = 0
        contarElemenLista (x:xs) = cantidadElementos x + contarElemenLista xs



---------------------------------------------------------------------------------------------------
-- -- AUX: funcion auxiliar que busca un elemento en una lista de arboles para la funcion busca  --
---------------------------------------------------------------------------------------------------
buscaEnLista :: Eq a => [Arbol a] -> a -> Bool
buscaEnLista [] _ =  False
buscaEnLista (t:ts) y = 
    if busca t y
        then True
        else buscaEnLista ts y

-- -----------------------------------------------
-- 2. funcion que busca un elemento en un arbol --
-- -----------------------------------------------
busca :: Eq a => Arbol a -> a -> Bool
busca Void _ = False
busca (Node x ys) y =
                     if x == y 
                          then True
                          else buscaEnLista ys y



-- ----------------------------------------------------------------------------
-- 3. Funcion que suma los elementos de un arbol (solo funciona con enteros) --
-- ----------------------------------------------------------------------------
sumaElementos :: Arbol Int -> Int
sumaElementos Void = 0
sumaElementos (Node x []) = x
sumaElementos (Node x (y:ys)) = x + sumaElementos y + sumaElementosLista ys
    where 
        sumaElementosLista [] = 0  
        sumaElementosLista [x] = sumaElementos x
        sumaElementosLista (x:xs) = sumaElementos x + sumaElementosLista xs



-------------------------
-- 4. Funcion preOrden --
-------------------------
preorden :: Arbol a -> [a]
preorden Void = []
preorden (Node x ys) = [x] ++ recorrerHijosIz ys
    where
        recorrerHijosIz [] = []
        recorrerHijosIz (t:ts) = preorden t ++ recorrerHijosIz ts

--------------------------
-- 4. Funcion postOrden --
--------------------------
postorden :: Arbol a -> [a]
postorden Void = []
postorden (Node x ys) = recorrerHijosDe ys ++ [x]
    where
        recorrerHijosDe [] = []
        recorrerHijosDe (t:ts) = postorden t ++ recorrerHijosDe ts 
                        


--------------------------------------------------------------------
-- -- AUX: Funcion auxiliar que calcula el maximo entre 2 numeros --
--------------------------------------------------------------------
maximo :: Int -> Int -> Int
maximo x y = if x > y then x else y

-- --------------------------------------------
-- 5. Funcion que calcula la altura de un arbol
-- --------------------------------------------
altura :: Arbol a -> Int
altura Void = 0
altura (Node x []) = 1
altura (Node x (y:ys)) = 1 + maximo (altura y) (alturaListaArboles ys)
    where 
        alturaListaArboles [] = 0
        alturaListaArboles (x:xs) = maximo (altura x) (alturaListaArboles xs)



-----------------------------------
-- -- AUX: Funcion auxiliar snoc --
-----------------------------------
snoc :: [a] -> a -> [a]
snoc [] x = [x]
snoc xs y = xs ++ [y]

-- ---------------------------------------
-- 6. (Espejo) funcion que voltea el arbol --
-- ---------------------------------------
espejo :: Arbol a -> Arbol a
espejo Void = Void
espejo (Node x []) = (Node x [])
espejo (Node x (y:ys)) = (Node x (snoc (espejoEnListaDeArboles ys) y))
    where
        espejoEnListaDeArboles [] = []
        espejoEnListaDeArboles (x:xs) = xs ++ [(espejo x)] 



-----------------------------------------
-- -- AUX: Funcion auxiliar para podar --
-----------------------------------------
podarListaA :: [Arbol a] -> Int -> [Arbol a]
podarListaA [] _ = []
podarListaA (t:ts) n = podar t n : podarListaA ts n

----------------------------------------------------------------------------------------------------------------------------------------
-- 7. Funcion podar que recibe un arbol y un numero entero, que regresa el mismo arbol pero elimina todos los subarboles a profundidad n --
----------------------------------------------------------------------------------------------------------------------------------------
podar :: Arbol a -> Int -> Arbol a
podar Void _ = Void
podar (Node x _) 0 = Node x []
podar (Node x ys) n = Node x (podarListaA ys (n-1))

        

------------------------------------------------------------------------------------------------------------------------------
-- 8. Funcion que recibe un arbol y un entero n. Regresa una lista con todos los elementos que se encuentran a profunidad n --
------------------------------------------------------------------------------------------------------------------------------
elementosProfundidad :: Arbol a -> Int -> [a]
elementosProfundidad Void _ = []
elementosProfundidad (Node x _) 0 = [x]
elementosProfundidad (Node _ ys) n = elementosEnHijos ys (n-1)

elementosEnHijos :: [Arbol a] -> Int -> [a]
elementosEnHijos [] _ = []
elementosEnHijos (t:ts) n = elementosProfundidad t n ++ elementosEnHijos ts n
