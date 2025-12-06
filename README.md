# Estructuras Discretas 2026-1

## Proyecto: Árboles n-arios

## Integrantes

    Martinez Briones Erick Xavier
        -No. de Cuenta: 426007742
    Quintana López Luis Fernando
        -No. de Cuenta: 323023092 


## Comentarios

1:

Antes de explicar el por qué de la implementación del arbol n-ario, nos parece importante dar ejemplos de como ejecutar las
funciones en la terminal. 

En el caso de la seccion de "ARBOL DE SINTAXIS ABSTRACTA", para ejecutar las funciones que reciben un arbol como argumento 
debe usarse un arbol como el del siguiente ejemplo:

(Node AndOp [Node OrOp [Node NotOp [Node (VarOp "r") []], Node (VarOp "q") []],Node (VarOp "p") []])

Esto se debe a que en este caso los arboles unicamente pueden tener en su raiz elementos de tipo "operador" (para ver todos
los operadores que pueden ser usados se puede consultar la definicion de "Operador" que se encuentra en la segunda linea de 
proyecto.hs) 

Para funciones donde se recibe como argumento un arbol cuyas raices son de tipo "a" la situcion se simplifica, porque
se puede usar cualquier tipo de dato como raiz. Basicamente el argumento debe tener la misma estructura del ejemplo 
antes mencionado (pero con la posibilidad de usar enteros, strings o cualquier tipo de dato). 

Cuando las funciones reciben como argumento una proposicion, el argumento debe tener una estructura similar a la del 
siguiente ejemplo:

(Impl (And (And (Impl (Var "p") (Not (Var "q"))) (Impl (Var "s") (Var "q"))) (Var "s")) (Var "p"))




2: 

Para hacer la implementación del árbol n-ario primero analizamos detalladamente la implementación del árbol binario realizada en clase (durante el laboratorio). 
Al igual que siempre, tenemos un caso base y una función constructora que nos permite crear nuevos árboles a partir de los que ya tenemos. 

Para el árbol binario el caso base era Void y la función constructora era "Node" seguida de un elemento de tipo A y dos árboles. 
Aquí es donde estaba el principal problema, pues la idea intuitiva es aumentar la cantidad de árboles que recibe la función constructora. Por ejemplo:

data Arbol a = Void | Node a (Arbol a) (Arbol a) (Arbol a) ...

Esto claramente tiene muchos problemas. Pero el principal de ellos es que aunque es posible pasar tantos árboles como se desee (al definir el tipo), técnicamente no estaríamos definiendo un árbol n-ario, ya que podríamos modelar un árbol 100-ario por ejemplo, pero al ser 100-ario estaría obligado a que TODOS los árboles posean 100 hijos, cosa que no es lo que buscamos, ya que podríamos querer algún árbol con menos de 100 hijos o con muchos más de 100. 


Entonces, hacer la implementación de este modo no es posible. 

Ahora bien, teníamos que buscar la manera de que un árbol pudiera construirse a partir de un elemento de tipo A y una cantidad arbitraria de hijos. Aquí es donde entra el tema de las listas, pues incluso durante las clases en el laboratorio se mencionó que muy probablemente debíamos hacer uso de ellas. Esto por supuesto tiene todo el sentido del mundo, pues esta estructura (las listas) nos permite "almacenar" muchos elementos del mismo tipo. 

Por lo anterior, fue fácil deducir que los árboles deberían definirse como Node a [Arbol a]
Es decir, para construir un árbol necesitamos el valor de la raíz y una lista con TODOS los "hijos" o sub-arboles. 

Esta fue finalmente la implementación utilizada:
data Arbol a = Void | Node a [Arbol a] deriving(Eq, Show)


Escribir funciones que trabajen con este tipo de dato no fue difícil, pues el proceso es muy similar al de los árboles binarios.
Simplemente, dependiendo del caso, hacemos alguna acción con el valor de la raíz y posteriormente hacemos recursión aplicando la función a cada uno de los sub-arboles (que en este caso están en la lista). 
