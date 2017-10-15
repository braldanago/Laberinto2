{-
LaberintoV2 UD 
-}
module Laberinto where

data Direccion = Norte | Este | Sur | Oeste | Inicio deriving Show
data ArbolN a valor dir = Nodo a valor dir [ArbolN a valor dir] | Fin deriving Show

a2::ArbolN (Int,Int) Char Direccion
a2 = Fin

lab::[[Char]]
lab= ["11111111",
    "100x0001",
    "10101101",
    "10100101",
    "101100f1",
    "10111101",
    "10000001",
    "11111111"]

--i/*/*/*/*/*/*/  PRINCIPAL   /*/*/*/*/*/*/i--
--juego (laberinto)
juego:: [[Char]]->[[Direccion]]
juego [] = error "Ingrese un laberinto valido (no vacio)" 
juego labto = if( (buscador (labto) 'x') && (buscador (labto) 'f') ) then starter(listaCamino (walker (gps (labto) 0 'x') labto a2))
    else error "Ingrese un laberinto valido (inicio y fin)"



--Indica si se encuentra un caracter o no--
--buscador (laberinto) (elemento a buscar)
buscador::[[Char]]->Char->Bool
buscador [] a = False
buscador (x:xs) a = if (encuentraChar x a) then True else buscador xs a

encuentraChar::String->Char->Bool
encuentraChar [] a = False
encuentraChar (x:xs) a = if (x == a) then True else encuentraChar xs a

-----------------------------------------


--Compara si dos direcciones son iguales------
comDir::Direccion->Direccion->Bool
comDir Norte Norte = True
comDir Sur Sur = True
comDir Este Este =True
comDir Oeste Oeste = True
comDir Inicio Inicio = True
comDir dir1 dir2 = False 
---------------------------------------------


--Inicializa cada lista en la lista de caminos ----
starter::[[Direccion]]->[[Direccion]]
starter [] = []
starter (x:xs) = starter2 x :starter xs

starter2 ::[Direccion]->[Direccion]
starter2 [] = []
starter2 (x:xs) = if (comDir x Inicio ) then (x:xs) else Inicio:(x:xs)
----------------------------------------------------


--Arma la lista de caminos dependiendo el arbol-----
listaCamino::ArbolN (Int,Int) Char Direccion->[[Direccion]]
listaCamino (Nodo cc valor dir []) = [[dir]] 
listaCamino (Nodo cc valor dir (x:xs))=    [concat ( [dir]:(listaCamino x ) ) ]++ concat (map (listaCamino) (xs)) 
-----------------------------------------------------


--Indica si dos tuplas son adyacentes o no
adyacente::(Int,Int)->(Int,Int)->Bool
adyacente (a,b) (c,d) = if( (a-1,b)==(c,d) || (a+1,b)==(c,d) || (a,b-1)==(c,d) || (a,b+1)==(c,d) )
    then True
    else False
------------------------------------------


---Busca una HOJA segun sus cc en el arbol----
buscarEnArbol::(Int,Int)->ArbolN (Int,Int) Char Direccion->Bool
buscarEnArbol n (Nodo cc valor dir []) = if cc == n then True else False
buscarEnArbol n (Nodo cc valor dir (y:ys)) = if n == cc then True
                                  else buscarEnArbol n y || buscarEnArbol2 n ys
buscarEnArbol2 n [] = False
buscarEnArbol2 n (y:ys) = buscarEnArbol n y || buscarEnArbol2 n ys
--------------------------------


------------------Inserta una HOJA(tupla, elemento y direccion) en el arbol-----------------------------------
--insertarArbol (tupla) (elemento) (Direccion) (Arbol)
insertarArbol::(Int,Int)->Char->Direccion->ArbolN (Int,Int) Char Direccion -> ArbolN (Int,Int) Char Direccion
insertarArbol (a,b) valor dir Fin = Nodo (a,b) valor dir [] 
insertarArbol (a,b) valor dir (Nodo cc vr dir2 []) | (vr=='f') = Nodo cc vr dir2 []
insertarArbol (a,b) valor dir (Nodo cc vr dir2 []) | (adyacente (a,b) cc) = Nodo cc vr dir2 [Nodo(a,b) valor dir []]
insertarArbol (a,b) valor dir (Nodo cc vr dir2 []) | (adyacente (a,b) cc)==False = Nodo cc vr dir2 []
insertarArbol (a,b) valor dir (Nodo cc vr dir2 (x:xs)) | (adyacente (a,b) cc && (buscarEnArbol (a,b) (Nodo cc vr dir2 (x:xs)))==False && valor=='f' ) = Nodo cc vr dir2 ([(Nodo (a,b) valor dir [])]++(x:xs))
insertarArbol (a,b) valor dir (Nodo cc vr dir2 (x:xs)) | (adyacente (a,b) cc && (buscarEnArbol (a,b) (Nodo cc vr dir2 (x:xs)))==False ) = Nodo cc vr dir2 ([(Nodo (a,b) valor dir [])]++(x:xs))
insertarArbol (a,b) valor dir (Nodo cc vr dir2 (x:xs)) | (adyacente (a,b) cc && (buscarEnArbol (a,b) (Nodo cc vr dir2 (x:xs)))==True ) = (Nodo cc vr dir2 (x:xs))
insertarArbol (a,b) valor dir (Nodo cc vr dir2 (x:xs)) | (adyacente (a,b) cc)==False = Nodo cc vr dir2 (map (insertarArbol (a,b) valor dir) (x:xs))
-----------------------------------------------------------------------------------


--Cambia un valor en la matriz o laberinto--
agregarElemento :: [[Char]] -> Char -> (Int, Int) -> [[Char]]
agregarElemento m x (r,c) =
  take r m ++
  [take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
  drop (r + 1) m
------------------------------------------


--Da una tupla adyacente segun direccion---------
move :: Direccion -> (Int,Int) -> (Int,Int)
move Norte (x,y) = (x,y-1)
move Este (x,y) = (x+1,y)
move Sur (x,y) = (x,y+1) 
move Oeste (x,y) = (x-1,y)
---------------------------------------------------



----------Es el "caminante" en el laberinto, que verifica elementos adyacentes---------------------------
walker::(Int,Int)->[[Char]]->ArbolN (Int,Int) Char Direccion->ArbolN (Int,Int) Char Direccion
walker (a,b) lab Fin = walker (a,b) lab (insertarArbol (a,b) 'x' Inicio Fin)
walker (a,b) lab (arbol)| ((gps2 lab 0 (move Sur (a,b)))=="0") = walker (a,(b+1)) (agregarElemento lab '1' (a,b+1)) (insertarArbol (a,(b+1)) '0' Este arbol)
                    | ((gps2 lab 0 (move Norte (a,b))=="0")) = walker (a,b-1) (agregarElemento lab '1' (a,b-1)) (insertarArbol (a,b-1) '0' Oeste arbol)
                    | ((gps2 lab 0 (move Este (a,b)))=="0") = walker ((a+1),b) (agregarElemento lab '1' (a+1,b)) (insertarArbol ((a+1),b) '0' Sur arbol)
                    | ((gps2 lab 0 (move Oeste (a,b))=="0")) = walker (a-1,b) (agregarElemento lab '1' (a-1,b)) (insertarArbol (a-1,b) '0' Norte arbol)
                    | ((gps2 lab 0 (move Sur (a,b)))=="f") = walker (gps (lab) 0 'x') (agregarElemento lab '1' (a,b)) (insertarArbol (a,b+1) 'f' Este arbol)
                    | ((gps2 lab 0 (move Norte (a,b)))=="f") = walker (gps (lab) 0 'x') (agregarElemento lab '1' (a,b)) (insertarArbol (a,b-1) 'f' Oeste arbol)
                    | ((gps2 lab 0 (move Este (a,b)))=="f") = walker (gps (lab) 0 'x') (agregarElemento lab '1' (a,b)) (insertarArbol (a+1,b) 'f' Sur arbol)
                    | ((gps2 lab 0 (move Oeste (a,b)))=="f") = walker (gps (lab) 0 'x') (agregarElemento lab '1' (a,b)) (insertarArbol (a-1,b) 'f' Norte arbol)
    | otherwise = arbol
----------------------------------------------------------------------------------------------------------


----------------ENCUENTRA CUALQUIER elemento Y DA LAS CC----------
--gps (lista o laberinto) (cont) (elemento a buscar)
gps::[[Char]]->Int->Char->(Int,Int)
gps [] a z = (0,0)
gps (x:xs) a z = if(recorregps x a 0 z)==(0,0) then gps xs (a+1) z
    else recorregps x a 0 z
--RecorreFila
recorregps::String->Int->Int->Char->(Int,Int)
recorregps "" a b z = (0,0)
recorregps (x:xs) a b z = if(x==z) 
    then (a,b) else recorregps xs a (b+1) z
-----------------------------------------------------------------


--------- dadas cc encontrar el elemento de esa posicion---
--gps2 (lista de char o laberinto) (cont) (cc del elemento)
gps2::[[Char]]->Int->(Int,Int)->String
gps2 [] a (x1,y1) = ""
gps2 (x:xs) a (x1,y1) = if(a==x1)==False then gps2 xs (a+1) (x1,y1)
    else recorregps2 x 0 y1 

recorregps2::String->Int->Int->String
recorregps2 "" b v = ""
recorregps2 (x:xs) b v = if(b==v) then [x]
    else recorregps2 xs (b+1) v
----------------------------------------------------------

--------------------------Fin------------------------