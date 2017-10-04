{-
    Encontrar camino simple de un laberinto
-}


module Laberinto where 

-- Segun la posicion retorna el valor que esta tiene
matxy :: [[Char]]->(Int,Int)->Char
matxy a (fila,columna) =  (a!!fila)!!columna

-- Se mueve de izquiera derecha y de arriba hacia abajo 
mover:: [[Char]]->(Int,Int)->(Int,Int)
mover (a:as) (fila,columna) = if (columna < ((length a)-1)) 
                                then (fila,columna+1) 
                            else if (columna == ((length a)-1)) 
                                then (fila+1,0) 
                            else (fila,columna)

-- Encuentra el inicio del camino 'x'
encontrarInicio:: [[Char]]->(Int,Int)->(Int,Int)
encontrarInicio a (fila,columna)
    | matxy a (fila,columna) == 'x' = (fila,columna)
    | matxy a (fila,columna) /= 'x' = encontrarInicio a (mover a (fila,columna))  

-- Movimiento hacia la izquierda    
movIzq::[[Char]]->(Int,Int)->(Int,Int)
movIzq (a:as) (fila,columna)
    | columna == 0 = (fila,columna)
    | columna > 0 = (fila,columna-1)       
             
-- Movimiento hacia la derecha
movDer::[[Char]]->(Int,Int)->(Int,Int)
movDer (a:as) (fila,columna)
    | columna == ((length a)-1) = (fila,columna)
    | columna < ((length a)-1) = (fila,columna+1)

-- Movimiento hacia arriba
movArr::[[Char]]->(Int,Int)->(Int,Int)
movArr (a:as) (fila,columna)
    | fila == 0 = (fila,columna)
    | fila > 0 = (fila-1,columna)


-- Movimiento hacia abajo
movAbj::[[Char]]->(Int,Int)->(Int,Int)
movAbj (a:as) (fila,columna)
    | fila == ((length (a:as))-1) = (fila,columna)
    | fila < ((length (a:as))-1) = (fila+1,columna)

-- Secuencia para comprobar posiciones a la izquierda
movimientos::[[Char]]->(Int,Int)->[(Int,Int)]
movimientos a (fila,columna)
    |matxy a (fila,columna) == 'f' = [(fila,columna)]
    |matxy a (movIzq a (fila,columna)) == '0' || matxy a (movIzq a (fila,columna)) == 'f' = [movIzq a (fila,columna)] ++ movimientos a (movIzq a (fila,columna))
    |matxy a (movArr a (fila,columna)) == '0' || matxy a (movArr a (fila,columna)) == 'f' = [movArr a (fila,columna)] ++ movimientosArr a (movArr a (fila,columna))
    |matxy a (movAbj a (fila,columna)) == '0' || matxy a (movAbj a (fila,columna)) == 'f' = [movAbj a (fila,columna)] ++ movimientosAbj a (movAbj a (fila,columna))
    |matxy a (movDer a (fila,columna)) == '0' || matxy a (movDer a (fila,columna)) == 'f' = [movDer a (fila,columna)] ++ movimientosDer a (movDer a (fila,columna))
    
-- Secuencia para comprobar posiciones arriba
movimientosArr::[[Char]]->(Int,Int)->[(Int,Int)]
movimientosArr a (fila,columna)
    |matxy a (fila,columna) == 'f' = [(fila,columna)]
    |matxy a (movArr a (fila,columna)) == '0' || matxy a (movArr a (fila,columna)) == 'f' = [movArr a (fila,columna)] ++ movimientosArr a (movArr a (fila,columna))
    |matxy a (movDer a (fila,columna)) == '0' || matxy a (movDer a (fila,columna)) == 'f' = [movDer a (fila,columna)] ++ movimientosDer a (movDer a (fila,columna))
    |matxy a (movIzq a (fila,columna)) == '0' || matxy a (movIzq a (fila,columna)) == 'f' = [movIzq a (fila,columna)] ++ movimientos a (movIzq a (fila,columna))
    |matxy a (movAbj a (fila,columna)) == '0' || matxy a (movAbj a (fila,columna)) == 'f' = [movAbj a (fila,columna)] ++ movimientosAbj a (movAbj a (fila,columna))
    
-- Secuencia para comprobar posiciones a la derecha
movimientosDer::[[Char]]->(Int,Int)->[(Int,Int)]
movimientosDer a (fila,columna)
    |matxy a (fila,columna) == 'f' = [(fila,columna)]
    |matxy a (movDer a (fila,columna)) == '0' || matxy a (movDer a (fila,columna)) == 'f' = [movDer a (fila,columna)] ++ movimientosDer a (movDer a (fila,columna))
    |matxy a (movArr a (fila,columna)) == '0' || matxy a (movArr a (fila,columna)) == 'f' = [movArr a (fila,columna)] ++ movimientosArr a (movArr a (fila,columna))
    |matxy a (movAbj a (fila,columna)) == '0' || matxy a (movAbj a (fila,columna)) == 'f' = [movAbj a (fila,columna)] ++ movimientosAbj a (movAbj a (fila,columna))
    |matxy a (movIzq a (fila,columna)) == '0' || matxy a (movIzq a (fila,columna)) == 'f' = [movIzq a (fila,columna)] ++ movimientos a (movIzq a (fila,columna))
    
    
-- Secuencia para comprobar posiciones abajo
movimientosAbj::[[Char]]->(Int,Int)->[(Int,Int)]
movimientosAbj a (fila,columna)
    |matxy a (fila,columna) == 'f' = [(fila,columna)]
    |matxy a (movAbj a (fila,columna)) == '0' || matxy a (movAbj a (fila,columna)) == 'f' = [movAbj a (fila,columna)] ++ movimientosAbj a (movAbj a (fila,columna))
    |matxy a (movDer a (fila,columna)) == '0' || matxy a (movDer a (fila,columna)) == 'f' = [movDer a (fila,columna)] ++ movimientosDer a (movDer a (fila,columna))
    |matxy a (movIzq a (fila,columna)) == '0' || matxy a (movIzq a (fila,columna)) == 'f' = [movIzq a (fila,columna)] ++ movimientos a (movIzq a (fila,columna))
    |matxy a (movArr a (fila,columna)) == '0' || matxy a (movArr a (fila,columna)) == 'f' = [movArr a (fila,columna)] ++ movimientosArr a (movArr a (fila,columna))    

-- Muestra las coordenadas del camino    
encontrarCamino::[[Char]]->[(Int,Int)]
encontrarCamino a = movimientos a (encontrarInicio a (0,0)) 