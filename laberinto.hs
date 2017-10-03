module Laberinto where


-- Muestra las coordenadas del camino    
solucionar::[[Char]]->[(Int,Int)]
solucionar a = movimientos a (buscarInicio a (0,0)) 


-- Segun la posicion retorna el valor que esta tiene
matxy :: [[Char]]->(Int,Int)->Char
matxy a (fila,columna) =  (a!!fila)!!columna


-- Se mueve de izquiera derecha y de arriba hacia abajo 
desplazar:: [[Char]]->(Int,Int)->(Int,Int)
desplazar (a:as) (fila,columna) = if (columna < ((length a)-1)) 
                                then (fila,columna+1) 
                            else if (columna == ((length a)-1)) 
                                then (fila+1,0) 
                            else (fila,columna)


-- Encuentra el inicio del camino 'x'
buscarInicio:: [[Char]]->(Int,Int)->(Int,Int)
buscarInicio a (fila,columna)
    | matxy a (fila,columna) == 'x' = (fila,columna)
    | matxy a (fila,columna) /= 'x' = buscarInicio a (desplazar a (fila,columna))  


-- Movimiento hacia la izquierda    
moverIzq::[[Char]]->(Int,Int)->(Int,Int)
moverIzq (a:as) (fila,columna)
    | columna == 0 = (fila,columna)
    | columna > 0 = (fila,columna-1)       
      

-- Movimiento hacia la derecha
moverDer::[[Char]]->(Int,Int)->(Int,Int)
moverDer (a:as) (fila,columna)
    | columna == ((length a)-1) = (fila,columna)
    | columna < ((length a)-1) = (fila,columna+1)


-- Movimiento hacia arriba
moverArr::[[Char]]->(Int,Int)->(Int,Int)
moverArr (a:as) (fila,columna)
    | fila == 0 = (fila,columna)
    | fila > 0 = (fila-1,columna)



-- Movimiento hacia abajo
moverAbj::[[Char]]->(Int,Int)->(Int,Int)
moverAbj (a:as) (fila,columna)
    | fila == ((length (a:as))-1) = (fila,columna)
    | fila < ((length (a:as))-1) = (fila+1,columna)


-- Secuencia para comprobar posiciones a la izquierda
movimientos::[[Char]]->(Int,Int)->[(Int,Int)]
movimientos a (fila,columna)
    |matxy a (fila,columna) == 'f' = [(fila,columna)]
    |matxy a (moverIzq a (fila,columna)) == '0' || matxy a (moverIzq a (fila,columna)) == 'f' = [moverIzq a (fila,columna)] ++ movimientos a (moverIzq a (fila,columna))
    |matxy a (moverArr a (fila,columna)) == '0' || matxy a (moverArr a (fila,columna)) == 'f' = [moverArr a (fila,columna)] ++ desplazarArriba a (moverArr a (fila,columna))
    |matxy a (moverAbj a (fila,columna)) == '0' || matxy a (moverAbj a (fila,columna)) == 'f' = [moverAbj a (fila,columna)] ++ desplazarAbajo a (moverAbj a (fila,columna))
    |matxy a (moverDer a (fila,columna)) == '0' || matxy a (moverDer a (fila,columna)) == 'f' = [moverDer a (fila,columna)] ++ desplazarDerecha a (moverDer a (fila,columna))
    

-- Secuencia para comprobar posiciones arriba
desplazarArriba::[[Char]]->(Int,Int)->[(Int,Int)]
desplazarArriba a (fila,columna)
    |matxy a (fila,columna) == 'f' = [(fila,columna)]
    |matxy a (moverArr a (fila,columna)) == '0' || matxy a (moverArr a (fila,columna)) == 'f' = [moverArr a (fila,columna)] ++ desplazarArriba a (moverArr a (fila,columna))
    |matxy a (moverDer a (fila,columna)) == '0' || matxy a (moverDer a (fila,columna)) == 'f' = [moverDer a (fila,columna)] ++ desplazarDerecha a (moverDer a (fila,columna))
    |matxy a (moverIzq a (fila,columna)) == '0' || matxy a (moverIzq a (fila,columna)) == 'f' = [moverIzq a (fila,columna)] ++ movimientos a (moverIzq a (fila,columna))
    |matxy a (moverAbj a (fila,columna)) == '0' || matxy a (moverAbj a (fila,columna)) == 'f' = [moverAbj a (fila,columna)] ++ desplazarAbajo a (moverAbj a (fila,columna))
    

-- Secuencia para comprobar posiciones a la derecha
desplazarDerecha::[[Char]]->(Int,Int)->[(Int,Int)]
desplazarDerecha a (fila,columna)
    |matxy a (fila,columna) == 'f' = [(fila,columna)]
    |matxy a (moverDer a (fila,columna)) == '0' || matxy a (moverDer a (fila,columna)) == 'f' = [moverDer a (fila,columna)] ++ desplazarDerecha a (moverDer a (fila,columna))
    |matxy a (moverArr a (fila,columna)) == '0' || matxy a (moverArr a (fila,columna)) == 'f' = [moverArr a (fila,columna)] ++ desplazarArriba a (moverArr a (fila,columna))
    |matxy a (moverAbj a (fila,columna)) == '0' || matxy a (moverAbj a (fila,columna)) == 'f' = [moverAbj a (fila,columna)] ++ desplazarAbajo a (moverAbj a (fila,columna))
    |matxy a (moverIzq a (fila,columna)) == '0' || matxy a (moverIzq a (fila,columna)) == 'f' = [moverIzq a (fila,columna)] ++ movimientos a (moverIzq a (fila,columna))
    
    
-- Secuencia para comprobar posiciones abajo
desplazarAbajo::[[Char]]->(Int,Int)->[(Int,Int)]
desplazarAbajo a (fila,columna)
    |matxy a (fila,columna) == 'f' = [(fila,columna)]
    |matxy a (moverAbj a (fila,columna)) == '0' || matxy a (moverAbj a (fila,columna)) == 'f' = [moverAbj a (fila,columna)] ++ desplazarAbajo a (moverAbj a (fila,columna))
    |matxy a (moverDer a (fila,columna)) == '0' || matxy a (moverDer a (fila,columna)) == 'f' = [moverDer a (fila,columna)] ++ desplazarDerecha a (moverDer a (fila,columna))
    |matxy a (moverIzq a (fila,columna)) == '0' || matxy a (moverIzq a (fila,columna)) == 'f' = [moverIzq a (fila,columna)] ++ movimientos a (moverIzq a (fila,columna))
    |matxy a (moverArr a (fila,columna)) == '0' || matxy a (moverArr a (fila,columna)) == 'f' = [moverArr a (fila,columna)] ++ desplazarArriba a (moverArr a (fila,columna))    
