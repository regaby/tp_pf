module TeselasWang where

{------------------------------------------------
Función    : getTile
Descripcion: Define conjunto de teselas (0=celeste, 1=amarillo)
Parámetros : ninguno
Retorno    : Lista de 15 teselas
--------------------------------------------------}
getTiles = [[north,east,south,west]|north<-[0,1], east<-[0,1], south<-[0,1], west<-[0,1]]

getTiles' = zipWith (\x y -> (x, y)) [0..15] getTiles

{------------------------------------------------
Función    : getATile
Descripcion: Obtiene un tesela dado el indice/peso
Parámetros : Indice/peso de la tesela
Retorno    : Lista de la tesela con sus 4 lados: west south east north | {0,1}
--------------------------------------------------}
getATile :: (Eq t, Num t) => t -> [Integer]
getATile index = subIndex getTiles index

{------------------------------------------------
Función    : subIndex
Descripcion: Obtiene el n elemento de una lista
Parámetros : lista , indice n
Retorno    : n elemento
--------------------------------------------------}
subIndex (x:_) 0 = x
subIndex (_:xs) n = subIndex xs (n-1)

{------------------------------------------------
Función    : getSum
Descripcion: Determina el peso de la tesela
Parámetros : west south east north | {0,1}
Retorno    : peso de la tesela
--------------------------------------------------}
getSum west south east north = west * 8 + south * 4 + east * 2 + north

{------------------------------------------------
Función    : getTileIndex
Descripcion: Idem a getSum
Parámetros : tupla: (west, south, east, north | {0,1})
Retorno    : peso de la tesela
--------------------------------------------------}
getTileIndex [west, south, east, north] = getSum west south east north

{------------------------------------------------
Función    : createTiles
Descripcion: Crea una lista de teselas
Parámetros : index = el peso de la tesela. n = cantidad de tesela a crear
Retorno    : Lista de teselas
--------------------------------------------------}
createTiles index 0 = []
createTiles index n = [getATile index] ++ createTiles index (n-1)

{------------------------------------------------
Función    : solveTiles
Descripcion: ....
Parámetros :
    n m = dimensión la superficie a teselar
    t0 - t15 = cantidad de teselas a cada tipo
    seed = tesela inicial que se posicionará en 0,0
Retorno    : lista con el teselado resuelto
--------------------------------------------------}
solveTiles n m t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 seed
    | n * m > t0 + t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 + t12 + t13 + t14 + t15
        = error "la superficie es mayor que la cantidad de teselas"
    | otherwise = completeSurface n m seed myTiles
      --otherwise = myTiles
    where myTiles = createTiles 0 t0 ++
                  createTiles 1 t1 ++
                  createTiles 2 t2 ++
                  createTiles 3 t3 ++
                  createTiles 4 t4 ++
                  createTiles 5 t5 ++
                  createTiles 6 t6 ++
                  createTiles 7 t7 ++
                  createTiles 8 t8 ++
                  createTiles 9 t9 ++
                  createTiles 10 t10 ++
                  createTiles 11 t11 ++
                  createTiles 12 t12 ++
                  createTiles 13 t13 ++
                  createTiles 14 t14 ++
                  createTiles 15 t15

completeSurface n m seed (myT:myTs) = iterates (n*m) headTile myTiles n m
    where headTile = [getATile seed]
          headTile' = getATile seed
          myTiles = useTile myTiles' headTile'
          myTiles' = (myT:myTs)

iterates n (x:xs) (myT:myTs) row col
    | n == 1 = (x:xs)
    | otherwise =  iterates (n-1) matchTile myTiles row col
    where matchTile = (x:xs) ++ headTile
          headTile = [head theTile]
          headTile' = head theTile
          theTile = getMatches myTiles' (last(x:xs) !! 2) (n-2) row col (x:xs)
          myTiles = useTile myTiles' headTile'
          myTiles' = (myT:myTs)

-- "cruzo" las dos listas
getMatches (myT:myTs) west n row col (x:xs)
    | mod (n + 1) col == 0     = filterList [getMatch'' (myT:myTs) c | c <- matchesN] -- fila n primer casilla
    | n >= ((row * col) - col) = filterList [getMatch'' (myT:myTs) c | c <- matchesW] -- primer fila
    | n < ((row * col) - col)  = filterList [getMatch'' (myT:myTs) c | c <- matchesWN] -- fila n
    where matchesN     = getMatchN  get_top -- north
          matchesW       = getMatchW west -- west
          matchesWN     = getMatchWN west get_top -- west north
          get_top       = ((x:xs) !! (length(x:xs)-col) !! 1)

-- Esta funcion me elimina la sublistas vacias de una lista
filterList [] = [[9,9,9,9]]
--filterList [] = error "no hay solucion"
filterList (x:xs) = if length (x) > 0 then x ++ filterList xs else filterList xs

getMatchWN w n
    | n == 9 = getMatchW w
    | w == 9 = getMatchN n
    | otherwise = filter (\x -> x!!0 == w &&
                                x!!3 == n) getTiles

getMatchW :: Integer -> [[Integer]]
getMatchW w = filter (\x -> x!!0 == w) getTiles

getMatchN n = filter (\x -> x!!3 == n) getTiles

getMatch'' (myT:myTs) [w, s, e, n] = filter (\x ->x!!0 == w &&
                                                  x!!1 == s &&
                                                  x!!2 == e &&
                                                  x!!3 == n) (myT:myTs)

useTile :: Eq a => [[a]] -> [a] -> [[a]]
useTile (x:xs) [w, s, e, n] = take index (x:xs) ++ drop next_index (x:xs)
    where index = getIndex (x:xs) [w, s, e, n]
          next_index = index + 1

getIndex (x:xs) [w, s, e, n] = if getIndex'(x:xs) [w, s, e, n] > 0 then length(x:xs) - getIndex'(x:xs) [w, s, e, n] else -1

getIndex' :: Eq a => [[a]] -> [a] -> Int
getIndex' [] [w, s, e, n] = 0
getIndex' (x:xs) [w, s, e, n]
    | x!!0 == w && x!!1 == s && x!!2 == e && x!!3 == n = length(x:xs)
    | otherwise = getIndex' xs [w, s, e, n]