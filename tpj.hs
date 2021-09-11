module TestTiles where
--d:\facu\doctorado\materias\04pf\practica
-- celeste = 0
-- amarillo = 1

-- Genera todas las teselas de Wang
getTiles = [[west,south,east,north]|west<-[0,1], south<-[0,1], east<-[0,1], north<-[0,1]]
--Crea n copias de la tesela indicada por su índice
createTiles index 0 = []
createTiles index n = [getATile index] ++ createTiles index (n-1)

--Extrae la cabecera de la lista recursivamente hasta la n-esima
--subIndex :: (Eq t, Num t) =>  [p] -> t -> p
subIndex (x:_) 0 = x
subIndex (_:xs) n = subIndex xs (n-1)

--Recupera una tesela de la lista original dado su índice
--getATile :: Int -> (Integer, Integer, Integer, Integer)
getATile :: Num a => a -> [Integer]
getATile index = subIndex getTiles index

--
getSum west south east north = west * 8 + south * 4 + east * 2 + north
--getSum north east south west = north * 8 + east * 4 + south * 2 + west
--Retorna el indice dada un tesela tupla
getTileIndex (west, south, east, north) = getSum west south east north
--getTileIndex (north, east, south, west) = getSum north east south west

-- getWest (x:xs) west = if subIndex x 0 == west then x else getWest xs west
-- getNorth :: Eq t => [[t]] -> t -> [t]
-- getNorth (x:xs) north = if subIndex x 3 == north then x else getNorth xs north

-- para position west=0 north=3
-- y parametros west y north ahora es match
getMatch (x:xs) match position = if subIndex x position == match then x else getMatch xs match position

myDrop [] n = []
myDrop xs 0 = xs
myDrop (x:xs) n = myDrop xs (n-1)

useTile (x:xs) typeof = myDrop getTypeofTyle 1
    where getTypeofTyle = subIndex (x:xs) typeof

-- Requiere de la cantidad de filas, columnas, el plano, y la lista de teselas
--placeTile fil col [] b
--    | fil == 0 && col == 0 = fil col [getATile 0]

--tilesList = [[0,0,0,0],[0,0,0,0],[0,0,0,1],[0,0,0,1]]

completeSurface n m seed (x:xs) = useTile


solveTiles' n m
    | n * m > length possibleTiles
        = error "la superficie es mayor que la cantidad de teselas"
    | otherwise = possibleTiles
    where 
      possibleTiles = allSolutions allSolutions'

-- - hacer una lista con todos los tiles de 1 a 16 instanciados
-- - iniciar superficie
-- - ir iterando lista y comprobando si el tile coincide con los adyacentes
--  11 y 12 wsenTiles ((solveTiles' 2 2)!!11)
-- Convierte los indices de las teselas en teselas
wsenTiles []      = []
wsenTiles (x:xs)  = [getATile x] ++ wsenTiles xs

--isSolution :: Int->[a]->bol
--isSolution p (x:xs) = True && match ((x:xs)!!a) [] [] && isSolution p-1 (x:xs)
isSolution 1 (x:xs) = True
isSolution p (x:xs) = True && match' a (x:xs) && isSolution (p-1) (x:xs)
  where
    ntotal  = length (x:xs)
    a       = (ntotal-p)+1
--    fil = truncate (sqrt (fromIntegral ntotal))
--    col = truncate (sqrt (fromIntegral ntotal))
--    fil==0 || col==0  = 
--    sol = True && take index (x:xs) ++ drop next_index (x:xs)

initSurface x y = []

{-
  Dadas de una a tres teselas determina si la primera empata 
  con las demas
  la segunda es la de la izquierda y
  la tercera es al de arriba
-}
match [] (x:xs) []          = True
match (y:ys) (x:xs) []      = (x:xs)!!0==(y:ys)!!2
match (y:ys) (x:xs) (z:zs)  = (x:xs)!!0==(y:ys)!!2 && (x:xs)!!3==(z:zs)!!1
match [] (x:xs) (z:zs)      = (x:xs)!!3==(z:zs)!!1

match' p (x:xs)
  | div p col == 0 = match ((x:xs)!!(p-1)) ((x:xs)!!p) []       --Primera fila (fila 0)
  | mod p col==0 = match [] ((x:xs)!!p) ((x:xs)!!((p-col))) --Columna 0
--  | mod p col>0 && div p col > 0 = match ((x:xs)!!(p-1)) ((x:xs)!!p) ((x:xs)!!((p-col)))  --
  | otherwise = match ((x:xs)!!(p-1)) ((x:xs)!!p) ((x:xs)!!((p-col)))
--  | otherwise = (x:xs)!!p==[1,0,0,0]
  where
    ntotal  = length (x:xs)
    fil = truncate (sqrt (fromIntegral ntotal))
    col = truncate (sqrt (fromIntegral ntotal))

--Tipos de teselas seleccionadas "se completa manualmente"
tilesType :: [Int]
tilesType = [0, 1]
--Cantidad de cada tesela seleccionada "se completa manualmente"
tilesAmount :: [Int]
tilesAmount = [2, 2]

--Bolsa de teselas
tilesBag :: [a] -> [Int] -> [a]
tilesBag [] [] = []
tilesBag (x:xs) [] = []
tilesBag [] (y:ys) = []
tilesBag (x:xs) (y:ys) = (replicate y x) ++ tilesBag xs ys


--Todas las soluciones "allSolutions (tilesBag tilesType tilesAmount)"
allSolutions :: [a] -> [[a]]
allSolutions [] = [[]]
allSolutions (x:xs) = [zs | ys <- allSolutions xs,
                            zs <- intercala x ys]
intercala :: a -> [a] -> [[a]]
intercala e [] = [[e]]
intercala e (x:xs) = (e:x:xs) : [(x:ys) | ys <- (intercala e xs)]

allSolutions' :: [Int]
allSolutions' = tilesBag tilesType tilesAmount

--  En desarrollo
tomar :: Num a => [a] -> [[Integer]]
tomar [] = []
tomar (x:xs) = [getATile x] ++ tomar xs

llenar n m (x:xs)
    | length (x:xs) < n*m = error "Las teselas no son suficientes para cubrir este plano" 
    | otherwise = [[y | y<-(getATile x)]]++aux xs
       where
            aux [] = []
            aux (x:xs) = [[z | z<-(getATile x)]] ++ aux xs
            n_1 = x
 --           n_m = z
            pos = length xs
            col = mod pos m
            fil = div pos m 
--            n_m = if pos<=m then [] 
--                          else z 


posición_1 :: Eq a => a -> [a] -> Int
posición_1 x ys =
  if elem x ys then aux x ys
  else 0
    where 
      aux x [] = 0
      aux x (y:ys)
        | x== y     = 1
        | otherwise = 1 + aux x ys

--     [[0,0,0,0],
--      [1,0,0,0],
--      [0,1,0,0],
--      [1,1,0,0],
--      [0,0,1,0],
--      [1,0,1,0],
--      [0,1,1,0],
--      [1,1,1,0],
--      [0,0,0,1],
--      [1,0,0,1],
--      [0,1,0,1],
--      [1,1,0,1],
--      [0,1,0,1],
--      [1,0,1,1],
--      [0,1,1,1],
--      [1,1,1,1],
-- ]
-- datos:
-- - superficie n x n
-- - cantidad de teselas de cada tipo: t0 a t16
-- coincide arriba? 0 o 1
-- coincide a la derecha? 0 o 1
-- coincide abajo? 0 o 1
-- coincide a izquierda? 0 o 1

-- sup 2x2
-- iniciar con un tile (random?) en 0,0
-- 0000
-- coincide a la derecha? con 0