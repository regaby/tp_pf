module TilesWang where
import Data.List
--d:\facu\doctorado\materias\04pf\practica
-- celeste = 0
-- amarillo = 1

-- Genera todas las teselas de Wang
getTiles = [[west,south,east,north]|west<-[0,1], south<-[0,1], east<-[0,1], north<-[0,1]]

--Crea n copias de la tesela indicada por su índice
createTiles index 0 = []
createTiles index n = [getATile index] ++ createTiles index (n-1)

--Extrae la cabecera de la lista recursivamente hasta la n-esima
subIndex :: (Eq t, Num t) =>  [p] -> t -> p
subIndex (x:_) 0 = x
subIndex (_:xs) n = subIndex xs (n-1)

--Recupera una tesela de la lista original dado su índice
getATile :: Num a => a -> [Integer]
getATile index = subIndex getTiles index

--
getSum west south east north = west * 8 + south * 4 + east * 2 + north

--Retorna el indice dada un tesela tupla
getTileIndex (west, south, east, north) = getSum west south east north

--Funcion llamadora (en mejora)
--solveTiles n m t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15
--  | nTiles > t0 + t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 + t12 + t13 + t14 + t15
--    = error "la superficie es mayor que la cantidad de teselas"
--  | otherwise 
--  where
--    nTiles = n*m
--    [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15] = t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15
--    tilesType' = foldl () [] [t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15]

solveTiles' n m
--    | n * m > length possibleTiles
      | n * m > length (tilesBag tilesType tilesAmount)
        = error "la superficie es mayor que la cantidad de teselas"
      | otherwise = solutions (n*m) possibleTiles
    where 
--      possibleTiles = (allSolutions allSolutions')
      possibleTiles = (permutaciones_1 allSolutions')

solutions p []      = []
solutions p (x:xs)  = [x | x<-(x:xs),isSolution p x]

-- Convierte los indices de las teselas en teselas
wsenTiles []      = []
wsenTiles (x:xs)  = [getATile x] ++ wsenTiles xs

--isSolution :: Int->[a]->bol
isSolution 1 (x:xs) = True
isSolution p (x:xs) = True && match' a (wsenTiles (x:xs)) && isSolution (p-1) (x:xs)
  where
    ntotal  = length (x:xs)
    a       = (ntotal-p)+1

{-
  Dadas de una a tres teselas determina si la primera empata 
  con las demas
  la segunda es la de la izquierda y
  la tercera es al de arriba
-}
match [] (x:xs) []          = True
match (y:ys) (x:xs) []      = (x:xs)!!0==(y:ys)!!2  --Fila 0
match (y:ys) (x:xs) (z:zs)  = (x:xs)!!0==(y:ys)!!2 && (x:xs)!!3==(z:zs)!!1  --Demás casos
match [] (x:xs) (z:zs)      = (x:xs)!!3==(z:zs)!!1  --Columna 0

match' p (x:xs)
  | div p col == 0 = match ((x:xs)!!(p-1)) ((x:xs)!!p) []       --Primera fila (fila 0)
  | mod p col==0 = match [] ((x:xs)!!p) ((x:xs)!!((p-col))) --Columna 0
  | otherwise = match ((x:xs)!!(p-1)) ((x:xs)!!p) ((x:xs)!!((p-col)))
  where
    ntotal  = length (x:xs)
    fil = truncate (sqrt (fromIntegral ntotal))
    col = truncate (sqrt (fromIntegral ntotal))

--Tipos de teselas seleccionadas "se completa manualmente"
tilesType :: [Int]
--tilesType = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
--tilesType = [13, 0, 1, 0]
--tilesType = [2, 5, 7]
--tilesType = [15, 11, 5, 13]
tilesType = [15, 11, 5, 13, 0]
--tilesType = [13, 8]
--tilesType = [0, 1]

--Cantidad de cada tesela seleccionada "se completa manualmente"
tilesAmount :: [Int]
--tilesAmount = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
--tilesAmount = [4, 4, 4, 4]
--tilesAmount = [3, 3, 1, 2]
tilesAmount = [3, 2, 1, 2, 1]
--tilesAmount = [3, 3, 3]
--tilesAmount = [2, 2]

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
allSolutions' = tilesBag tilesType tilesAmount  --cambiar por allTiles

deleteDuplicate :: (Eq a) => [a] -> [a]
deleteDuplicate [] = []
deleteDuplicate (x:xs) = x : deleteDuplicate (filter (/= x) xs)

deleteDuplicate1 :: Eq a => [a] -> [a]
deleteDuplicate1 xs = [x | x <- nub xs]

permutaciones_1 :: Eq a => [a] -> [[a]]
permutaciones_1 [] = [[]]
permutaciones_1 xs = deleteDuplicate([a:p | a <- xs, p <- permutaciones_1(xs \\ [a])])