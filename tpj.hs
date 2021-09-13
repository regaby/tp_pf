module TestTiles where
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
--getSum north east south west = north * 8 + east * 4 + south * 2 + west
--Retorna el indice dada un tesela tupla
getTileIndex (west, south, east, north) = getSum west south east north

--useTile (x:xs) typeof = myDrop getTypeofTyle 1
--    where getTypeofTyle = subIndex (x:xs) typeof

-- Requiere de la cantidad de filas, columnas, el plano, y la lista de teselas
--placeTile fil col [] b
--    | fil == 0 && col == 0 = fil col [getATile 0]

--tilesList = [[0,0,0,0],[0,0,0,0],[0,0,0,1],[0,0,0,1]]

--completeSurface n m seed (x:xs) = useTile

--Funcion llamadora
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
--    | otherwise = possibleTiles
    | otherwise = solutions (n*m) possibleTiles
    where 
--      possibleTiles = (allSolutions allSolutions')
      possibleTiles = (permutaciones_1 allSolutions')

solutions p []      = []
solutions p (x:xs)  = [x | x<-(x:xs),isSolution p x]
-- - hacer una lista con todos los tiles de 1 a 16 instanciados
-- - iniciar superficie
-- - ir iterando lista y comprobando si el tile coincide con los adyacentes
--  11 y 12 wsenTiles ((solveTiles' 2 2)!!11)

-- Convierte los indices de las teselas en teselas
wsenTiles []      = []
wsenTiles (x:xs)  = [getATile x] ++ wsenTiles xs

--isSolution :: Int->[a]->bol
--isSolution p (x:xs) = True && match ((x:xs)!!a) [] [] && isSolution p-1 (x:xs)
--isSolution p (x:xs) = True && match' a (x:xs) && isSolution (p-1) (x:xs)
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

--  | mod p col>0 && div p col > 0 = match ((x:xs)!!(p-1)) ((x:xs)!!p) ((x:xs)!!((p-col)))  --
--  | otherwise = (x:xs)!!p==[1,0,0,0]

--Tipos de teselas seleccionadas "se completa manualmente"
tilesType :: [Int]
--tilesType = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]
--tilesType = [13, 0, 1, 0]
--tilesType = [15, 11, 5]
--tilesType = [15, 11, 5, 13]
tilesType = [13, 2]
--tilesType = [0, 1]

--Cantidad de cada tesela seleccionada "se completa manualmente"
tilesAmount :: [Int]
--tilesAmount = [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1]
--tilesAmount = [4, 4, 4, 4]
--tilesAmount = [3, 3, 1, 2]
--tilesAmount = [3, 3, 3]
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
    
--intercala :: a -> [a] -> [[a]]
--intercala e [] = [[e]]
--intercala e (x:xs) = sol
--  where
--    sol = (e:x:xs) : [(x:ys) | ys <- (intercala e xs)]
--    sol = if (match ((wsenTiles[e])!!0) ((wsenTiles[x])!!0) []) then ((e:x:xs) : [(x:ys) | ys <- (intercala e xs)]) else []
--if sol==True then ((e:x:xs) : [(x:ys) | ys <- (intercala e xs)]) else []
--match (y:ys) (x:xs) []
--wsenTiles (x:xs)

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

--combinaciones :: Int -> [a] -> [[a]]
--combinaciones n xs = [ys | ys <- sublistas xs, length ys == n]


--combinaciones :: Integer -> [a] -> [[a]]
--combinaciones 0 _ = [[]]
--combinaciones _ [] = []
--combinaciones k (x:xs) = [x:ys | ys <- combinaciones (k-1) xs] ++ combinaciones k xs

--comb n k = (factorial n) `div` ((factorial k) * (factorial (n-k)))

--factoriales_3 :: Integer -> [Integer]
--factoriales_3 n = [factorial x | x <- [0..n]]

--combinaciones_1 :: Int -> [a] -> [[a]]
--combinaciones_1 n xs = [ys | ys <- sublistas xs, length ys == n]

--factorial :: (Integral a) => a -> a
--factorial 0 = 1
--factorial n = n * factorial (n - 1)

--sinRepetidos :: Eq a => [a] -> Bool
--sinRepetidos xs = nub xs == xs

--permutaciones_1 xs = [a:p | a <- xs, p <- permutaciones_1(xs \\ [a])]

--variaciones :: Int -> [a] -> [[a]]
--variaciones k xs = concatMap permutaciones (combinaciones k xs)

--variacionesR :: Int -> [a] -> [[a]]
--variacionesR 0 _  = [[]] 
--variacionesR k xs = [z:ys | z <- xs, ys <- variacionesR (k-1) xs]

--variacionesR :: Int -> [a] -> [[a]]
--variacionesR _ [] = [[]]
--variacionesR 0 _ = [[]]
--variacionesR k us = [u:vs | u <- us, vs <- variacionesR (k-1) us]

--variacionesR :: Integer -> [a] -> [[a]]
--variacionesR _ [] = [[]]
--variacionesR 0 _  = [[]] 
--variacionesR k xs = [z:ys | z <- xs, ys <- variacionesR (k-1) xs]

--variacionesRN :: Integer -> Integer -> [[Integer]]    
--variacionesRN n k = variacionesR k [1..n]

--igual_conjunto_2 :: Eq a => [a] -> [a] -> Bool
--igual_conjunto_2 xs ys = aux (nub xs) (nub ys)
--  where aux [] [] = True
--        aux (x:_) [] = False
--        aux [] (y:_) = False
--        aux (x:xs) ys = x `elem` ys && aux xs (delete x ys)

--igual_conjunto_3 :: (Eq a, Ord a) => [a] -> [a] -> Bool
--igual_conjunto_3 xs ys = sort (nub xs) == sort (nub ys)


--sublistas :: [a] -> [[a]]
--sublistas [] = [[]]
--sublistas (x:xs) = [x:ys | ys <- sub] ++ sub
--  where sub = sublistas xs

--  En desarrollo
--tomar :: Num a => [a] -> [[Integer]]
--tomar [] = []
--tomar (x:xs) = [getATile x] ++ tomar xs

--llenar n m (x:xs)
--    | length (x:xs) < n*m = error "Las teselas no son suficientes para cubrir este plano" 
---    | otherwise = [[y | y<-(getATile x)]]++aux xs
--       where
--            aux [] = []
--            aux (x:xs) = [[z | z<-(getATile x)]] ++ aux xs
--            n_1 = x
 --           n_m = z
--            pos = length xs
--            col = mod pos m
--            fil = div pos m 
--            n_m = if pos<=m then [] 
--                          else z 


--posición_1 :: Eq a => a -> [a] -> Int
--posición_1 x ys =
--  if elem x ys then aux x ys
--  else 0
--    where 
--      aux x [] = 0
--      aux x (y:ys)
--       | x== y     = 1
--        | otherwise = 1 + aux x ys

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