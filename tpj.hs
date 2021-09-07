module TestTiles where
--d:\facu\doctorado\materias\04pf\practica
-- celeste = 0
-- amarillo = 1
--data Lado = West Int | South Int | East Int | North Int deriving (Show)
--data Tile = Wang West: South: East: North deriving (Show)
--data south = 0 | 1
--data east = 0 | 1
--data north = 0 | 1
--data lado = west int | south int | east int | north int
--data Tile = Tile [Integer, Integer, Integer, Integer]

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

--placeTile n m a 
--  0 0 _ = []
--  _ _ a = [a]

--  getATile 0

tilesList = [[0,0,0,0],[0,0,0,1],[0,0,0,0],[0,0,0,0]]

completeSurface n m seed (x:xs) = useTile

solveTiles n m t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 seed
    | n * m > t0 + t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 + t12 + t13 + t14 + t15
        = error "la superficie es mayor que la cantidad de teselas"
    | otherwise = myTiles
    where myTiles = [createTiles 0 t0] ++
                  [createTiles 1 t1] ++
                  [createTiles 2 t2] ++
                  [createTiles 3 t3] ++
                  [createTiles 4 t4] ++
                  [createTiles 5 t5] ++
                  [createTiles 6 t6] ++
                  [createTiles 7 t7] ++
                  [createTiles 8 t8] ++
                  [createTiles 9 t9] ++
                  [createTiles 10 t10] ++
                  [createTiles 11 t11] ++
                  [createTiles 12 t12] ++
                  [createTiles 13 t13] ++
                  [createTiles 14 t14] ++
                  [createTiles 15 t15]



-- - hacer una lista con todos los tiles de 1 a 16 instanciados
-- - iniciar superficie
-- - ir iterando lista y comprobando si el tile coincide con los adyacentes

initSurface x y = []

--matchTile west south east north =
--getNg ng = mod ng 3

--Typos de teselas seleccionadas
tilesType :: [Int]
tilesType = [0, 1]
--Cantidad de cada tesela seleccionada
tilesAmount :: [Int]
tilesAmount = [2, 2]

--poblar (x:xs) (y:ys) = take y (repeat x)
--poblar (x:xs) (y:ys) = [take y (repeat x) | x <- [1..10]]
--Bolsa de teselas
tilesBag [] [] = []
tilesBag (x:xs) [] = []
tilesBag [] (y:ys) = []
tilesBag (x:xs) (y:ys) = (replicate y x) ++ tilesBag xs ys
--  | xs ys = [x | x <- xs, y <- ys, replicate y x ]
-- | (x:xs) (y:ys) = [(x : ) | x <- (x:xs)] ys

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