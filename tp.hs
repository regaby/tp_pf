module TestTiles where

import GHC.Base (getTag)
getTiles = [[left,bottom,right,top]|left<-[0,1], bottom<-[0,1], right<-[0,1], top<-[0,1]]

createTiles typeof 0 = []
createTiles typeof n = [getATile typeof] ++ createTiles typeof (n-1)

--subIndex :: (Eq t, Num t) => [p] -> t -> p
subIndex (x:_) 0 = x
subIndex (_:xs) n = subIndex xs (n-1)

--getATile :: Int -> (Integer, Integer, Integer, Integer)
getATile t = subIndex getTiles t

getSum left bottom right top = left * 8 + bottom * 4 + right * 2 + top

getATileByNum (left, bottom, right, top) = getSum left bottom right top

-- getLeft (x:xs) left = if subIndex x 0 == left then x else getLeft xs left
-- getTop :: Eq t => [[t]] -> t -> [t]
-- getTop (x:xs) top = if subIndex x 3 == top then x else getTop xs top

-- para position left=0 top=3
-- y parametros left y top ahora es match
getMatch (x:xs) match position = if subIndex x position == match then x else getMatch xs match position

myDrop [] n = []
myDrop xs 0 = xs
myDrop (x:xs) n = myDrop xs (n-1)

useTile (x:xs) typeof = myDrop getTypeofTyle 1
    where getTypeofTyle = subIndex (x:xs) typeof



completeSurface n m seed (x:xs) = useTile

solveTiles n m t0 t1 t2 t3 t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15 seed
    | n * m > t0 + t1 + t2 + t3 + t4 + t5 + t6 + t7 + t8 + t9 + t10 + t11 + t12 + t13 + t14 + t15
        = error "la superficie es mayor que la cantidad de teselas"
    | otherwise = myTiles
        --completeSurface n m seed myTiles
        --useTile myTiles seed
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

--matchTile left bottom right top =
--getNg ng = mod ng 3



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