import qualified Data.List
import Kd2nTree

p1 :: Point3d
p1 = list2Point [3.0,-1.0,2.1]
p2 :: Point3d
p2 = list2Point [3.5,2.8,3.1]
p3 :: Point3d
p3 = list2Point [3.5,0.0,2.1]
p4 :: Point3d
p4 = list2Point [3.0,-1.7,3.1]
p5 :: Point3d
p5 = list2Point [3.0,5.1,0.0]
p6 :: Point3d
p6 = list2Point [1.5,8.0,1.5]
p7 :: Point3d
p7 = list2Point [3.3,2.8,2.5]
p8 :: Point3d
p8 = list2Point [4.0,5.1,3.8]
p9 :: Point3d
p9 = list2Point [3.1,3.8,4.8]
p10 :: Point3d
p10 = list2Point [1.8,1.1,-2.0]
p11 :: Point3d
p11 = list2Point [-100000, 0.0, 100000]

enunciat :: Kd2nTree Point3d
enunciat = buildIni [
    ([3.0, -1.0, 2.1], [1, 3]), 
    ([3.5, 2.8, 3.1], [1, 2]), 
    ([3.5, 0.0, 2.1], [3]),
    ([3.0, -1.7, 3.1], [1, 2, 3]), 
    ([3.0, 5.1, 0.0], [2]), 
    ([1.5, 8.0, 1.5], [1]), 
    ([3.3, 2.8, 2.5], [3]), 
    ([4.0, 5.1, 3.8], [2]), 
    ([3.1, 3.8, 4.8], [1, 3]), 
    ([1.8, 1.1, -2.0], [1, 2])]

pau :: Kd2nTree Point3d
pau = buildIni [
    ([0,0,0], [1]),
    ([-1,1,0], [1]),
    ([2,200,0],[1]),
    ([1,2,0], [1]),
    ([1,20,0], [1]),
    ([30,-20,0], [1])]

empty :: Kd2nTree Point3d
empty = buildIni []

equivalent1 :: Kd2nTree Point3d
equivalent1 = buildIni [
                       ([2,3,-100.5], [2,3]),
                       ([5,0,-10],    [1]),
                       ([-2,-2,-2],   [3])
                       ]

equivalent2 :: Kd2nTree Point3d
equivalent2 = buildIni [
                       ([5,0,-10.0], [2,3]),
                       ([-2,-2,-2], [1]),
                       ([2,3,-100.5], [2])
                       ]

antiequivalent :: Kd2nTree Point3d
antiequivalent = buildIni [
                       ([5,0,-10.00001], [2,3]),
                       ([-2,-2,-2], [1]),
                       ([2,3,-100.5], [2])
                       ]

punts :: [Point3d]
punts = [p1, p2, p3, p4, p5, p6, p11, pscale 2 p11, pscale (-2) (pscale (-1) p11), nearest pau p11, nearest enunciat p11, nearest equivalent1 p11]

arbres :: [Kd2nTree Point3d]
arbres = [pau, enunciat, antiequivalent, equivalent1, equivalent2, empty]

llistes :: [[Point3d]]
llistes =  [allinInterval equivalent1 p6 p8, allinInterval equivalent1 p8 p6, allinInterval enunciat (list2Point [-500, -500, -500]) (list2Point [500, 500, 500])]

strings :: [String]
strings = [show $ dist p1 p2, show $ allinInterval enunciat (list2Point [-500, -500, -500]) (list2Point [500,500,500])]

proves :: [([Char], Bool)]
proves = [
         ("Igualtat punts", p1 == p1),
         ("Desigualtat punts", p1 /= p2),
         ("Dimensio punts", dim p1 == 3),
         ("Seleccio punts", sel 1 p1 == 3),
         ("Translacio punts", ptrans [1,1,1] p1 == list2Point [4,0,3.1]),
         ("Distancia punts", dist p1 p1 == 0 && dist2 p1 p1 == 0),

         ("Comprovacio empty", empty == Empty),
         ("Igualtat d'empty", empty == empty),
         ("Desigualtat empty/no empty", empty /= enunciat && enunciat /= empty),
         ("Igualtat no empty", enunciat == enunciat),

         ("Equivalencia certa directa", equivalent1 == equivalent2),
         ("Equivalencia certa insert", (insert equivalent1 p11 [1]) == (insert equivalent2 p11 [2,3])),
         ("Equivalencia certa elems", elems equivalent1 `eqList` elems equivalent2),
         ("Equivalencia certa translation", (translation [500,0,-1] equivalent1) == (translation [500,0,-1] equivalent2)),
         ("Equivalencia certa scale", (elems $ scale (-5.33) equivalent1) `eqList` (elems $ scale (-5.33) equivalent2)),
         ("Equivalencia certa scale 2", (scale (200) equivalent1) == (scale 200 equivalent2)),
         ("Equivalencia certa allInInterval", (allinInterval equivalent1 p6 p8) `eqList` (allinInterval equivalent2 p6 p8)),

         ("Equivalencia falsa directa", equivalent1 /= antiequivalent && equivalent2 /= antiequivalent),  
         ("Equivalencia falsa insert", (insert equivalent1 p1 [2,3]) /= (insert equivalent2 p11 [2,3])),
         ("Equivalencia falsa elems", elems equivalent1 `difList` elems antiequivalent),
         ("Equivalencia falsa translation", (translation [500,0,1] equivalent1) /= (translation [500,0,-1] equivalent2)),
         ("Equivalencia falsa scale", (elems $ scale (-5.23) equivalent1) `difList` (elems $ scale (-5.33) equivalent2)),
         ("Equivalencia falsa scale 2", (scale (100) equivalent1) /= (scale 200 equivalent2)),
         
         ("Operacions sobre empty's 1", (insert Empty p1 [2,3]) == (insert Empty p1 [1,2])),
         ("Operacions sobre empty's 2", (get_all Empty) == (get_all $ foldl remove enunciat (elems enunciat))),
         ("Operacions sobre empty's 3", (remove Empty p1) == empty),
         ("Operacions sobre empty's 4", not $ (any (== True)) (map (contains Empty) (elems enunciat))),
         ("Operacions sobre empty's 5", null $ allinInterval empty (list2Point [-500,-500,-500]) (list2Point [500,500,500])),
         ("Operacions sobre empty's 6", translation [-5,0,-5] Empty == empty),
         ("Operacions sobre empty's 7", scale (-10.5) Empty == empty),
         
         ("Propietat conjunts1", insert enunciat p1 [2,3] == enunciat),
         ("Propietat conjunts2", insert enunciat p2 [2,3] == enunciat),
         ("Propietat conjunts3", insert enunciat p3 [1] == enunciat),
         ("Propietat conjunts4", insert enunciat p11 [1] /= enunciat),
         ("Propietat conjunts5", foldl (\e (x,y) -> insert e x y)  enunciat (get_all enunciat) == enunciat),
         ("Propietat conjunts6", foldl (\e (x,y) -> insert e x y)  equivalent1 (get_all equivalent1) == equivalent2)
         ]

eqList l1 l2 = (length l1 == length l2) && (length l1 == length (l1 `Data.List.intersect` l2))
difList = not .: eqList where (.:) = (.).(.)

printTests _ [] = do putStrLn "Fi de proves\n----------"
printTests s l  = do putStrLn "----------"; printTests' s l 1
    where 
        printTests' _ [] _ = do putStrLn "Fi de proves\n----------"
        printTests' s (x:xs) i = do
            putStrLn (s ++ "#" ++ (show i))
            putStrLn (show x)
            putStrLn ""
            printTests' s xs (i + 1)

main = do 
    printTests "Punt " punts
    printTests "Arbre " arbres
    printTests "Llista " llistes
    printTests "Prova " proves
        
    if (and (map snd proves)) then do putStrLn "Proves OK!" else do putStrLn "Proves NOT ok!"

    mapM_ putStrLn [string ++ "\n" | string <- strings]
