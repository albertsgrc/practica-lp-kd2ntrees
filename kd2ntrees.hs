class Point p where
    sel        :: Int -> p -> Double 
    dim        :: p -> Int
    child      :: p -> p -> [Int] -> Int
    dist       :: p -> p -> Double
    list2Point :: [Double] -> p

newtype Point3d = Point (Double, Double, Double)

instance Point Point3d where
    sel index (Point (x,y,z))
        | index == 1 = x | index == 2 = y | index == 3 = z

    dim _ = 3

    child e1 e2 coords = value2 $ zipWith (>) coordse1 coordse2
        where coordse1 = map (sel' e1) coords
              coordse2 = map (sel' e2) coords
              sel'     = flip sel
              value2   = foldl hornerBin 0
              hornerBin acc b | b = 2*acc + 1 | otherwise = 2*acc

    dist (Point (x,y,z)) (Point (x',y',z')) = 
        sqrt $ (x - x')^2 + (y - y')^2 + (z - z')^2

    list2Point [x,y,z] = Point (x,y,z)

p  = Point (3.1, -1.0, 2.5)
p1 = Point (1.1, -1.5, 2.5)
p2 = Point (2.1, 3.5, 6.5)
p3 = Point (4.1, 2.0, 0.5)
p4 = Point (4.1, -2.0, 2.6)

data Kd2nTree t = Node t [Int] [Kd2nTree t] | Empty
