module Kd2nTree where

class Point p where
    sel        :: Int -> p -> Double 
    dim        :: p -> Int
    child      :: p -> p -> [Int] -> Int
    child e1 e2 coords = value2 $ zipWith (>) coordse1 coordse2
        where (coordse1, coordse2) = (map (sel' e1) coords, map (sel' e2) coords)
              sel'     = flip sel
              value2   = foldl hornerBin 0
              hornerBin acc b | b = 2*acc + 1 | otherwise = 2*acc

    dist :: p -> p -> Double
    dist = sqrt .: dist2 where (.:) = (.).(.)
    
    list2Point :: [Double] -> p
    ptrans     :: [Double] -> p -> p
    ptrans l = list2Point . zipWith (+) l . coordinates

    pscale     :: Double -> p -> p
    pscale x = list2Point . map (* x)     . coordinates

coordinates :: Point p => p -> [Double]
coordinates p = let dimensions = dim p in [ sel i p | i <- [1..dimensions] ]
    
dist2 :: Point p => p -> p -> Double
dist2 p p' = sum [ (c - c')^2 | (c, c') <- coordinates p `zip` coordinates p' ]

newtype Point3d = Point3d (Double, Double, Double) deriving (Eq, Ord)

instance Point Point3d where
    sel index (Point3d (x,y,z)) | index == 1 = x | index == 2 = y | index == 3 = z
    
    dim _ = 3
    
    list2Point [x,y,z] = Point3d (x,y,z)

instance Show Point3d where show (Point3d p) = show p

data Kd2nTree t = Node t [Int] [Kd2nTree t] | Empty 

instance (Point p, Eq p) => Eq (Kd2nTree p) where  
    t1 == t2 = lt1 == lt2 && all (contains t2) elemst1
        where (elemst1, elemst2) = (elems t1, elems t2); (lt1, lt2) = (length elemst1, length elemst2)

instance (Show t) => Show (Kd2nTree t) where
    show Empty = ""
    show (Node elem distr childs) = showNode elem distr ++ showChilds "" childs
        where showNode elem distr      = show elem ++ " " ++ show distr
              showChilds indent childs = concatMap (show' indent) (map show [0..] `zip` childs)
              show' _ (_,Empty)        = ""
              show' indent (childIndex, (Node elem distr childs)) = 
                  "\n " ++ indent ++ "<" ++ childIndex ++ ">" ++ showNode elem distr ++ 
                  showChilds ("    " ++ indent) childs

insert :: (Point p, Eq p) => Kd2nTree p -> p -> [Int] -> Kd2nTree p
insert Empty point distr = Node point distr []
insert t@(Node point distr childs) point' pdistr | point == point' = t | otherwise = Node point distr insert' 
    where childIndex = child point' point distr
          ldiff      = childIndex - (length childs)
          insert' | ldiff >= 0 = childs ++ (take ldiff (repeat Empty)) ++ [Node point' pdistr []]
                  | otherwise  = left ++ insert subtree point' pdistr : right
                  where (left, subtree:right) = splitAt childIndex childs

build :: (Point p, Eq p) => [(p,[Int])] -> Kd2nTree p
build list = foldl (\tree (point,distr) -> insert tree point distr) Empty list

buildIni :: (Point p, Eq p) => [([Double],[Int])] -> Kd2nTree p
buildIni list = build [ (list2Point pointl, distr) | (pointl,distr) <- list ]

get_all ::  Kd2nTree p -> [(p,[Int])]
get_all Empty = []
get_all (Node elem distr childs) = (elem, distr) : (concatMap get_all childs)

elems :: Kd2nTree p -> [p]
elems tree = [ elem | (elem,_) <- get_all tree ]

remove :: (Point p, Eq p) => Kd2nTree p -> p -> Kd2nTree p
remove Empty _ = Empty
remove t@(Node point distr childs) point'
    | point == point'               = build $ tail $ get_all t
    | childIndex >= (length childs) = t
    | otherwise                     = Node point distr (left ++ remove subtree point' : right)
    where childIndex = child point' point distr
          (left, subtree:right) = splitAt childIndex childs 

contains :: (Point p, Eq p) => Kd2nTree p -> p -> Bool
contains Empty _ = False
contains (Node point distr childs) point'
    | point == point'               = True
    | childIndex >= (length childs) = False 
    | otherwise                     = childs !! childIndex `contains` point'
    where childIndex = child point' point distr

nearest :: Point p => Kd2nTree p -> p -> p
nearest t p = fst $ argvalmin (dist2 p) (elems t)
    where argvalmin f (x:xs) = foldl (minDecider f) (x, f x) xs
          minDecider f m@(amin, vmin) anew
            | vmin < vnew = m | otherwise = (anew, vnew) where vnew = f anew

allinInterval :: Ord p => Kd2nTree p -> p -> p -> [p]
allinInterval t pl pr = qsort [ p | p <- elems t, pl <= p, p <= pr ]
    where qsort [] = []
          qsort (x:xs) = (qsort lh) ++ [x] ++ (qsort rh)
               where lh = [ y | y <- xs, y <  x ]
                     rh = [ y | y <- xs, y >= x ]

kdmap :: (p -> q) -> Kd2nTree p -> Kd2nTree q
kdmap _ Empty = Empty
kdmap f (Node elem distr childs) = Node (f elem) distr [ kdmap f s | s <- childs ]

translation :: Point p => [Double] -> Kd2nTree p -> Kd2nTree p
translation = kdmap . ptrans

scale :: Point p => Double -> Kd2nTree p -> Kd2nTree p
scale = kdmap . pscale
