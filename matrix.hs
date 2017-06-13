module Matrix (
    Matrix(..)
  , EdgeMatrix
  , PolygonMatrix
  , fromList
  , toList
  , fromEdgelist
  , toEdgelist
  , getRows
  , getCols
  , getRow
  , getCol
  , multMatrix
  , scaleMatrix
  , identMatrix
  , translate
  , dilate
  , rotateX
  , rotateY
  , rotateZ
) where

import Types
import Line
import qualified Data.Vector as V

type EdgeMatrix = [Point3D]
type PolygonMatrix = [Point3D]

data Matrix a = M {
   rows :: Int
  ,cols :: Int
  ,vect :: V.Vector a
  }

instance Functor Matrix where
  fmap f (M r c v) = M r c $ V.map f v

--TODO: Not very pretty. Need to revisit later.
instance Show a => Show (Matrix a) where
  show (M r c v) = concat $ fmap (showSlice c) [i | i<-[0,c..(r-1)*c]]
    where showSlice len a = (++"\n") . showVec $ V.slice a len v
          showVec v = show $ V.toList v

fromList :: Int -> Int -> [a] -> Matrix a
fromList r c = M r c . V.fromListN (r*c)

toList :: Matrix a -> [a]
toList (M r c v) = V.toList v

{-
fromEdgelist :: [Point3D] -> Matrix Double
fromEdgelist el = M (length el) 4 vec
  where vec = V.concat $ fmap addPt el
        addPt p@(x,y,z) = V.fromList [fromIntegral x,fromIntegral y,fromIntegral z,1.0]
-}

fromEdgelist :: [Point3D] -> Matrix Double
fromEdgelist el = M 4 (length el) vec
  where vec = V.fromList $ allXs ++ allYs ++ allZs ++ all1s
        allXs = fmap getX el
        allYs = fmap getY el
        allZs = fmap getZ el
        all1s = take (length allXs) $ repeat 1.0
        getX (x,_,_) = x
        getY (_,y,_) = y
        getZ (_,_,z) = z

toEdgelist :: Matrix Double -> [Point3D]
toEdgelist em = fmap pairify listCols
  where listCols = fmap (V.toList . V.init . getCol em) [0..cols-1]
        cols = getCols em
        pairify (x:y:z:[]) = (x,y,z)

getRows (M r _ _) = r
getCols (M _ c _) = c
getVec  (M _ _ v) = v

getRow :: Num a => Matrix a -> Int -> V.Vector a
getRow m@(M r c v) n
  | n >= r = error "Requested row greater than matrix dimension."
  | otherwise = V.slice (n*c) c v

getCol :: Num a => Matrix a -> Int -> V.Vector a
getCol m@(M r c v) n
  | n >= c = error "Requested column greater than matrix dimension."
  | otherwise = V.generate r $ \i -> v V.! (i*c+n)

multMatrix :: Num a => Matrix a -> Matrix a -> Matrix a
multMatrix a@(M r1 c1 v1) b@(M r2 c2 v2)
  | c1 /= r2 = error "Can't multiply matrices due to mismatched dimensions."
  | otherwise = M r1 c2 v'
    where v' = V.concat rows
          rows = map mult_ [(i,j) | i<-[0..r1-1], j<-[0..c2-1]]
          mult_ (i,j) = V.singleton . V.sum $ V.zipWith (*) (getRow a i) (getCol b j)

scaleMatrix :: Num a => a -> Matrix a -> Matrix a
scaleMatrix n m = fmap (*n) m

identMatrix :: Num a => Int -> Matrix a
identMatrix n = M n n $ V.generate (n*n) $ \i -> if i `mod` (n+1) == 0
                                                    then 1
                                                    else 0

translate :: Num a => a -> a -> a -> Matrix a -> Matrix a
translate a b c m = multMatrix m (translationMatrix a b c)
  where translationMatrix :: Num a => a -> a -> a -> Matrix a
        translationMatrix a b c = fromList 4 4 [1, 0, 0, a
                                               ,0, 1, 0, b
                                               ,0, 0, 1, c
                                               ,0, 0, 0, 1]

dilate :: Num a => a -> a -> a -> Matrix a -> Matrix a
dilate a b c m = multMatrix m (dilationMatrix a b c)
  where dilationMatrix :: Num a => a -> a -> a -> Matrix a
        dilationMatrix a b c = fromList 4 4 [a, 0, 0, 0
                                            ,0, b, 0, 0
                                            ,0, 0, c, 0
                                            ,0, 0, 0, 1]

degToRad :: Double -> Double
degToRad d = d*pi/180.0

rotateX :: Double -> Matrix Double -> Matrix Double
rotateX d m = multMatrix m (rotationMatrix $ degToRad d)
  where rotationMatrix theta = fromList 4 4 [1, 0        ,  0        , 0
                                            ,0, cos theta, sin theta, 0
                                            ,0, -sin theta,  cos theta, 0
                                            ,0, 0        ,  0        , 1]

rotateY :: Double -> Matrix Double -> Matrix Double
rotateY d m = multMatrix m (rotationMatrix $ degToRad d)
  where rotationMatrix theta = fromList 4 4 [cos theta, 0, -sin theta, 0
                                            ,0        , 1, 0         , 0
                                            ,sin theta, 0,  cos theta, 0
                                            ,0        , 0,  0        , 1]

rotateZ :: Double -> Matrix Double -> Matrix Double
rotateZ d m = multMatrix m (rotationMatrix $ degToRad d)
  where rotationMatrix theta = fromList 4 4 [cos theta, -sin theta, 0, 0
                                            ,-sin theta,  cos theta, 0, 0
                                            ,0        ,  0        , 1, 0
                                            ,0        ,  0        , 0, 1]
