{-# LANGUAGE FlexibleContexts #-}
module Display (
    addEdge
  , addEdge3D
  , addPolygon
  , group
  , flipY
  , showPic
  , draw
  ) where

import Types
import Line
import Curve
import Matrix
import Data.Array.ST
import Data.Array.Unboxed
import Control.Monad as M
import Control.Monad.State
import qualified Data.Vector as V
import Debug.Trace

addPoint :: Point3D -> State PolygonMatrix ()
addPoint p = do
  em <- get
  put (p:em)

addEdge :: Point2D -> Point2D -> State EdgeMatrix ()
addEdge p1@(x1,y1) p2@(x2,y2) = do
  addPoint (fromIntegral x2, fromIntegral y2, 0.0)
  addPoint (fromIntegral x1, fromIntegral y1, 0.0)

addEdge3D :: Point3D -> Point3D -> State EdgeMatrix ()
addEdge3D p1 p2 = do
  addPoint p2
  addPoint p1

addPolygon :: Point3D -> Point3D -> Point3D -> State PolygonMatrix ()
addPolygon p1 p2 p3 = do
  addPoint p3
  addPoint p2
  addPoint p1

--Taken from http://stackoverflow.com/a/12876438
group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative n"

colToInt :: Color -> Int
colToInt (r,g,b) = (r*256*256)+(g*256)+b


flipY :: Array Point2D Int -> Array Point2D Int
flipY d = runSTArray $ do
  arr <- newArray ((0,0),(499,499)) $ colToInt (0,0,0)
  forM_ [(i,j) | i<-[0..499], j<-[0..499]] $ \(a,b) -> do
    writeArray arr (a,b) (d ! (499-a,499-b))
  return arr

drawLine arr p1 p2 col = do
  -- let p1' = (round $ p1 V.! 1, round $ p1 V.! 0)
  -- let p2' = (round $ p2 V.! 1, round $ p2 V.! 0)
  let (oct,p1',p2') = getOct p1 p2
  let pts = line oct p1' p2'
  --let pts' = filter (inRange ((0,0),(499,499))) pts
  let pts' = pts
  forM_ pts' $ \p -> do
    let (x,y) = getXY p
    when (0 <= x && x <= 499 && 0 <= y && y <= 499) $ do
      val <- readArray arr (x,y)
      when (getZ p > snd val) $ do
        writeArray arr (x,y) $ (colToInt col,getZ p)
        return ()
    return arr
    where getXY (a,b,_) = (round a,round b)
          getZ  (_,_,c) = round c

-- drawTriangle arr p1 p2 p3 = do
--   drawLine arr p1 p2
--   drawLine arr p2 p3
--   drawLine arr p3 p1

isVisible :: Point3D -> Point3D -> Point3D -> Bool
isVisible p1@(x1,y1,z1) p2@(x2,y2,z2) p3@(x3,y3,z3)
  | normal > 0.0 = True
  | otherwise = False
  where (_,_,normal) = crossProduct a b
        a = (x2 - x1, y2 - y1, z2 - z1)
        b = (x3 - x1, y3 - y1, z3 - z1)

crossProduct :: Point3D -> Point3D -> Point3D
crossProduct (x1,y1,z1) (x2,y2,z2) =
  (y1*z2 - z1*y2, z1*x2 - x1*z2, x1*y2 - y1*x2)

--Unused. Look at intToCol' below.
pixelify :: Color -> String
pixelify (r,g,b) = show r ++ " " ++ show g ++ " " ++ show b ++ " "

--Unused. Look at intToCol' below.
intToCol :: Int -> Color
intToCol n = (n `div` 256 `div` 256 `mod` 256, n `div` 256 `mod` 256, n `mod` 256)

--More efficient memory-wise than (pixelify . intToCol)
intToCol' :: Int -> String
intToCol' n = show r ++ " " ++ show g ++ " " ++ show b ++ " "
  where r = n `div` 256 `div` 256 `mod` 256
        g = n `div` 256 `mod` 256
        b = n `mod` 256

showPic :: Array Point2D Int -> String
showPic d = "P3 500 500 255\n" ++ concat (map intToCol' $ elems d)

scanline p1 p2 p3
  | getY p1 > getY p2 = scanline p2 p1 p3
  | getY p1 > getY p3 = scanline p3 p2 p1
  | getY p2 > getY p3 = scanline p1 p3 p2
  | getY p1 == getY p2 &&
    getX p2 < getX p1 = scanline p2 p1 p3
  | getY p2 == getY p3 &&
    getX p3 < getX p2 = scanline p1 p3 p2
  | otherwise = if bot == []
                   then []
                   else bot ++ top
      --Fill in bottom half first
      --p1 < p2 < p3
      where dx1 = safeDiv (getX p3 - getX p1) (getY p3 - getY p1)
            dx2 = safeDiv (getX p2 - getX p1) (getY p2 - getY p1)
            dz1 = safeDiv (getZ p3 - getZ p1) (getY p3 - getY p1)
            dz2 = safeDiv (getZ p2 - getZ p1) (getY p2 - getY p1)
            bot = step (getX p1) dx1 (getX p1) dx2 (getY p1) (getZ p1) dz1 (getZ p1) dz2 (0,0,255) p2
            --Top half
            dx2' = safeDiv (getX p3 - getX p2) (getY p3 - getY p2)
            dz2' = safeDiv (getZ p3 - getZ p2) (getY p3 - getY p2)
            (x1',y',z1') = (\(a,_,_)->a) $ last bot
            (x2',_ ,z2') = (\(_,b,_)->b) $ last bot
            top = step x1' dx1 (getX p2) dx2' y' z1' dz1 (getZ p2) dz2' (255,0,0) p3
            getX (x,_,_) = x :: Double
            getY (_,y,_) = y :: Double
            getZ (_,_,z) = z :: Double
            safeDiv a b
              | abs b < 1 = 0
              | otherwise = a / b
            step x1 dx1 x2 dx2 y z1 dz1 z2 dz2 col stop
              -- | y >= getY stop = (x1,dx1,x2,dx2,y,z1,dz1,z2,dz2)
              | y >= getY stop = []
              | otherwise = ((x1+dx1,y+1,z1+dz1),(x2+dx2,y+1,z2+dz2),col) :
                step (x1+dx1) dx1 (x2+dx2) dx2 (y+1) (z1+dz1) dz1 (z2+dz2) dz2 col stop

draw :: Matrix Double -> Matrix Double -> Array Point2D Int
draw pm@(M rp cp vp) em@(M re ce ve) = fmap fst $ runSTArray $ do
  --arr <- newArray ((0,0),(499,499)) $ colToInt (0,0,0)
  arr <- newArray ((0,0),(499,499)) $ (colToInt (0,0,0),minBound::Int)
  --farr <- newArray ((0,0),(499,499)) $ colToInt (0,0,0)
  let pointsp = map (getCol pm) [0..cp-1]
  let ptsp = group 3 pointsp

  let pointse = map (getCol em) [0..ce-1]
  let ptse = group 2 pointse

  --Draw
  M.mapM_ (drawPts_p arr) ptsp
  M.mapM_ (drawPts_e arr) ptse
  return arr
    where drawPts_p arr [a,b,c] = do
            let p1 = (a V.! 1, a V.! 0, a V.! 2)
            let p2 = (b V.! 1, b V.! 0, b V.! 2)
            let p3 = (c V.! 1, c V.! 0, c V.! 2)
            when (isVisible p1 p2 p3) $ do
               --drawTriangle arr p1 p2 p3
               forM_ (scanline p1 p2 p3) $ \(a,b,c) -> do
                 drawLine arr a b c
               return ()
            return ()
          drawPts_e arr [a,b] = do
            let p1 = (a V.! 1, a V.! 0, a V.! 2)
            let p2 = (b V.! 1, b V.! 0, b V.! 2)
            drawLine arr p1 p2 (255,255,255)
