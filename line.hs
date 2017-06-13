{-# LANGUAGE FlexibleContexts #-}
module Line (
    line
  , getOct
  ) where

import Types
import Data.Array
import Control.Monad.State

type Octant = Integer

--Assumes 0<=m<=1
line :: Octant -> Point3D -> Point3D -> [Point3D]
line 1 p1 p2 = step p1 p2 (2* calca + calcb) calca calcb
  where
    dy p1@(_,b,_) p2@(_,d,_) = d-b
    dx p1@(a,_,_) p2@(c,_,_) = c-a
    dz p1@(_,_,e) p2@(_,_,f) = (f-e) / (dx p1 p2)
    dz0 = dz p1 p2
    calca = dy p1 p2
    calcb = -dx p1 p2

    step p1@(x1, y1, z1) p2@(x2, y2, z2) d a b
      | x1 > x2 = []
      | y1 > y2 = []
      | d > 0 = p1 : step (x1+1,y1+1,z1+dz0) p2 (d + 2*a + 2*b) a b
      | otherwise = p1 : step (x1+1,y1,z1+dz0) p2 (d + 2*a) a b

line 2 p1 p2 = step p1 p2 (calca + 2*calcb) calca calcb
  where
    dy p1@(_,b,_) p2@(_,d,_) = d-b
    dx p1@(a,_,_) p2@(c,_,_) = c-a
    dz p1@(_,_,e) p2@(_,_,f) = (f-e) / (dy p1 p2)
    dz0 = dz p1 p2
    calca = dy p1 p2
    calcb = -dx p1 p2

    step (x1, y1, z1) p2@(x2, y2, z2) d a b
      | x1 > x2 = []
      | y1 > y2 = []
      | d < 0 = (x1,y1,z1) : step (x1+1,y1+1,z1+dz0) p2 (d + 2*a + 2*b) a b
      | otherwise = (x1,y1,z1) : step (x1,y1+1,z1+dz0) p2 (d + 2*b) a b

line 7 p1 p2 = step p1 p2 (calca + 2*calcb) calca calcb
  where
    dy p1@(_,b,_) p2@(_,d,_) = d-b
    dx p1@(a,_,_) p2@(c,_,_) = c-a
    dz p1@(_,_,e) p2@(_,_,f) = (f-e) / (dy p1 p2)
    dz0 = dz p1 p2
    calca = dy p1 p2
    calcb = -dx p1 p2

    step (x1, y1, z1) p2@(x2, y2, z2) d a b
      | x1 > x2 = []
      | y2 > y1 = []
      | d > 0 = (x1,y1,z1) : step (x1+1,y1-1,z1+dz0) p2 (d + 2*a - 2*b) a b
      | otherwise = (x1,y1,z1) : step (x1,y1-1,z1+dz0) p2 (d - 2*b) a b

line 8 p1 p2 = step p1 p2 (2*calca - calcb) calca calcb
  where
    dy p1@(_,b,_) p2@(_,d,_) = d-b
    dx p1@(a,_,_) p2@(c,_,_) = c-a
    dz p1@(_,_,e) p2@(_,_,f) = (f-e) / (dx p1 p2)
    dz0 = dz p1 p2
    calca = dy p1 p2
    calcb = -dx p1 p2

    step (x1, y1, z1) p2@(x2, y2, z2) d a b
      | x1 > x2 = []
      | y2 > y1 = []
      | d < 0 = (x1,y1,z1) : step (x1+1,y1-1,z1+dz0) p2 (d + 2*a - 2*b) a b
      | otherwise = (x1,y1,z1) : step (x1+1,y1,z1+dz0) p2 (d + 2*a) a b

line n _ _ = undefined

getOct :: Point3D -> Point3D -> (Octant, Point3D, Point3D)
getOct p1@(x1,y1,z1) p2@(x2,y2,z2)
  | x1 > x2 = getOct p2 p1
  | otherwise =
      if slope >= 0
        then if slope <= 1
          then (1,p1,p2)
          else (2,p1,p2)
      else if slope <= -1
        then (7,p1,p2)
        else (8,p1,p2)
  where div' a b = fromIntegral a / fromIntegral b
        slope = (y2-y1) / (x2-x1)
