module Types (
    Point2D
  , Point3D
  , Color
  , Slope
  ) where

import Data.Array.Unboxed

type Point2D = (Integer, Integer)
type Point3D = (Double, Double, Double)
type Color = (Int, Int, Int)
type Slope = (Integer, Integer)
