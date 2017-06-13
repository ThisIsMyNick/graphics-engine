module Curve (
    circle
  , hermite
  , bezier
  , sphere
  , torus
  --, drawCircle
  ) where

import Types
import Matrix
import Data.Array
import Control.Monad.State

circle :: Point3D -> Double -> [Point3D]
circle p@(cx,cy,cz) r = map genPoints [0.01,0.02..1.01]
  where genPoints :: Double -> Point3D
        genPoints t = (x t, y t, z t)
        x,y,z :: Double -> Double
        x t = r * cos (2*pi*t) + cx
        y t = r * sin (2*pi*t) + cy
        z t = cz

-- invHermiteM = fromList 4 4 [ 2, -2, 1, 1
--                            ,-3, -2, 1, 1
--                            , 0,  0, 1, 0
--                            , 1,  0, 0, 0]
invHermiteM = fromList 4 4 [ 2, -2,  1,  1
                           ,-3,  3, -2, -1
                           , 0,  0,  1,  0
                           , 1,  0,  0,  0]

hermite :: Point2D -> Point2D -> Slope -> Slope -> [Point2D]
hermite p1@(x1,y1) p2@(x2,y2) m1@(rx0,ry0) m2@(rx1,ry1) = map genPoints [0.00,0.01..1]
  where genPoints :: Double -> Point2D
        genPoints t = (x t, y t)
        x,y :: Double -> Integer
        x t = round $ a_x * t**3 + b_x * t**2 + c_x * t + d_x
        y t = round $ a_y * t**3 + b_y * t**2 + c_y * t + d_y

        (a_x:b_x:c_x:d_x:[]) = map fromIntegral $ toList coeffsx
        (a_y:b_y:c_y:d_y:[]) = map fromIntegral $ toList coeffsy
        coeffsx = multMatrix invHermiteM givenMx
        givenMx = fromList 4 1 [x1,x2,rx0,rx1]
        coeffsy = multMatrix invHermiteM givenMy
        givenMy = fromList 4 1 [y1,y2,ry0,ry1]

invBezierM = fromList 4 4 [-1,  3, -3,  1
                          , 3, -6,  3,  0
                          ,-3,  3,  0,  0
                          , 1,  0,  0,  0]

bezier :: Point2D -> Point2D -> Point2D -> Point2D -> [Point2D]
bezier p1@(x1,y1) p2@(x2,y2) p3@(x3,y3) p4@(x4,y4) = map genPoints [0.00,0.01..1]
  where genPoints :: Double -> Point2D
        genPoints t = (x t, y t)
        x,y :: Double -> Integer
        x t = round $ a_x * t**3 + b_x * t**2 + c_x * t + d_x
        y t = round $ a_y * t**3 + b_y * t**2 + c_y * t + d_y

        (a_x:b_x:c_x:d_x:[]) = map fromIntegral $ toList coeffsx
        (a_y:b_y:c_y:d_y:[]) = map fromIntegral $ toList coeffsy
        coeffsx = multMatrix invBezierM givenMx
        givenMx = fromList 4 1 [x1,x2,x3,x4]
        coeffsy = multMatrix invBezierM givenMy
        givenMy = fromList 4 1 [y1,y2,y3,y4]

spherepts :: Point3D -> Double -> [Point3D]
spherepts p1@(cx,cy,cz) r = map step [(t,p) | t<-[0.0,0.05..1], p<-[0.0,0.05..1]]
  where step (t,p) = (x t p, y t p, z t p)
        x,y,z :: Double -> Double -> Double
        x t p = r * cos (pi*p) + cx
        y t p = r * sin (pi*p) * cos (2*pi*t) + cy
        z t p = r * sin (pi*p) * sin (2*pi*t) + cz

sphere :: Point3D -> Double -> [(Point3D,Point3D,Point3D)]
sphere p1 r = concat . connect $ spherepts p1 r
  where connect l = map
          (\(la,lo) -> triangles l la lo)
          [(la,lo) | la<-[0..stepsize-2], lo<-[0..stepsize-2]]
        stepsize = floor (1.0/0.05) + 1
        startIndex la lo = la*stepsize + lo
        nextIndex la lo = (la*stepsize+stepsize) + lo
        triangles l la lo
          | lo > 0 && lo < stepsize-2 =
            [(l !! (startIndex la lo)
             ,l !! (startIndex la lo +1)
             ,l !! (nextIndex la lo)),
             (l !! (startIndex la lo +1)
             ,l !! (nextIndex la lo +1)
             ,l !! (nextIndex la lo))]
          | lo > 0 =
            [(l !! (startIndex la lo)
             ,l !! (startIndex la lo +1)
             ,l !! (nextIndex la lo))]
          | lo < stepsize-2 =
             [(l !! (startIndex la lo +1)
              ,l !! (nextIndex la lo +1)
              ,l !! (nextIndex la lo))]

toruspts :: Point3D -> Double -> Double -> [Point3D]
toruspts p1@(cx,cy,cz) r1 r2 = map step [(t,p) | t<-[0.0,0.05..1], p<-[0.0,0.05..1]]
  where step (t,p) = (x t p, y t p, z t p)
        x,y,z :: Double -> Double -> Double
        x t p = cos (2*pi*t) * (r1 * cos (2*pi*p) + r2) + cx
        y t p = r1 * sin (2*pi*p) + cy
        z t p = sin (2*pi*t) * (r1 * cos (2*pi*p) + r2) + cz

torus :: Point3D -> Double -> Double -> [(Point3D, Point3D, Point3D)]
torus p1 r1 r2 = concat . connect $ toruspts p1 r1 r2
  where connect l = map
          (\(la,lo) -> triangles l la lo)
            [(la,lo) | la<-[0..stepsize-2], lo<-[0..stepsize-2]]
        stepsize = floor (1.0/0.05) + 1
        index la lo = la*stepsize + lo
        triangles l la lo = [(l !! (index la lo)
                             ,l !! (index la lo +1)
                             ,l !! (index la lo +stepsize))
                            ,(l !! (index la lo +stepsize+1)
                             ,l !! (index la lo +stepsize)
                             ,l !! (index la lo +1))]
