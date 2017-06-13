module Parser (
  parseFile
  ) where

import Types
import Line
import Curve
import Display
import Matrix
import Data.List
import qualified Data.Vector as V
import Control.Monad as M
import Control.Monad.State
import System.Process
import System.Directory
import Data.Foldable hiding (elem)
import qualified Data.HashTable.IO as H
import Text.Printf
import Grammar
import Lexer

type ParseState = (PicInfo,PolygonMatrix, EdgeMatrix, [Matrix Double])

type HashTable k v = H.BasicHashTable k v

--PicInfo Basename Frames SymbolTable
data PicInfo = PicInfo { basename :: String
                       , frames   :: Int
                       , htable   :: HashTable String [Double]
                       , isgif    :: Bool
                       } deriving (Show)



toPointlist pts = zip pts (tail pts)

execute :: Int -> Statement -> StateT ParseState IO ()
execute fr (Line p1 p2) = do
  (pp,pm,em,tm) <- get
  let el = execState (do (addEdge3D p1 p2)) em
  put (pp,pm,el,tm)

execute fr (Circle p r) = do
  (pp,pm,em,tm) <- get
  let pts = toPointlist $ circle p r
  let el = execState (do
                     M.mapM_ (uncurry addEdge3D) pts
                     ) em
  put (pp,pm,el,tm)

execute fr (Hermite p1 p2 m1 m2) = do
  (pp,pm,em,tm) <- get
  let pts = toPointlist $ hermite p1 p2 m1 m2
  let el = execState (do
                     M.mapM_ (uncurry addEdge) pts
                     ) em
  put (pp,pm,el,tm)

execute fr (Bezier p1 p2 p3 p4) = do
  (pp,pm,em,tm) <- get
  let pts = toPointlist $ bezier p1 p2 p3 p4
  let el = execState (do
                     M.mapM_ (uncurry addEdge) pts
                     ) em
  put (pp,pm,el,tm)

execute fr (Box p1@(x,y,z) p2@(w,h,d)) = do
  (pp,pm,em,t:tm) <- get
  let x1 = x + w
  let y1 = y - h
  let z1 = z - d
  let tem = []
  let el = execState (do
                      addPolygon (x, y1, z)   (x, y, z)   (x1, y, z)
                      addPolygon (x, y1, z)   (x1, y, z)  (x1, y1, z)
                      addPolygon (x1, y1, z1) (x1, y, z1) (x, y, z1)
                      addPolygon (x1, y1, z1) (x, y, z1)  (x, y1, z1)
                      addPolygon (x, y1, z1)  (x, y1, z)  (x1, y1, z)
                      addPolygon (x, y1, z1)  (x1, y1, z) (x1, y1, z1)
                      addPolygon (x1, y, z1)  (x1, y, z)  (x, y, z)
                      addPolygon (x1, y, z1)  (x, y, z)   (x, y, z1)
                      addPolygon (x, y1, z1)  (x, y, z1)  (x, y, z)
                      addPolygon (x, y1, z1)  (x, y, z)   (x, y1, z)
                      addPolygon (x1, y1, z)  (x1, y, z)  (x1, y, z1)
                      addPolygon (x1, y1, z)  (x1, y, z1) (x1, y1, z1)
                     ) tem
  let tem' = toEdgelist $ multMatrix t (fromEdgelist el)
  put (pp,tem'++pm,em,t:tm)

execute fr (Sphere p1 r) = do
  (pp,pm,em,t:tm) <- get
  let sph = sphere p1 r
  let tem = []
  let el = execState (do
                     M.mapM_ ((\f (a,b,c) -> f a b c) addPolygon) sph
                     ) tem
  let tem' = toEdgelist $ multMatrix t (fromEdgelist el)
  put (pp,tem'++pm,em,t:tm)

execute fr (Torus p1 r1 r2) = do
  (pp,pm,em,t:tm) <- get
  let tor = torus p1 r1 r2
  let tem = []
  let el = execState (do
                     M.mapM_ ((\f (a,b,c) -> f a b c) addPolygon) tor
                     ) tem
  let tem' = toEdgelist $ multMatrix t (fromEdgelist el)
  put (pp,tem'++pm,em,t:tm)

execute fr (Ident) = do
  (pp,pm,em,t:tm) <- get
  let t' = identMatrix 4
  put (pp,pm,em,t':tm)

execute fr (Scale (x,y,z)) = do
  (pp,pm,em,t:tm) <- get
  let sm = dilate x y z t
  put (pp,pm,em,sm:tm)

execute fr (ScaleV (x,y,z) v) = do
  (PicInfo bn f ht ig,pm,em,t:tm) <- get
  knob <- lift $ H.lookup ht v
  let knobVal = case knob of
                  Just a -> a
                  Nothing -> error $ "Invalid variable: " ++ v
  let kv = knobVal !! fr
  let sm = dilate (x*kv) (y*kv) (z*kv) t
  put (PicInfo bn f ht ig,pm,em,sm:tm)

execute fr (Move (x,y,z)) = do
  (pp,pm,em,t:tm) <- get
  let mm = translate x y z t
  put (pp,pm,em,mm:tm)

execute fr (MoveV (x,y,z) v) = do
  (PicInfo bn f ht ig,pm,em,t:tm) <- get
  knob <- lift $ H.lookup ht v
  let knobVal = case knob of
                  Just a -> a
                  Nothing -> error $ "Invalid variable: " ++ v
  let kv = knobVal !! fr
  let mm = translate (x*kv) (y*kv) (z*kv) t
  put (PicInfo bn f ht ig,pm,em,mm:tm)

execute fr (Rotate axis theta)
  | axis == "x" = do
                  (pp,pm,em,t:tm) <- get
                  let rm = rotateX theta t
                  put (pp,pm,em,rm:tm)
  | axis == "y" = do
                  (pp,pm,em,t:tm) <- get
                  let rm = rotateY theta t
                  put (pp,pm,em,rm:tm)
  | axis == "z" = do
                  (pp,pm,em,t:tm) <- get
                  let rm = rotateZ theta t
                  put (pp,pm,em,rm:tm)
  | otherwise = error "Invalid rotational axis."

execute fr (RotateV axis theta v)
  | axis == "x" = do
                  (PicInfo bn f ht ig,pm,em,t:tm) <- get
                  knob <- lift $ H.lookup ht v
                  let knobVal = case knob of
                                  Just a -> a
                                  Nothing -> error $ "Invalid variable: " ++ v
                  let kv = knobVal !! fr
                  let rm = rotateX (theta*kv) t
                  put (PicInfo bn f ht ig,pm,em,rm:tm)
  | axis == "y" = do
                  (PicInfo bn f ht ig,pm,em,t:tm) <- get
                  knob <- lift $ H.lookup ht v
                  let knobVal = case knob of
                                  Just a -> a
                                  Nothing -> error $ "Invalid variable: " ++ v
                  let kv = knobVal !! fr
                  let rm = rotateY (theta*kv) t
                  put (PicInfo bn f ht ig,pm,em,rm:tm)
  | axis == "z" = do
                  (PicInfo bn f ht ig,pm,em,t:tm) <- get
                  knob <- lift $ H.lookup ht v
                  let knobVal = case knob of
                                  Just a -> a
                                  Nothing -> error $ "Invalid variable: " ++ v
                  let kv = knobVal !! fr
                  let rm = rotateZ (theta*kv) t
                  put (PicInfo bn f ht ig,pm,em,rm:tm)
  | otherwise = error "Invalid rotational axis."

execute fr (Clear) = do
  (pp,pm,em,tm) <- get
  let pm' = []
  let em' = []
  put (pp,pm',em',tm)

execute fr (Vary s (s1,s2) (v1,v2)) = do
  (PicInfo bn f ht _,pm,em,tm) <- get
  put (PicInfo bn f ht True,pm,em,tm)

execute fr (Frames n) = do
  (PicInfo bn f ht _,pm,em,tm) <- get
  put (PicInfo bn f ht True,pm,em,tm)

execute fr (Basename s) = do
  (PicInfo bn f ht _,pm,em,tm) <- get
  put (PicInfo bn f ht True,pm,em,tm)

execute fr (Apply) = do
  (pp,pm,em,t:tm) <- get
  let mp = toEdgelist . multMatrix t $ fromEdgelist pm
  let me = toEdgelist . multMatrix t $ fromEdgelist em
  let t' = identMatrix 4
  put (pp,mp,me,t':tm)

execute fr (Display) = do
  (PicInfo bn f ht ig,pm,em,tm) <- get
  when (ig == True) $ do
    error "No support for displaying gifs yet. Please save instead."
  let mp = fromEdgelist pm
  let me = fromEdgelist em
  lift $ writeFile "temp.ppm" . showPic . flipY $ draw mp me
  (_,_,_,hnd) <- lift $ createProcess (shell "display temp.ppm")
  lift $ waitForProcess hnd
  discard <- lift $ removeFile "temp.ppm"
  put (PicInfo bn f ht ig,pm,em,tm)

execute fr (Save s)
  |    ';'  `elem` s ||
       '#'  `elem` s ||
       '\'' `elem` s = do
                  error "Unacceptable file name specified."
  | otherwise = do
    (PicInfo bn f ht ig,pm,em,tm) <- get
    when (';' `elem` bn
      ||  '#' `elem` bn
      ||  '\'' `elem` bn) $ do
        error "Unacceptable file name specified."
    --lift $ writeGif s $ (PicInfo bn f ht ig,pm,em,tm)
    lift $ writeImg fr s $ (PicInfo bn f ht ig,pm,em,tm)

    -- let mp = fromEdgelist pm
    -- let me = fromEdgelist em
    -- lift $ writeFile "pic.ppm" . showPic . flipY $ draw mp me
    -- (_,_,_,hnd) <- lift $ createProcess
    --                       (shell $ "convert pic.ppm '" ++ bn ++ "'")
    -- lift $ waitForProcess hnd
    -- discard <- lift $ removeFile "pic.ppm"
    put (PicInfo bn f ht ig,pm, em,tm)

execute fr (Push) = do
  (pp, pm, em,t:tm) <- get
  put (pp, pm, em,t:t:tm)

execute fr (Pop) = do
  (pp, pm, em, _:tm) <- get
  put (pp, pm, em, tm)

-- execute (None) = do
--   (pp,pm,em,tm) <- get
--   put (pp,pm,em,tm)

-- execute (Error s) = do
--   lift $ putStrLn ("Invalid operation: " ++ s)

writeImg :: Int -> String -> ParseState -> IO ()
writeImg fr dir ps = do
  let (PicInfo bn f ht ig,pm,em,tm) = ps
  putStrLn $ "writeImg frame " ++ show fr
  when (fr == 0) $ do
    createDirectoryIfMissing True dir
    removeDirectoryRecursive dir
    createDirectory dir
    return ()
  when (fr == f-1) $ do
    let cmd = "convert -delay 3 " ++ (dir ++ "/" ++ bn ++ "*.ppm") ++ " " ++ bn ++ ".gif"
    (_,_,_,hnd) <- createProcess (shell cmd)
    waitForProcess hnd
    return ()
  let fn = dir ++ "/" ++ bn ++ printf "%02d" fr ++ ".ppm"
  let mp = fromEdgelist pm
  let me = fromEdgelist em
  writeFile fn . showPic . flipY $ draw mp me

-- writeGifFrame :: Int -> String -> ParseState -> IO ()
-- writeGifFrame dir ps = do
--   removeDirectoryRecursive dir
--   createDirectory dir

--   let (PicInfo bn f ht ig, pm, em, tm) = ps
--   for_ [0..f-1] $ \p -> do
--     writeImg (dir ++ "/" ++ bn ++ show p ++ ".png") ps

--   let cmd = "convert -delay 3 " ++ (dir ++ "/" ++ bn ++ "*") ++ " " ++ bn ++ ".gif"
--   (_,_,_,hnd) <- createProcess (shell cmd)
--   waitForProcess hnd
--   return ()

----------FIRST SCAN----------
exec_anim :: Int -> Statement -> StateT ParseState IO ()
exec_anim fr (Frames n) = do
  (PicInfo bn _ ht ig,pm,em,tm) <- get
  put (PicInfo bn (fromIntegral n) ht True,pm,em,tm)

exec_anim fr (Basename s) = do
  (PicInfo _ f ht _,pm,em,tm) <- get
  put (PicInfo s f ht True,pm,em,tm)

exec_anim _ _ = do
  (pp,pm,em,tm) <- get
  put (pp,pm,em,tm)

----------SECOND SCAN----------
exec_vary :: Int -> Statement -> StateT ParseState IO ()
exec_vary fr (Vary s (s1,s2) (v1,v2)) = do
  (PicInfo bn f ht _,pm,em,tm) <- get
  knob <- lift $ H.lookup ht s
  case knob of
    Just a -> lift $ continueL ht a
    Nothing -> lift $ newL ht
  put (PicInfo bn f ht True,pm,em,tm)
  where continueL :: HashTable String [Double] -> [Double] -> IO ()
        continueL ht a = do
          let diff = fromIntegral (v2 - v1) / fromIntegral (s2 - s1)
          let maxFrame = length a - 1
          when (fromIntegral s1 /= length a) $ do
            error "Continue frame at right spot pls"
          let l = [fromIntegral v1,fromIntegral v1+diff..fromIntegral v2]
          H.insert ht s (a++l)
          return ()
        newL :: HashTable String [Double] -> IO ()
        newL ht = do
          let diff = fromIntegral (v2 - v1) / fromIntegral (s2 - s1)
          let l = [fromIntegral v1,fromIntegral v1+diff..fromIntegral v2]
          H.insert ht s l
          return ()

exec_vary _ _ = do
  (pp,pm,em,tm) <- get
  put (pp,pm,em,tm)

-------------------------

combfold p f (Atom x) = f p x : []
combfold p f (Node a b) = combfold p f a ++ combfold p f b

parseFile :: String -> IO ()
parseFile fn = do
  content <- readFile fn
  let (Ok cmds) = parse . lexer $ content
  case (parse . lexer $ content) of
    Failed err -> error err
    Ok a -> return ()

  --1st pass, find animation commands
  ht <- H.new
  (pp,_,_,_) <- execStateT (do
                  M.sequence $ combfold 0 exec_anim cmds
                  ) (PicInfo "img" 1 ht False, [], [], [identMatrix 4])

  --2nd pass, find vary
  (pp',_,_,_) <- execStateT (do
                  M.sequence $ combfold 0 exec_vary cmds
                  ) (pp, [], [], [identMatrix 4])
  let f = frames pp'

  M.forM_ [0..f-1] $ \p -> do
    _ <- execStateT (do
                    M.sequence $ combfold p execute cmds
                    ) (pp',[],[],[identMatrix 4])
    return ()
  --d <- execStateT (do
  --                --M.sequence $ fmap execute cmds
  --                M.forM_ [0..f-1] $ \p -> do
  --                  M.sequence $ combfold p execute cmds
  --                ) (pp',[], [], [identMatrix 4])
  return ()
