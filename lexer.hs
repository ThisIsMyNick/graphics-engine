module Lexer where
import Data.Char

type Point2D = (Integer, Integer)
type Point3D = (Double, Double, Double)

data Combiner a = Atom a
                | Node (Combiner a) (Combiner a)
              deriving Show

instance Functor Combiner where
    fmap f (Atom x) = Atom (f x)
    fmap f (Node a b) = Node (fmap f a) (fmap f b)

data Statement = Comment
               | Push
               | Pop
               | Line Point3D Point3D
               | Circle Point3D Double
               | Hermite Point2D Point2D Point2D Point2D
               | Bezier Point2D Point2D Point2D Point2D
               | Box Point3D Point3D
               | Sphere Point3D Double
               | Torus Point3D Double Double
               | Ident
               | Scale Point3D
               | ScaleV Point3D String
               | Move Point3D
               | MoveV Point3D String
               | Rotate String Double
               | RotateV String Double String
               | Clear
               | Vary String (Integer,Integer) (Integer,Integer)
               | Frames Integer
               | Basename String
               | Apply
               | Display
               | Save String
               deriving Show

data Token
    = TokComment
    | TokPush
    | TokPop
    | TokFloat Double
    | TokInt Integer
    | TokStr String
    | TokLine
    | TokCircle
    | TokHermite
    | TokBezier
    | TokBox
    | TokSphere
    | TokTorus
    | TokIdent
    | TokScale
    | TokMove
    | TokRotate
    | TokClear
    | TokVary
    | TokFrames
    | TokBasename
    | TokApply
    | TokDisplay
    | TokSave
    deriving Show

--------------------------

lexer :: String -> [Token]
lexer [] = []
lexer ('/':'/':cs) = let (_,_:rest) = span (/= '\n') cs in lexer rest
lexer (c:cs)
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
    | isDigit c = lexNum (c:cs)
    | c == '-'  = lexNum (c:cs)
    | otherwise = lexer cs

lexVar cs =
    case span (\x -> isAlpha x) cs of
        ("push",rest) -> TokPush : lexer rest
        ("pop",rest)  -> TokPop  : lexer rest
        ("line",rest) -> TokLine : lexer rest
        ("circle",rest) -> TokCircle : lexer rest
        ("hermite",rest) -> TokHermite : lexer rest
        ("bezier",rest) -> TokBezier : lexer rest
        ("box",rest) -> TokBox : lexer rest
        ("sphere",rest) -> TokSphere : lexer rest
        ("torus",rest) -> TokTorus : lexer rest
        ("ident",rest) -> TokIdent : lexer rest
        ("scale",rest) -> TokScale : lexer rest
        ("move",rest) -> TokMove : lexer rest
        ("rotate",rest) -> TokRotate : lexer rest
        ("clear",rest) -> TokClear : lexer rest
        ("vary",rest) -> TokVary : lexer rest
        ("frames",rest) -> TokFrames : lexer rest
        ("basename",rest) -> TokBasename : lexer rest
        ("apply",rest) -> TokApply : lexer rest
        ("display",rest) -> TokDisplay : lexer rest
        ("save",rest) -> TokSave : lexer rest
        (s,rest) -> TokStr s : lexer rest

--Repetitive, yes. Sry.
lexNum ('-':cs) =
    if '.' `elem` (takeWhile (not . isSpace) cs)
    then
        case span (\x -> isDigit x || x == '.') cs of
            (n,cs') -> TokFloat (- read n :: Double) : lexer cs'
    else
        case span isDigit cs of
            (n,cs') -> TokInt (- read n :: Integer) : lexer cs'
lexNum cs =
    if '.' `elem` (takeWhile (not . isSpace) cs)
    then
        case span (\x -> isDigit x || x == '.') cs of
            (n,cs') -> TokFloat (read n :: Double) : lexer cs'
    else
        case span isDigit cs of
            (n,cs') -> TokInt (read n :: Integer) : lexer cs'

--main = getContents >>= print . parse . lexer
