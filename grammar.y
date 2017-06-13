{
module Grammar where
import Lexer
}

%name parse
%tokentype { Token }
%monad { E } { thenE } { returnE }
%error { parseError }

%token
    COMMENT  { TokComment  }
    FLOAT    { TokFloat $$ }
    INT      { TokInt   $$ }
    STRING   { TokStr   $$ }
    PUSH     { TokPush     }
    POP      { TokPop      }
    LINE     { TokLine     }
    CIRCLE   { TokCircle   }
    HERMITE  { TokHermite  }
    BEZIER   { TokBezier   }
    BOX      { TokBox      }
    SPHERE   { TokSphere   }
    TORUS    { TokTorus    }
    IDENT    { TokIdent    }
    SCALE    { TokScale    }
    MOVE     { TokMove     }
    ROTATE   { TokRotate   }
    CLEAR    { TokClear    }
    VARY     { TokVary     }
    FRAMES   { TokFrames   }
    BASENAME { TokBasename }
    APPLY    { TokApply    }
    DISPLAY  { TokDisplay  }
    SAVE     { TokSave     }

%%

Combiner : Statement          { Atom $1 }
         | Combiner Combiner  { Node $1 $2 }

Statement : COMMENT                       { Comment }
| PUSH                                    { Push }
| POP                                     { Pop }
| LINE Num Num Num Num Num Num            { Line ($2,$3,$4) ($5,$6,$7) }
| CIRCLE Num Num Num Num                  { Circle ($2,$3,$4) $5 }
| HERMITE INT INT INT INT INT INT INT INT { Hermite ($2,$3) ($4,$5) ($6,$7) ($8,$9) }
| BEZIER INT INT INT INT INT INT INT INT  { Bezier ($2,$3) ($4,$5) ($6,$7) ($8,$9) }
| BOX Num Num Num Num Num Num             { Box ($2,$3,$4) ($5,$6,$7) }
| SPHERE Num Num Num Num                  { Sphere ($2,$3,$4) $5}
| TORUS Num Num Num Num Num               { Torus ($2,$3,$4) $5 $6 }
| IDENT                                   { Ident }
| SCALE Num Num Num                       { Scale ($2,$3,$4) }
| SCALE Num Num Num STRING                { ScaleV ($2,$3,$4) $5 }
| MOVE Num Num Num                        { Move ($2,$3,$4) }
| MOVE Num Num Num STRING                 { MoveV ($2,$3,$4) $5 }
| ROTATE STRING Num                       { Rotate $2 $3 }
| ROTATE STRING Num STRING                { RotateV $2 $3 $4 }
| CLEAR                                   { Clear }
| VARY STRING INT INT INT INT             { Vary $2 ($3,$4) ($5,$6) }
| FRAMES INT                              { Frames $2 }
| BASENAME STRING                         { Basename $2 }
| APPLY                                   { Apply }
| DISPLAY                                 { Display }
| SAVE STRING                             { Save $2 }

Num : FLOAT { $1 }
    | INT   { fromInteger $1 :: Double }

{
data E a = Ok a | Failed String

thenE :: E a -> (a -> E b) -> E b
m `thenE` k =
    case m of
        Ok a     -> k a
        Failed e -> Failed e

returnE :: a -> E a
returnE a = Ok a

failE :: String -> E a
failE err = Failed err

catchE :: E a -> (String -> E a) -> E a
m `catchE` k =
    case m of
        Ok a     -> Ok a
        Failed e -> k e

parseError :: [Token] -> E a
parseError tks = failE $ "Parse error!" ++ show tks
}
