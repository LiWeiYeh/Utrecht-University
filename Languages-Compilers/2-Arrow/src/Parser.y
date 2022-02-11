{
module Parser where

import Model
}

%name foo
%tokentype { Token }
%error { happyError }

%token
  '->'                                  { TArrow      }
  '.'                                   { TDot        }
  ','                                   { TComma      }
  go                                    { TGo         }
  take                                  { TTake       }
  mark                                  { TMark       }
  nothing                               { TNothing    }
  turn                                  { TTurn       }
  case                                  { TCase       }
  of                                    { TOf         }
  end                                   { TEnd        }
  left                                  { TLeft       }
  right                                 { TRight      }
  front                                 { TFront      }
  ';'                                   { TSemicolon  }
  Empty                                 { TEmpty      }
  Lambda                                { TLambda     }
  Debris                                { TDebris     }
  Asteroid                              { TAsteroid   }
  Boundary                              { TBoundary   }
  '_'                                   { TUnderscore }
  Ident                                 { TIdent $$   }
%%

program : Rules { Program $1 }

Rule : Ident '->' Cmds '.' { Rule $1 $3 }

Rules : Rule { [$1] }
      | Rule Rules { $1 : $2}

Cmds : { - empty - } { [] }
     | Cmd morecmds { $1 : $2 }

morecmds : { - empty - } { [] }
         | ',' Cmds Cmd morecmds { $2 : $3}

Cmd : go { Go }
    | take { Take }
    | nothing { Nothing'}
    | turn Dir { Turn $2 }
    | case Dir of Alts end { Case $2 $4}
    | Ident { Ident $1  }


Dir : left  { Left }
    | right { Right }
    | front { Front }

Alts : {- empty -}            { [] }
     | Alt                    { [$1] }
     | Alts ';' Alt           { $3 : $1 }

Alt : pat '->' Cmds { Alt $1 : $3 }

pat : empty       { Empty }
    | lambda      { Lambda }
    | debris      { Debris }
    | asteroid    { Asteroid }
    | boundary    { Boundary }
    | underscore  { Underscore }

{
happyError :: [Token] -> a
happyError _ = error "parse error"
}
