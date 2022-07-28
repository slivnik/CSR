{
module InpParser ( readExtCSRFile ) where

import Data.List
import System.IO

import Common
import InpAST
import InpLexer
import qualified CSR
}

%name inpParser

%tokentype { Token }
%token
  tokens   { Token _ SymTOKENS _ }
  goal     { Token _ SymGOAL   _ }
  arrow    { Token _ SymARROW  _ }
  comma    { Token _ SymCOMMA  _ }
  semic    { Token _ SymSEMIC  _ }
  lpar     { Token _ SymLPAR   _ }
  rpar     { Token _ SymRPAR   _ }
  dsj      { Token _ SymDSJ    _ }
  neg      { Token _ SymNEG    _ }
  pos      { Token _ SymPOS    _ }
  symbol   { Token _ SymSYMBOL _ }

%%

System
  : tokens Toks semic
    goal symbol semic
    Reds
      { ($2,lexeme $5,$7) }

Toks
  :
      { [] }
  | symbol Toks
      { (lexeme $1):$2 }

Reds
  : Red
      { [$1] }
  | Red Reds
      { $1:$2 }

Red
  : Srcs arrow Dsts semic
      { Reduction (-1) $1 $3 Nothing }

Srcs
  : SrcPfx
      { [$1] }
  | SrcPfx Srcs
      { $1:$2 }

SrcDsj
  : SrcCnc
      { $1 }
  | SrcCnc dsj SrcDsj
      { SrcDsj $1 $3 }
  ;

SrcCnc
  : SrcPfx
      { $1 }
  | SrcPfx SrcCnc
      { SrcCnc $1 $2 }

SrcPfx
  : SrcAtm
      { $1 }
  | neg SrcAtm
      { SrcNeg $2 }

SrcAtm
  : symbol
      { SrcAtm (lexeme $1) }
  | lpar SrcDsj rpar
      { $2 }

Dsts
  : Dst
      { [$1] }
  | Dst Dsts
      { $1:$2 }

Dst
  : pos
      { DstOld (read (lexeme $1)) Nothing }
  | pos lpar SrcPfx rpar
      { DstOld (read (lexeme $1)) (Just $3) }
  | symbol
      { DstNew (lexeme $1) []}
  | symbol lpar Poss rpar
      { DstNew (lexeme $1) $3 }

Poss
  : pos
      { [read (lexeme $1)] }
  | pos comma Poss
      { (read (lexeme $1)):$3 }

{
loc :: (Locate loc1,Locate loc2) => loc1 -> loc2 -> Location 
loc l1 l2 = (fst (locate l1),snd (locate l2))

lexeme :: Token -> String
lexeme (Token _ _ lex) = lex

happyError :: [Token] -> a
happyError [] = error ("Syntax error at the end of file.")
happyError ((Token ((ln,ch),(_,_)) _ lex):_) =
  error ("["++(show ln)++":"++(show ch)++"] Unexpected token '"++lex++"'.\n")

readExtCSRFile :: FilePath -> IO ([String],String,[Reduction])
readExtCSRFile fileName =
  withFile fileName ReadMode (\ inpH -> do
  inp <- hGetContents inpH
  let (tokens,goal,unnumReductions) = inpParser (inpLexer (error "") inp)
  let reductions =
        [ Reduction rId srcs dsts orig
        | (rId,Reduction _ srcs dsts orig) <- zip [1..] unnumReductions ]
  ([do
      if checkBegSymb red then return () else
        error (":-( Reduction "++(show red)++" violates [-rule.")
      if checkEndSymb red then return () else
        error (":-( Reduction "++(show red)++" violates ]-rule.")
    | red <- reductions ]
   |> foldr (>>) (return ()))
  if (length reductions)==(-1) then return () else return ()
  return (tokens,goal,reductions))
}
