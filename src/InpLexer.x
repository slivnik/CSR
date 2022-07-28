{
module InpLexer ( Locate, Location, Token(..), Symbol(..), locate, inpLexer ) where
}

%wrapper "posn"

token :-
  $white+ ;
  "#".*   ;

  "-->"   { \ pos lex -> token pos SymARROW lex }
  ","     { \ pos lex -> token pos SymCOMMA lex }
  ";"     { \ pos lex -> token pos SymSEMIC lex }
  "("     { \ pos lex -> token pos SymLPAR lex }
  ")"     { \ pos lex -> token pos SymRPAR lex }
  "|"     { \ pos lex -> token pos SymDSJ lex }
  "!"     { \ pos lex -> token pos SymNEG lex }

  "%tokens"                          { \ pos lex -> token pos SymTOKENS lex }
  "%goal"                            { \ pos lex -> token pos SymGOAL lex }

  ([0-9][0-9]*)                      { \ pos lex -> token pos SymPOS lex }
  ([\[\]]|[A-Za-z][A-Za-z0-9_\-]*)   { \ pos lex -> token pos SymSYMBOL lex }

  . {\ (AlexPn _ ln ch) s ->
       error ("["++(show ln)++":"++(show ch)++"] Unexpected char '"++s++"'.\n") }

{
type Location = ((Int,Int),(Int,Int))

class Locate input where
  locate :: input -> Location

data Token
  = Token Location Symbol String

instance Locate Token where
  locate (Token loc _ _) = loc

instance Show Token where
  showsPrec _ (Token _ _ lex) = (lex++)

data Symbol
  = SymTOKENS
  | SymGOAL
  | SymARROW
  | SymCOMMA
  | SymSEMIC
  | SymLPAR
  | SymRPAR
  | SymDSJ
  | SymNEG
  | SymPOS
  | SymSYMBOL
  
inpLexer :: AlexPosn -> String -> [Token]
inpLexer _ = alexScanTokens

token :: AlexPosn -> Symbol -> String -> Token
token (AlexPn _ ln ch) sym lex = Token ((ln,ch),(ln,ch+(length lex)-1)) sym lex
}
