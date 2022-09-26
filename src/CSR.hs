module CSR where

import Data.List ( find, nub )

import Common ( showsWithSep, (|>) )
import qualified InpAST as AST

-------------------------------------------------------------------------------
-- SYMBOLS:

data Symbol
  = Symbol {
      symbId :: Int,
      symbNm :: String }

instance Eq Symbol where
  s1==s2 = (symbId s1)==(symbId s2)

instance Ord Symbol where
  compare s1 s2 = compare (symbId s1) (symbId s2)

instance Show Symbol where
  showsPrec _ s = ((symbNm s)++)
  showList ss = showsWithSep (' ':) (map shows ss)

-------------------------------------------------------------------------------
-- PRODUCTIONS:

data DstSymbol
  = Old {
      dstS :: Symbol,
      dstSrc :: Int }
  | New {
      dstS :: Symbol,
      dstSrcs :: [Int] }

instance Eq DstSymbol where
  s1==s2 = (dstS s1)==(dstS s2)
instance Ord DstSymbol where
  compare s1 s2 = compare (dstS s1) (dstS s2)

instance Show DstSymbol where
  showsPrec _ s = shows (dstS s)
  showList ss =
    showsWithSep (' ':) (map shows ss)

data Reduction
  = Reduction {
      redId :: Int,
      redSrc :: [Symbol],
      redDst :: [DstSymbol],
      redOrig :: AST.Reduction }

instance Eq Reduction where
  r1==r2 = (redId r1)==(redId r2)
instance Ord Reduction where
  compare r1 r2 = compare (redId r1) (redId r2)

instance Show Reduction where
  showsPrec 1 r =
    (showsPrec 0 r).('\n':).
    ("  from "++).(showsPrec 1 (redOrig r))
  showsPrec _ r =
    ('(':).(shows (redId r)).(") "++).
    (shows (redSrc r)).(" --> "++).(shows (redDst r))
  showList rs = showsWithSep ('\n':) (map shows rs)

-------------------------------------------------------------------------------
-- CONTEXT-SENSITIVE REDUCTION SYSTEM:

data CSR
  = CSR {
      csrSymbs :: [Symbol],
      csrTerms :: [Symbol],
      csrReds :: [Reduction],
      csrBeg :: Symbol,
      csrEnd :: Symbol,
      csrGoal :: Symbol }

instance Show CSR where
  showsPrec prec csr =
    ("CONTEXT-SENSITIVE REDUCTION SYSTEM:\n"++).
    ("\nsymbs: "++).(showsWithSep (' ':) (map shows (csrSymbs csr))).
    ("\nterms: "++).(showsWithSep (' ':) (map shows (csrTerms csr))).
    ("\ngoal:  "++).(shows (csrGoal csr)).
    ("\nreductions:\n"++).
      (showsWithSep ('\n':) (map (showsPrec prec) (csrReds csr))).
    ("\n"++)

toCSR :: ([String],[String],String,[AST.Reduction]) -> IO CSR.CSR
toCSR (allSymbNms,allTermNms,goalNm,reductions) = do
  if "[" `elem` allTermNms then error ":-( '[' should not be on the token list." else return ()
  if "]" `elem` allTermNms then error ":-( ']' should not be on the token list." else return ()
  if goalNm `elem` allTermNms then error (":-( Goal '"++goalNm++"' should not be on the token list.") else return ()
  (CSR {
     csrSymbs = csrSymbs,
     csrTerms = csrTerms,
     csrReds = csrReductions,
     csrBeg = findSymbByNm "[",
     csrEnd = findSymbByNm "]",
     csrGoal = findSymbByNm goalNm }
   |> return)
  where
    csrSymbs :: [Symbol]
    csrSymbs =
      [ Symbol sId nm
      | (sId,nm) <- zip [0..] (nub allSymbNms) ]        
    csrTerms :: [Symbol]
    csrTerms =
      [ s | s <- csrSymbs, (symbNm s) `elem` ("[":"]":allTermNms)]
    csrReductions :: [Reduction]
    csrReductions =
      [ let csrSrcs :: [Symbol]
            csrSrcs =
              [ case src of
                  AST.SrcAtm nm -> findSymbByNm nm
                  _ -> error ":-( [INTERNAL ERROR]"
              | src <- srcs ]
            csrDsts :: [DstSymbol]
            csrDsts =
              [ case dst of
                  AST.DstOld pos _ -> CSR.Old (csrSrcs!!(pos-1)) pos
                  AST.DstNew nm poss -> CSR.New (findSymbByNm nm) poss
              | dst <- dsts ]
        in Reduction {
             redId = rId,
             redSrc = csrSrcs,
             redDst = csrDsts,
             redOrig = r }
      | r@(AST.Reduction rId srcs dsts _) <- reductions ]
    findSymbByNm :: String -> CSR.Symbol
    findSymbByNm nm =
      case find ((==nm).CSR.symbNm) csrSymbs of
        Just s -> s
        Nothing -> error ":-( [INTERNAL ERROR]"

-------------------------------------------------------------------------------
-- ABSTRACT SYNTAX TREES:

data AST
  = ASTNode {
      astSymb :: Symbol,
      astSubtrees :: [AST] }
  | ASTLeaf {
      astSymb :: Symbol,
      astIndex :: Int }

instance Show AST where
  showsPrec 1 ast@(ASTNode (Symbol _ nm) subtrees) =
    (showsPrec 0 ast).
    (if null subtrees then id else
     ('(':).(showsWithSep (',':) (map (showsPrec 1) subtrees)).(')':))
  showsPrec 1 ast@(ASTLeaf (Symbol _ nm) index) =
    (showsPrec 0 ast).
    ('(':).(shows index).(')':)
  showsPrec _ ast = (((symbNm.astSymb) ast)++)

-------------------------------------------------------------------------------
-- [EOF]
