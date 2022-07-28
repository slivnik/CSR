module InpAST where

import Data.List ( nub )
import Data.Set ( Set )
import qualified Data.Set as Set

import Common ( showsWithSep, (|>) )

-------------------------------------------------------------------------------

data SrcSymbol
  = SrcAtm String
  | SrcDsj SrcSymbol SrcSymbol
  | SrcCnc SrcSymbol SrcSymbol
  | SrcNeg SrcSymbol
  deriving (Eq,Ord)

instance Show SrcSymbol where
  showsPrec _ (SrcAtm nm) = (nm++)
  showsPrec 1 dsj@(SrcDsj _ _) =
    ('(':).(showsPrec 0 dsj).(')':)
  showsPrec _ (SrcDsj src1 src2) =
    (shows src1).(" | "++).(shows src2)
  showsPrec 1 cnc@(SrcCnc _ _) =
    ('(':).(showsPrec 0 cnc).(')':)
  showsPrec _ (SrcCnc src1 src2) =
    (case src1 of
       SrcDsj _ _ -> ("("++).(shows src1).(")"++)
       _ -> (shows src1)).
    (' ':).
    (case src2 of
       SrcDsj _ _ -> ("("++).(shows src2).(")"++)
       _ -> (shows src2))
  showsPrec _ (SrcNeg src) =
    (case src of
       SrcAtm _ -> ("!"++).(shows src)
       _ -> ("!("++).(shows src).(") "++))
  showList srcs =
    showsWithSep (' ':) (map (showsPrec 1) srcs)

data DstSymbol
  = DstOld Int (Maybe SrcSymbol) 
  | DstNew String [Int]
  deriving (Eq,Ord)

instance Show DstSymbol where
  showsPrec _ (DstOld pos Nothing) = (shows pos)
  showsPrec _ (DstOld pos (Just src)) = (shows pos).
    ('(':).(shows src).(')':)
  showsPrec _ (DstNew nm []) = (nm++)
  showsPrec _ (DstNew nm poss) = (nm++).
    ('(':).(showsWithSep (',':) (map shows poss)).(')':)
  showList dsts =
    showsWithSep (' ':) (map shows dsts)

data Reduction
  = Reduction Int [SrcSymbol] [DstSymbol] (Maybe Reduction)

instance Show Reduction where
  showsPrec 1 red@(Reduction id src dsts Nothing) =
    (showsPrec 0 red)
  showsPrec 1 red@(Reduction id src dsts (Just origRed)) =
    (showsPrec 0 red).('\n':).
    ("  from "++).(showsPrec 1 origRed)
  showsPrec _ (Reduction id src dsts _) =
    (shows id).(": "++).(shows src).(" --> "++).(shows dsts)
  showList reds =
    showsWithSep ('\n':) (map shows reds)

-------------------------------------------------------------------------------

srcNms :: SrcSymbol -> ([String] -> [String])
srcNms (SrcAtm nm) = (nm:)
srcNms (SrcDsj src1 src2) = (srcNms src1).(srcNms src2)
srcNms (SrcCnc src1 src2) = (srcNms src1).(srcNms src2)
srcNms (SrcNeg src) = (srcNms src)

dstNms :: DstSymbol -> ([String] -> [String])
dstNms (DstOld _ Nothing) = id
dstNms (DstOld _ (Just src)) = srcNms src
dstNms (DstNew nm _) = (nm:)

checkBegSymb :: Reduction -> Bool
checkBegSymb r@(Reduction _ srcs dsts _) =
  case (head srcs,head dsts) of
    (SrcAtm "[",DstOld pos _) ->
      ((foldr (.) id (map srcNms srcs)).
       (foldr (.) id (map dstNms (drop 1 dsts)))) []
      |> filter (=="[")
      |> ((==1).length)
      |> (&& (pos==1))
    (SrcAtm "[",DstNew "[" _) ->
      ((foldr (.) id (map srcNms srcs)).(foldr (.) id (map dstNms dsts))) []
      |> filter (=="[")
      |> ((==2).length)
    _ ->
      ((foldr (.) id (map srcNms srcs)).(foldr (.) id (map dstNms dsts))) []
      |> filter (=="[")
      |> null

checkEndSymb :: Reduction -> Bool
checkEndSymb (Reduction _ srcs dsts _) =
  case (last srcs,last dsts) of
    (SrcAtm "]",DstOld pos _) ->
      ((foldr (.) id (map srcNms srcs)).
       (foldr (.) id (map dstNms (take ((length dsts)-1) dsts)))) []
      |> filter (=="]")
      |> ((==2).length)
      |> (&& (pos==(length srcs)))
    (SrcAtm "]",DstNew "]" _) ->
      ((foldr (.) id (map srcNms srcs)).(foldr (.) id (map dstNms dsts))) []
      |> filter (=="]")
      |> ((==2).length)
    _ ->
      ((foldr (.) id (map srcNms srcs)).(foldr (.) id (map dstNms dsts))) []
      |> filter (=="]")
      |> null

desugarSrcSymb :: [String] -> SrcSymbol -> Set [SrcSymbol]
desugarSrcSymb allSymbs s@(SrcAtm nm) = Set.singleton [s]
desugarSrcSymb allSymbs (SrcDsj src1 src2) =
  Set.union (desugarSrcSymb allSymbs src1)
            (desugarSrcSymb allSymbs src2)
desugarSrcSymb allSymbs (SrcCnc src1 src2) =
  [ ss1++ss2
  | ss1 <- Set.toList (desugarSrcSymb allSymbs src1),
    ss2 <- Set.toList (desugarSrcSymb allSymbs src2) ]
  |> Set.fromList
desugarSrcSymb allSymbs (SrcNeg (SrcAtm nm)) =
  [ [SrcAtm s] | s <- allSymbs, s/=nm ]
  |> Set.fromList
desugarSrcSymb allSymbs (SrcNeg (SrcDsj src1 src2)) =
  Set.intersection
    (desugarSrcSymb allSymbs (SrcNeg src1))
    (desugarSrcSymb allSymbs (SrcNeg src2))
desugarSrcSymb allSymbs (SrcNeg (SrcCnc src1 src2)) =
  Set.union
    desNegs1
    ([ desSrc1++desNeg2
     | desSrc1 <- Set.toList desSrcs1, desNeg2 <- Set.toList desNegs2 ]
     |> Set.fromList)
  where
    desSrcs1 = desugarSrcSymb allSymbs src1
    desNegs1 = desugarSrcSymb allSymbs (SrcNeg src1)
    desNegs2 = desugarSrcSymb allSymbs (SrcNeg src2)
desugarSrcSymb allSymbs (SrcNeg (SrcNeg src)) =
  desugarSrcSymb allSymbs src

desugarRed :: [String] -> Reduction -> [Reduction]
desugarRed allSymbs red@(Reduction rId srcs dsts _) =
  [ Reduction rId desSrcs desDsts (Just red)
  | (desSrcs,desDsts) <- desugar 1 srcs dsts ]
  where
    desugar :: Int -> [SrcSymbol] -> [DstSymbol]
            -> [([SrcSymbol],[DstSymbol])]
    desugar _ [] dsts = [([],dsts)]
    desugar i (src:srcs) dsts =
      [ [ ((desSrc++desSrcs,dsts):)
        | (desSrcs,dsts) <- desugar (i+desSrcLen) srcs dsts' ]
        |> foldr (.) id
      | desSrc <- Set.toList (desugarSrcSymb allSymbs src),
        let desSrcLen = length desSrc,
        let dsts' = expand (i,desSrcLen) desSrc dsts ]
      |> foldr (.) id
      |> ($ [])
    expand :: (Int,Int) -> [SrcSymbol] -> [DstSymbol] -> [DstSymbol]
    expand _ _ [] = []
    expand (i,len) desSrcs ((DstOld pos mbSrc):dsts) =
      case compare pos i of
        LT -> (DstOld pos mbSrc):(expand (i,len) desSrcs dsts)
        EQ -> [ ((DstOld pos (Just desSrc)):)
              | (pos,desSrc) <- zip [i..] desSrcs ]
              |> foldr (.) id 
              |> ($ (expand (i,len) desSrcs dsts))
        GT -> (DstOld (pos+len-1) mbSrc):(expand (i,len) desSrcs dsts)
    expand (i,len) srcs ((DstNew nm poss):dsts) =
      [ case compare pos i of
          LT -> [pos]
          EQ -> [(pos+0)..(pos+(len-1))]
          GT -> [pos+len-1]
      | pos <- poss ]
      |> foldr (++) []
      |> DstNew nm
      |> (:(expand (i,len) srcs dsts))

desugarReds :: ([String],String,[Reduction])
            -> ([String],[Reduction])
desugarReds (allTerms,goal,reductions) =
  (allSymbs,
   [ Reduction rId srcs dsts orig
   | (rId,Reduction _ srcs dsts orig) <-
       reductions
       |> map (desugarRed allSymbs)
       |> concat
       |> filter checkBegSymb
       |> filter checkEndSymb
       |> zip [1..] ])
  where
    allSymbs :: [String]
    allSymbs =
      (("[":).("]":).(goal:).
      ([ ([ srcNms src | src <- srcs ]
           |> foldr (.) id).
         ([ dstNms dst | dst <- dsts ]
          |> foldr (.) id)
       | (Reduction _ srcs dsts _) <- reductions ]
       |> foldr (.) id))
      allTerms
      |> nub

-------------------------------------------------------------------------------
-- [EOF]
