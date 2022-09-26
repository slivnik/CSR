module TrieParser where

import Data.List ( find )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromJust )
import GHC.Base ( maxInt )

import Common ( showsWithSep, (|>) )
import CSR
    ( CSR(csrReds, csrSymbs, csrBeg, csrEnd),
      Reduction(redDst, redSrc),
      Symbol(Symbol, symbNm),
      DstSymbol(New, Old, dstS),
      AST(ASTNode, ASTLeaf, astSymb, astSubtrees, astIndex) )

-------------------------------------------------------------------------------
-- TRIE:

data Trie key val
  = Trie {
      trieVal :: val,
      trieNxt :: Map key (Trie key val) }

instance (Show key,Show val) => Show (Trie key val) where
  showsPrec d (Trie val nxt) =
    (shows val).('\n':).
    (showsWithSep id
       [ (indent (d+2)).(shows s).(": "++).(showsPrec (d+2) trie)
       | (s,trie) <- Map.toList nxt ])
    where
      indent :: Int -> ShowS
      indent d = foldr (.) id (take d (repeat (' ':)))

-------------------------------------------------------------------------------
-- TRACES:

type Cell = (Bool,AST)

data Trace
  = TraceSF ([Cell],[Cell])
  | TraceACT (Int,Maybe (Int,Reduction))
  | TraceLOG String

instance Show Trace where
  showsPrec prec (TraceSF (pfxCs,sfxCs)) =
    ([ showsPrec prec ast
     | (_,ast) <- pfxCs ]
     |> showsWithSep (' ':)).
    (case (null pfxCs,null sfxCs) of
       (True,False) -> (". "++)
       (False,True) -> (" ."++)
       _ -> (" . "++)).
    ([ showsPrec prec ast
     | (_,ast) <- takeWhile fst sfxCs ]
     |> showsWithSep (' ':))
  showsPrec prec (TraceACT (cmps,Nothing)) =
    ("  "++).(" compares"++)
  showsPrec prec (TraceACT (cmps,Just (num,r))) =
    ("  "++).(shows cmps).(" compares"++).
    (" and applying "++).(shows num).(": "++).(shows r)
  showsPrec prec (TraceLOG log) =
    (log++)

-------------------------------------------------------------------------------
-- NAIVE PARSER:

type NaiveTrie = Trie Symbol (Maybe Reduction)

consNaiveTrie :: CSR -> NaiveTrie
consNaiveTrie csr =
  construct 0 Nothing (csrReds csr)
  where
    construct :: Int -> Maybe Reduction -> [Reduction] -> NaiveTrie
    construct depth fallback rs =
      Trie mbR next
      where
        mbR :: Maybe Reduction
        mbR =
          case filter ((==depth).length.redSrc) rs of
            []  -> fallback
            [r] -> Just r
            rs  -> error (":-( Multiple conflicting reductions:\n"++(shows rs []))
        next :: Map Symbol NaiveTrie
        next =
          [ (s,construct (depth+1) mbR sReds)
          | s <- csrSymbs csr,
            let sReds =
                  [ r
                  | r <- rs,
                    ((length.redSrc) r)>depth,
                    (redSrc r)!!depth==s ],
            (not.null) sReds ]
          |> Map.fromList

naiveParser :: NaiveTrie -> [Cell] -> [Trace]
naiveParser trie sfCs =
  (TraceSF ([],sfCs)):
  (parser 0 0 0 ([],sfCs))
  where
    parser :: Int -> Int -> Int
           -> ([Cell],[Cell])
           -> [Trace]
    parser num allCmps cmps (revPfxCs,[]) =
      (TraceSF (reverse revPfxCs,[])):
      (TraceACT (cmps,Nothing)):
      (TraceLOG ("\n"++(show (allCmps+cmps))++" total compares")):[]
    parser num allCmps cmps (revPfxCs,sfxCs) =
      case match trie sfxCs of
        (cmpsM,Just r,sfxCs') ->
          let pfxCs = reverse revPfxCs
              sfxCs'' = apply r sfxCs'
          in (TraceSF (pfxCs,sfxCs')):
             (TraceACT (cmps+cmpsM,Just (num+1,r))):
             (TraceSF (pfxCs,sfxCs'')):
             (TraceLOG ""):
             (TraceSF ([],pfxCs++sfxCs'')):
             (parser (num+1) (allCmps+cmpsM+cmps) 0 ([],pfxCs++sfxCs''))
        (cmpsM,Nothing,sfxCs') ->
          parser num allCmps (cmps+cmpsM) ((head sfxCs'):revPfxCs,tail sfxCs')
    match :: NaiveTrie -> [Cell] -> (Int,Maybe Reduction,[Cell])
    match (Trie mbR next) [] = (0,mbR,[])
    match (Trie mbR next) sfCs | Map.null next = (0,mbR,sfCs)
    match (Trie mbR next) ((seen,ast):sfCs) =
      case (Map.!?) next (astSymb ast) of
        Nothing -> (1,mbR,(True,ast):sfCs)
        Just sTrie ->
          let (cmps',mbR',sfCs') = match sTrie sfCs
          in (cmps'+1,mbR',(True,ast):sfCs')
    apply :: Reduction -> [Cell] -> [Cell]
    apply r sfCs =
      (++) [ case dstS of
               Old _ pos -> sfCs!!(pos-1)
               New s poss ->
                 (True,ASTNode s (map (\ pos -> snd (sfCs!!(pos-1))) poss))
           | dstS <- redDst r ]
           (drop (length (redSrc r)) sfCs)

naiveParserDriver :: (String,Int) -> CSR -> String -> IO ()
naiveParserDriver (mode,prec) csr input = do
  if mode=="full" then do
    putStrLn "NAIVE TRIE:\n"
    print trie
  else return ()
  putStrLn "NAIVE PARSER:\n"
  (naiveParser trie sfCs
   |> map (\ trace -> putStrLn (showsPrec prec trace ""))
   |> foldr (>>) (return ()))
  putStrLn ""
  where
    trie :: NaiveTrie
    trie = consNaiveTrie csr
    sfCs :: [Cell]
    sfCs = (words input)
           |> map (\ nm -> fromJust (find ((==nm).symbNm) (csrSymbs csr)))
           |> zip [1..]
           |> map (\ (i,s) -> (False,ASTLeaf s i))
           |> ((False,ASTNode (csrBeg csr) []):)
           |> (++[(False,ASTNode (csrEnd csr) [])])

-------------------------------------------------------------------------------
-- PARSER WITH LIMITED BACKJUMPING:

type LimBJumpTrie = Trie Symbol (Maybe (Reduction,Int))

consLimBJumpTrie :: CSR -> LimBJumpTrie
consLimBJumpTrie csr =
  construct 0 Nothing (csrReds csr)
  where
    construct :: Int -> Maybe (Reduction,Int) -> [Reduction] -> LimBJumpTrie
    construct depth fallback rs =
      Trie mbRBj next
      where
        mbRBj :: Maybe (Reduction,Int)
        mbRBj =
          case filter ((==depth).length.redSrc) rs of
            []  -> fallback
            [r] -> Just (r,minBackJump r)
            rs  -> error (":-( Multiple conflicting reductions:\n"++(shows rs []))
        next :: Map Symbol LimBJumpTrie
        next =
          [ (s,construct (depth+1) mbRBj sReds)
          | s <- csrSymbs csr,
            let sReds =
                  [ r
                  | r <- rs,
                    ((length.redSrc) r)>depth,
                    (redSrc r)!!depth==s ],
            (not.null) sReds ]
          |> Map.fromList
        minBackJump :: Reduction -> Int
        minBackJump r =
          [ d
          | r' <- csrReds csr,
            let fst = 1 - (length (redSrc r')),
            let lst = length (redDst r),
            d <- [fst..lst],
            match (zip [0..] ((map dstS.redDst) r))
                  (zip [d..] (redSrc r')) ]
          |> foldl min maxInt
          where
            match :: [(Int,Symbol)] -> [(Int,Symbol)] -> Bool
            match [] _ = True
            match _ [] = True
            match patt1@((d1,s1):_) patt2@((d2,s2):_) =
              case compare d1 d2 of
                LT -> match (tail patt1) patt2
                EQ -> if s1==s2 then match (tail patt1) (tail patt2) else False
                GT -> match patt1 (tail patt2)

limBJumpParser :: LimBJumpTrie -> [Cell] -> [Trace]
limBJumpParser trie sfCs =
  (TraceSF ([],sfCs)):
  (parser 0 0 0 ([],sfCs))
  where
    parser :: Int -> Int -> Int
           -> ([Cell],[Cell])
           -> [Trace]
    parser num allCmps cmps (revPfxCs,[]) =
      (TraceSF (reverse revPfxCs,[])):
      (TraceACT (cmps,Nothing)):
      (TraceLOG ("\n"++(show (allCmps+cmps))++" total compares")):[]
    parser num allCmps cmps (revPfxCs,sfxCs) =
      case match trie sfxCs of
        (cmpsM,Just (r,bj),sfxCs') ->
          let pfxCs = reverse revPfxCs
              sfxCs'' = apply r sfxCs'
              (revPfxCs''',sfxCs''') = move bj (revPfxCs,sfxCs'')
          in (TraceSF (pfxCs,sfxCs')):
             (TraceACT (cmps+cmpsM,Just (num+1,r))):
             (TraceSF (pfxCs,sfxCs'')):
             (TraceLOG ""):
             (TraceSF (reverse revPfxCs''',sfxCs''')):
             (parser (num+1) (allCmps+cmpsM+cmps) 0 (revPfxCs''',sfxCs'''))
        (cmpsM,Nothing,sfxCs') ->
          parser num allCmps (cmps+cmpsM) ((head sfxCs'):revPfxCs,tail sfxCs')
    match :: LimBJumpTrie -> [Cell] -> (Int,Maybe (Reduction,Int),[Cell])
    match (Trie mbRBj next) [] = (0,mbRBj,[])
    match (Trie mbRBj next) sfCs | Map.null next = (0,mbRBj,sfCs)
    match (Trie mbRBj next) ((seen,ast):sfCs) =
      case (Map.!?) next (astSymb ast) of
        Nothing -> (1,mbRBj,(True,ast):sfCs)
        Just sTrie ->
          let (cmps',mbRBj',sfCs') = match sTrie sfCs
          in (cmps'+1,mbRBj',(True,ast):sfCs')
    apply :: Reduction -> [Cell] -> [Cell]
    apply r sfCs =
      (++) [ case dstS of
               Old _ pos -> sfCs!!(pos-1)
               New s poss ->
                 (True,ASTNode s (map (\ pos -> snd (sfCs!!(pos-1))) poss))
           | dstS <- redDst r ]
           (drop (length (redSrc r)) sfCs)
    move :: Int -> ([Cell],[Cell]) -> ([Cell],[Cell])
    move bj (revPfxCs,sfxCs) =
      case compare bj 0 of
        EQ -> (revPfxCs,sfxCs)
        LT -> if null revPfxCs then (revPfxCs,sfxCs) else
                move (bj+1) (tail revPfxCs,(head revPfxCs):sfxCs)
        GT -> if null sfxCs then (revPfxCs,sfxCs) else
                ((head sfxCs):revPfxCs,tail sfxCs)

limBJumpParserDriver :: (String,Int) -> CSR -> String -> IO ()
limBJumpParserDriver (mode,prec) csr input = do
  if mode=="full" then do
    putStrLn "LIMITED BACKJUMP TRIE:\n"
    print trie
  else return ()
  putStrLn "LIMITED BACKJUMP PARSER:\n"
  (limBJumpParser trie sfCs
   |> map (\ trace -> putStrLn (showsPrec prec trace ""))
   |> foldr (>>) (return ()))
  putStrLn ""
  where
    trie :: LimBJumpTrie
    trie = consLimBJumpTrie csr
    sfCs :: [Cell]
    sfCs = (words input)
           |> map (\ nm -> fromJust (find ((==nm).symbNm) (csrSymbs csr)))
           |> zip [1..]
           |> map (\ (i,s) -> (False,ASTLeaf s i))
           |> ((False,ASTNode (csrBeg csr) []):)
           |> (++[(False,ASTNode (csrEnd csr) [])])

-------------------------------------------------------------------------------
-- [EOF]
