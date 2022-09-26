module DFAParser where

import Data.List ( find, sort )
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromJust )
import Data.Set ( Set )
import qualified Data.Set as Set

import Common ( showsWithSep, (|>) )
import CSR
    ( CSR(csrReds, csrBeg, csrSymbs, csrEnd),
      Reduction(redSrc, redDst),
      Symbol(symbNm),
      DstSymbol(New, Old),
      AST(ASTNode, ASTLeaf, astSymb) )

-------------------------------------------------------------------------------
-- DFA:

type Item = (Reduction,Int)

data State
  = State {
      stId :: Int,
      stItems :: Set Item,
      stApp :: Maybe Item,
      stGoto :: Map Symbol State }

instance Eq State where
  st1==st2 = (stId st1)==(stId st2)
instance Ord State where
  compare st1 st2 = compare (stId st1) (stId st2)

instance Show State where
  showsPrec 0 st =
    ("state "++).(shows (stId st)).(":\n"++).
    ("  items:\n"++).
    ([ ("    "++).
       (showsWithSep (' ':) ((map shows.take d.redSrc) r)).
       (if d==0 then (". "++) else
        if d==(length (redSrc r)) then (" ."++) else
        (" . "++)).
       (showsWithSep (' ':) ((map shows.drop d.redSrc) r)).
       (" --> "++).
       (showsWithSep (' ':) (map shows (redDst r))).
       ('\n':)
     | (r,d) <- Set.toList (stItems st)]
     |> foldr (.) id).
    ("  apply: "++).
    (case stApp st of
       Nothing -> id
       Just (r,d) -> (shows r).(" at "++).(shows d)).
    ('\n':).
    ("  goto: "++).
    ([ (shows (s,stId st))
     | (s,st) <- Map.toList (stGoto st) ]
     |> foldr (.) id).('\n':)
  showsPrec _ st = (shows (stId st))

data DFA
  = DFA {
      dfaInitSt :: State,
      dfaStates :: [State] }

instance Show DFA where
  showsPrec _ dfa = showsWithSep ('\n':) (map shows (sort (dfaStates dfa)))

-------------------------------------------------------------------------------
-- TRACES:

data Cell
  = CodeCell (Maybe State)
  | DataCell AST

instance Show Cell where
  showsPrec prec (CodeCell Nothing) = ('-':)
  showsPrec prec (CodeCell (Just st)) = shows (stId st)
  showsPrec prec (DataCell ast) = showsPrec prec ast

data Trace cell
  = TraceSF ([(Bool,cell)],[(Bool,cell)])
  | TraceACT (Int,Maybe (Int,Reduction))
  | TraceLOG String

instance Show cell => Show (Trace cell) where
  showsPrec prec (TraceSF (pfxCs,sfxCs)) =
    (pfxCs
     |> map (showsPrec prec.snd)
     |> showsWithSep (' ':)).
    (case (null pfxCs,null sfxCs) of
       (True,False) -> (". "++)
       (False,True) -> (" ."++)
       _ -> (" . "++)).
    (takeWhile fst sfxCs
     |> map (showsPrec prec.snd)
     |> showsWithSep (' ':))
  showsPrec prec (TraceACT (cmps,Nothing)) =
    ("  "++).(shows cmps).(" compares"++)
  showsPrec prec (TraceACT (cmps,Just (num,r))) =
    ("  "++).(shows cmps).(" compares"++).
    (" and applying "++).(shows num).(": "++).(shows r)
  showsPrec prec (TraceLOG log) =
    (log++)

-------------------------------------------------------------------------------
-- DFA BASED PARSER:

type ProtoState = (Set Item,Maybe Item)

consDFA :: CSR -> DFA
consDFA csr =
  DFA {
    dfaInitSt = (Map.!) allSts initProtoSt,
    dfaStates = Map.elems allSts }
  where
    initProtoSt :: ProtoState
    initProtoSt = (Set.fromList [ (r,0) | r <- csrReds csr ],Nothing)
    allSts :: Map ProtoState State
    allSts = consStates [initProtoSt] Map.empty
    consStates :: [ProtoState]         -- states yet to be constructed
               -> Map ProtoState State -- already constructed states
               -> Map ProtoState State -- all states
    consStates [] sts = sts
    consStates (protoSt@(protoItems,protoApp):protoSts) sts =
      case Map.lookup protoSt sts of
        Just st -> consStates protoSts sts
        Nothing -> Map.insert protoSt st sts
                   |> consStates (map snd newProtoStsByS)
                   |> consStates protoSts
      where
        st :: State
        st =
          State {
            stId = Map.size sts,
            stItems = items,
            stApp = app,
            stGoto =
              [ (s,(Map.!) allSts protoSt)
              | (s,protoSt) <- newProtoStsByS ] 
              |> Map.fromList }
        app :: Maybe Item
        app =
          foldl select 
            protoApp
            [ if (length (redSrc r))==d then Just (r,d) else Nothing
            | (r,d) <- Set.toList protoItems ]
          where
            select :: Maybe Item -> Maybe Item -> Maybe Item
            select Nothing mbItem = mbItem
            select mbItem Nothing = mbItem
            select mbItem1@(Just (r1,d1)) mbItem2@(Just (r2,d2)) =
              case compare d1 d2 of
                LT -> mbItem2 ; GT -> mbItem1
                EQ -> case compare (length (redSrc r1)) (length (redSrc r2)) of
                        LT -> mbItem2 ; GT -> mbItem1
                        EQ -> error ":-( [INTERNAL ERROR]"
        items :: Set Item
        items =
          (Set.fromList [ (r,0) | r <- csrReds csr ])
          |> Set.union protoItems
          |> Set.filter (keepBy app)
          where
            keepBy :: Maybe Item -> Item -> Bool
            keepBy Nothing item = True
            keepBy (Just (appR,appD)) (r,d) =
              (d>appD)||((d==appD)&&((length (redSrc r))>(length (redSrc appR))))
        newProtoStsByS :: [(Symbol,ProtoState)]
        newProtoStsByS =
          if null items then [] else
          [ (s,
             ([ (r,d+1)
              | (r,d) <- Set.toList items,
                d<(length (redSrc r)),
                ((redSrc r)!!d)==s ]
              |> Set.fromList,
              protoApp))
          | s <- csrSymbs csr ]
          |> filter (\ (_,(protoItems,_)) -> not (null protoItems))
          where
            protoApp :: Maybe Item
            protoApp = case app of
                         Nothing -> Nothing 
                         Just (r,d) -> Just (r,d+1)

dfaParser :: DFA -> [(Bool,Cell)] -> [Trace Cell]
dfaParser dfa sfCs =
  (TraceSF (initPfx,initSfx)):
  (TraceLOG ""):
  parser 0 0 0 Nothing (initPfx,initSfx)
  where
    initPfx :: [(Bool,Cell)]
    initSfx :: [(Bool,Cell)]
    initPfx = [(True,CodeCell (Just (dfaInitSt dfa)))]
    initSfx = sfCs
    parser :: Int -> Int -> Int
           -> Maybe Item
           -> ([(Bool,Cell)],[(Bool,Cell)])
           -> [Trace Cell]
    parser num allCmps cmps Nothing
           sf@(revPfxCs@((True,CodeCell (Just st)):_),[]) =
      case stApp st of
        Just (r,d) ->
          (revPfxCs,[])
          |> parser num allCmps cmps (Just (r,d))
        Nothing ->
          (TraceSF (reverse revPfxCs,[])):
          (TraceACT (cmps,Nothing)):
          (TraceLOG ("\n"++(show (allCmps+cmps))++" total compares")):[]
    parser num allCmps cmps Nothing
           sf@(revPfxCs@((True,CodeCell (Just st)):_),sfxCs@((_,DataCell ast):_)) =
      if Map.null (stGoto st) then
        case stApp st of
          Just (r,d) ->
            (revPfxCs,sfxCs)
            |> parser num allCmps cmps (Just (r,d))
          Nothing -> error ":-( [INTERNAL ERROR]"
      else
        case (Map.!?) (stGoto st) (astSymb ast) of
          Just st' ->
            ((True,CodeCell (Just st')):(True,DataCell ast):revPfxCs
            ,drop 2 sfxCs)
            |> parser num allCmps (cmps+1) Nothing 
          Nothing ->
            case stApp st of
              Just (r,d) ->
                (revPfxCs,(True,DataCell ast):(drop 1 sfxCs))
                |> parser num allCmps (cmps+1) (Just (r,d))
              Nothing ->
                ((True,CodeCell (Just (dfaInitSt dfa))):(True,DataCell ast):revPfxCs,
                 drop 2 sfxCs)
                |> parser num allCmps (cmps+1) Nothing
    parser num allCmps cmps (Just (r,d)) (revPfxCs,sfxCs) =
      (TraceSF (reverse revPfxCs,sfxCs)):
      (TraceACT (cmps,Just (num+1,r))):
      (TraceSF (reverse revPfxCs',sfxCs')):
      (TraceLOG ""):
      (parser (num+1) (allCmps+cmps) 0 Nothing (revPfxCs',sfxCs'))
      where
        len = (length (redSrc r)) :: Int
        src = ((reverse.drop (2*(d-len)).take (2*d)) revPfxCs) :: [(Bool,Cell)]
        rem = ((reverse.take (2*(d-len))) revPfxCs) :: [(Bool,Cell)]
        revPfxCs' :: [(Bool,Cell)]
        revPfxCs' = drop (2*d) revPfxCs
        sfxCs' :: [(Bool,Cell)]
        sfxCs' =
          [ (case dstS of
               Old s pos -> ((True,snd (src!!(2*(pos-1)))):)
               New s poss ->
                 ((True,
                   [ case snd (src!!(2*(pos-1))) of
                       DataCell ast -> ast
                       CodeCell _ -> error ":-( [INTERNAL ERROR]"
                   | pos <- poss ]
                   |> ASTNode s
                   |> DataCell):)).
                 ((True,CodeCell Nothing):)
          | dstS <- redDst r ]
          |> foldr (.) id
          |> ($ (rem++sfxCs))
    parser _ _ _ _ (pfx,sfx) = error ("\n\n"++(show (reverse pfx))++"\n"++(show (take 10 sfx)++"\n\n")) -- ":-( [INTERNAL ERROR]"

dfaParserDriver :: (String,Int) -> CSR -> String -> IO ()
dfaParserDriver (mode,prec) csr input = do
  if mode=="full" then do
    putStrLn "DFA:\n"
    print dfa
  else return ()
  putStrLn "DFA PARSER:\n"
  (dfaParser dfa sfCs
   |> map (\ trace -> putStrLn (showsPrec prec trace ""))
   |> foldr (>>) (return ()))
  putStrLn ""
  where
    dfa :: DFA
    dfa = consDFA csr
    sfCs :: [(Bool,Cell)]
    sfCs =
      (False,DataCell (ASTNode (csrBeg csr) [])):
      (False,CodeCell Nothing):
      ([ ((False,DataCell leaf):).
         ((False,CodeCell Nothing):)
       | leaf <- (words input)
                 |> map (\ nm -> fromJust (find ((==nm).symbNm) (csrSymbs csr)))
                 |> zip [1..]
                 |> map (\ (i,s) -> ASTLeaf s i) ]
       |> foldr (.) id
       |> ($ [(False,DataCell (ASTNode (csrEnd csr) [])),
              (False,CodeCell Nothing)]))

-------------------------------------------------------------------------------
-- [EOF]
