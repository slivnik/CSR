module Main where

import System.Environment ( getArgs )

import Common
import InpAST
import InpParser ( readExtCSRFile )
import CSR
import TrieParser ( naiveParserDriver, limBJumpParserDriver )
import DFAParser ( dfaParserDriver )

-------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  let (filename,parser,mode) =
        case args of
          [filename] -> (filename,"all","full")
          [filename,parser] -> (filename,parser,"full")
          [filename,parser,mode] -> (filename,parser,mode)
          _ -> error ":-( Wrong number of arguments."
  (terms,goal,reductions) <- readExtCSRFile (filename)
  if mode=="full" then do
    putStr "ALL TERMINALS:\n\n"
    putStrLn (showsWithSep (' ':) (map (\ t -> (t++)) terms) "\n")
    putStr "REDUCTIONS:\n\n"
    putStrLn (showsWithSep ('\n':) (map (showsPrec 1) reductions) "\n")
  else return ()
  let (symbs,desReductions) = desugarReds (terms,goal,reductions)
  if mode=="full" then do
    putStr "ALL SYMBOLS:\n\n"
    putStrLn (showsWithSep (' ':) (map (\ s -> (s++)) symbs) "\n")
    putStr "DESUGARED REDUCTIONS:\n\n"
    putStrLn (showsWithSep ('\n':) (map (showsPrec 1) desReductions) "\n")
  else return ()
  csr <- toCSR (symbs,terms,goal,desReductions)
  -- putStrLn (showsPrec 1 csr "")
  input <- getContents
  if (parser `elem` ["all","naive"]) then naiveParserDriver 0 csr input else return ()
  if (parser `elem` ["all","bjump"]) then limBJumpParserDriver 0 csr input else return ()
  if (parser `elem` ["all","dfa"]) then dfaParserDriver 1 csr input else return ()
  return ()

-------------------------------------------------------------------------------
-- [EOF]
