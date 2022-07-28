module Common where

import GHC.IO (unsafePerformIO)

-------------------------------------------------------------------------------

(|>) :: a -> (a -> b) -> b
x |> f = f x

showsWithSep :: ShowS -> [ShowS] -> ShowS
showsWithSep sep [] = id
showsWithSep sep [s] = s
showsWithSep sep (s:ss) = s.sep.(showsWithSep sep ss)

debug :: Show a => a -> b -> b
debug a b = unsafePerformIO (print (show a) >> return b)

-------------------------------------------------------------------------------
-- [EOF]
