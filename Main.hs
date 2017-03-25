-- Imports
import Control.Monad
import Prelude hiding (and, or)
import Statement

-- Main action
main :: IO ()
main = do
  -- Create two statements
  let s1 = fromString "011"
  let s2 = fromString "101"
  print s1
  print s2

  -- Logical or
  let result = do
                 temp <- liftM2 or s1 s2
                 temp
  print result

  putStrLn ""
  print $ allStatements 4

  return ()

