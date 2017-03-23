-- Imports
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
        s1' <- s1
        s2' <- s2
        s1' `or` s2'

  print result

  return ()

