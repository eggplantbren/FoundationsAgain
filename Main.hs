-- Imports
import Control.Applicative (liftA2)
import Prelude hiding (and, or)
import Statement

-- Main action
main :: IO ()
main = do
  -- Create two statements
  let s1 = fromString "011" :: Maybe Statement
  let s2 = fromString "101" :: Maybe Statement

  -- I am SMRT
  print s1
  print s2
  print $ liftA2 or  s1 s2
  print $ liftA2 and s1 s2
  return ()

