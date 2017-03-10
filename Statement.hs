-- Name of module
module Statement
       (makeFromList) where

-- Imports
import qualified Data.Set as S

-- Represent a statement by a set of integers
data Statement = Statement (S.Set Int)
    deriving (Eq, Show)

-- Make a statement from a list of ints
makeFromList :: [Int] -> Maybe Statement
makeFromList is
  | any (< 0) is = Nothing
  | otherwise    = Just (Statement is') where is' = S.fromList is


