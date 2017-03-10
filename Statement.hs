-- Name of module
module Statement where

-- Imports
import Data.List (subsequences)
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as U
import qualified ProbabilityDistribution as P

-- Represent a statement by a set of integers
data Statement = Statement (S.Set Int)
    deriving (Eq, Show)

-- Make a statement from a list of ints
makeFromList :: [Int] -> Maybe Statement
makeFromList is
  | any (< 0) is = Nothing
  | otherwise    = Just (Statement is') where is' = S.fromList is

-- All statements from the atoms of a particular
-- probability distribution
allStatements :: P.ProbabilityDistribution -> [Statement]
allStatements (P.ProbabilityDistribution ps) = let
    n   = U.length ps
    ss  = subsequences [0..(n-1)]
    ss' = mapM makeFromList ss
  in
    fromMaybe (error "Error in allStatements.") ss'

