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
    deriving (Eq, Read, Show)

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

-- A new probability distribution that conditions on a statement
given :: P.ProbabilityDistribution
      -> Statement
      -> Maybe P.ProbabilityDistribution
given (P.ProbabilityDistribution ps) (Statement is) =
  P.makeFromList newPs where
    n      = U.length ps
    newP i = if (S.member i is) then ps U.! i else 0.0 
    newPs  = map newP [0..(n - 1)]

