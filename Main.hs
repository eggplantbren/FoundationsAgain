-- Imports
import Data.Maybe
import qualified ProbabilityDistribution as P
import qualified Statement as S

-- Main action
main :: IO ()
main = do
  -- A probability distribution
  let p = fromMaybe (error "Failure in P.makeFromList")
                    (P.makeFromList [1.0, 2.0, 3.0])
  print p

  -- All statements that can be made from the atoms
  let ss = S.allStatements p
  print ss

