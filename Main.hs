-- Imports
import Data.Maybe
import ProbabilityDistribution

-- Main action
main :: IO ()
main = do
  -- A probability distribution
  let p = fromMaybe (error "Failure in makeUniform")
                    (makeUniform 3)
  print p

