-- Imports
import Data.Maybe
import ProbabilityDistribution

-- Main action
main :: IO ()
main = do
  -- A probability distribution
  let p = fromMaybe (error "Failure in makeUniform")
                    (makeFromList [1.0, 2.0, 3.0])
  print p

