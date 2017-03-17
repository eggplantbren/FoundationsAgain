-- Imports
import qualified Question as Q
import qualified ProbabilityDistribution as P
import qualified Statement as S

-- Main action
main :: IO ()
main = do
  -- A probability distribution
  let p = P.fromList [1.0, 2.0, 3.0]
  print p
  putStrLn ""

  let ss = S.allStatements 3
  let q = Q.downset (ss !! 3)
  print (ss !! 3)
  putStrLn ""
  print q

  return ()

