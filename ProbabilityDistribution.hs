-- Module name
module ProbabilityDistribution
       (makeUniform) where

-- Imports
import Data.Vector.Unboxed as U

-- A type to represent a discrete probability distribution
-- over N atoms.
data ProbabilityDistribution = ProbabilityDistribution (U.Vector Double)
  deriving (Eq, Show)

-- Smart constructor of uniform distributions
makeUniform :: Int -> Maybe ProbabilityDistribution
makeUniform n
  | n <= 0    = Nothing
  | otherwise = Just (ProbabilityDistribution vec) where
                  vec = U.replicate n (1.0 / fromIntegral n)

