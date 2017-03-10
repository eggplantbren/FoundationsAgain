-- Module name
module ProbabilityDistribution where

-- Imports
import qualified Data.Vector.Unboxed as U

-- A type to represent a discrete probability distribution
-- over N atoms.
data ProbabilityDistribution = ProbabilityDistribution (U.Vector Double)
  deriving (Eq, Show)

-- Normalise a list
normaliseList :: [Double] -> [Double]
normaliseList xs = map (/ tot) xs where
  tot = sum xs

-- Smart constructor from a list of nonnegative doubles
makeFromList :: [Double] -> Maybe ProbabilityDistribution
makeFromList [] = Nothing
makeFromList ps
  | any (< 0.0) ps = Nothing
  | otherwise      = Just (ProbabilityDistribution ps') where
                       ps' = U.fromList (normaliseList ps)

-- Smart constructor of uniform distributions
makeUniform :: Int -> Maybe ProbabilityDistribution
makeUniform n
  | n <= 0    = Nothing
  | otherwise = Just (ProbabilityDistribution vec) where
                  vec = U.replicate n (1.0 / fromIntegral n)


