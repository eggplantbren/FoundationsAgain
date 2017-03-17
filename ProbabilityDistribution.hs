-- Module name
module ProbabilityDistribution where

-- A type to represent a discrete probability distribution
-- over N atoms.
data ProbabilityDistribution = ProbabilityDistribution [Double]
    deriving Show

-- Turns a list into a probability distribution
fromList :: [Double] -> ProbabilityDistribution
fromList xs = ProbabilityDistribution ps where
    ps  = map (/tot) xs
    tot = sum xs



