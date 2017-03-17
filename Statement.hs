-- Name of module
module Statement where

import Data.List (subsequences)

-- Represent a statement by a list of bools
newtype Statement = Statement [Bool]
    deriving (Eq, Read, Show)

-- All statements from n atoms
allStatements :: Int -> [Statement]
allStatements n = ss' where
    toStatement s = Statement [i `elem` s | i <- [0..(n-1)]]
    ss  = subsequences [0..(n-1)]
    ss' = map toStatement ss

-- Logical or
logicalOr :: Statement -> Statement -> Statement
logicalOr (Statement x) (Statement y) = Statement (zipWith (||) x y)

-- Logical and
logicalAnd :: Statement -> Statement -> Statement
logicalAnd (Statement x) (Statement y) = Statement (zipWith (&&) x y)

-- Implies
implies :: Statement -> Statement -> Bool
implies (Statement x) (Statement y) = all (== True) z where
    z     = zipWith f x y
    f a b = if a then b else True

-- Is implied by
isImpliedBy :: Statement -> Statement -> Bool
isImpliedBy x y = implies y x

---- A new probability distribution that conditions on a statement
--given :: P.ProbabilityDistribution
--      -> Statement
--      -> Maybe P.ProbabilityDistribution
--given (P.ProbabilityDistribution ps) (Statement is) =
--  P.makeFromList newPs where
--    n      = U.length ps
--    newP i = if (S.member i is) then ps U.! i else 0.0 
--    newPs  = map newP [0..(n - 1)]

