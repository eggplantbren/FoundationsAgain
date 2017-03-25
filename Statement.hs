-- Don't export everything
module Statement (and, fromString, or, Statement) where

-- Imports
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import Prelude hiding (and, or)

-- Represent a statement using a vector of bools
-- (for which atoms are included)
data Statement = Statement (U.Vector Bool)

-- Show instance
instance Show Statement where
  show (Statement bools) = U.toList chars where
    chars = U.map (\b -> if b then '1' else '0') bools

-- Converts a char to a bool
charToBool :: Char -> Maybe Bool
charToBool '0' = Just False
charToBool '1' = Just True
charToBool _   = Nothing

---- Smart constructor of statements
fromString :: String -> Maybe Statement
fromString s =
  let
    sVector = U.fromList s                :: U.Vector Char
    bools   = U.mapM charToBool sVector   :: Maybe (U.Vector Bool)
  in
    fmap Statement bools

-- Number of atoms (i.e., length)
numAtoms :: Statement -> Int
numAtoms (Statement bs) = U.length bs

-- Logical or
or :: Statement -> Statement -> Maybe Statement
or sx@(Statement x) sy@(Statement y)
  | numAtoms sx /= numAtoms sy = Nothing
  | otherwise = Just (Statement z) where z = U.zipWith (||) x y

-- Logical and
and :: Statement -> Statement -> Maybe Statement
and sx@(Statement x) sy@(Statement y)
  | numAtoms sx /= numAtoms sy = Nothing
  | otherwise = Just (Statement z) where z = U.zipWith (&&) x y

