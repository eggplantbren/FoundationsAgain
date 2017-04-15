module Statement where

-- Imports
import Data.List (subsequences)
import qualified Data.Set as S
import qualified Data.Vector as V
import Prelude hiding (and, or)

-- These two things (an id and the number of atoms)
-- are the defining features of a hypothesis space
data HypothesisSpace = HypothesisSpace
                       {
                           hypothesisSpaceID       :: String,
                           hypothesisSpaceNumAtoms :: Int
                       } deriving (Eq, Read, Show)

-- A type for statements. They are defined by
-- the hypothesis space they are 'in',
-- and which atoms are included.
data Statement = Statement
                 {
                     statementHypothesisSpace   :: HypothesisSpace,
                     statementIncludedAtoms     :: S.Set Int
                 } deriving (Eq, Read, Show)

-- A prettier way of showing a statement
render :: Statement -> String
render (Statement hs ias) =
  let
    member i = if S.member i ias then (1 :: Int) else (0 :: Int)
    bits     = map member [0..(hypothesisSpaceNumAtoms hs - 1)]
  in
    hypothesisSpaceID hs ++ mconcat (map show bits)


-- All statements in a hypothesis space with the
-- given label and number of atoms
allStatements :: HypothesisSpace -> V.Vector Statement
allStatements hypothesisSpace@(HypothesisSpace _ numAtoms) =
  let
    ss   = subsequences [0..(numAtoms-1)] :: [[Int]]
    incl = map S.fromList ss              :: [S.Set Int]
  in
    V.fromList $ map (Statement hypothesisSpace) incl



-- Define logical and
and :: Statement -> Statement -> Statement
and (Statement hs1 ias1) (Statement hs2 ias2)
  | hs1 /= hs2 = error "Not in same space."
  | otherwise  = Statement hs ias where
                   hs  = hs1
                   ias = S.intersection ias1 ias2

-- Define logical or
or :: Statement -> Statement -> Statement
or (Statement hs1 ias1) (Statement hs2 ias2)
  | hs1 /= hs2 = error "Not in same space."
  | otherwise  = Statement hs ias where
                   hs  = hs1
                   ias = S.union ias1 ias2

-- A demo that can be executed
demo :: IO ()
demo = do

  -- Create a whole Boolean lattice of statements!
  let hs = HypothesisSpace "x" 3
  let ss = allStatements hs

  let x = ss V.! 3
  let y = ss V.! 4
  putStrLn $ render x
  putStrLn $ render y
  putStrLn $ render (x `or` y)

