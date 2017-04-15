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


-- Hypothesis space product
product :: HypothesisSpace -> HypothesisSpace -> HypothesisSpace
product (HypothesisSpace id1 na1) (HypothesisSpace id2 na2) =
  let
    newId = id1 ++ id2
    na    = na1*na2
  in
    HypothesisSpace newId na

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
and (Statement h1 ias1) (Statement h2 ias2)
  | h1 /= h2   = error "Not in same space."
  | otherwise  = Statement h ias where
                   h   = h1
                   ias = S.intersection ias1 ias2

-- Define logical or
or :: Statement -> Statement -> Statement
or (Statement h1 ias1) (Statement h2 ias2)
  | h1 /= h2  = error "Not in same space."
  | otherwise = Statement h ias where
                  h   = h1
                  ias = S.union ias1 ias2

-- A demo that can be executed
demo :: IO ()
demo = do

  -- Create a whole Boolean lattice of statements!
  let h  = HypothesisSpace "x" 3
  let ss = allStatements h

  let x = ss V.! 3
  let y = ss V.! 4
  putStrLn $ render x
  putStrLn $ render y
  putStrLn $ render (x `or` y)

