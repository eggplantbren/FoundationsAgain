module Measure where

-- Imports
import Control.Monad.Primitive (RealWorld)
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Statement
import System.Random.MWC

-- A hypothesis space with associated measure
data Measure = Measure HypothesisSpace (U.Vector Double)
    deriving (Eq, Read, Show)



-- Generate a measure from an exponential distribution
generateMeasure :: HypothesisSpace
                -> Gen RealWorld
                -> IO Measure
generateMeasure h rng = do
  us <- U.replicateM (hypothesisSpaceNumAtoms h) (uniform rng)
  let transform = negate . log . (1.0 - )
  let ms = U.map transform us
  return $ Measure h ms


-- The measure of a statement
measureOf :: Measure -> Statement -> Double
measureOf (Measure h ms) (Statement h' ias)
  | h /= h'   = error "Not in same hypothesis space."
  | otherwise = sum values where
                  values = map ((U.!) ms) (S.toList ias)

-- A demo
demo :: IO ()
demo = withSystemRandom . asGenIO $ \rng -> do

  -- Create a whole Boolean lattice of statements!
  let h  = HypothesisSpace "x" 3
  let ss = allStatements h

  -- Put a measure on it
  m <- generateMeasure h rng
  print m
 
  let x = ss V.! 3
  putStrLn $ render x
  print $ measureOf m x

