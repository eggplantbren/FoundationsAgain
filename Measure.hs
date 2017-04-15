module Measure where

-- Imports
import Control.Monad.Primitive (RealWorld)
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

-- A demo
demo :: IO ()
demo = withSystemRandom . asGenIO $ \rng -> do
  let h = HypothesisSpace "x" 3
  m <- generateMeasure h rng
  print m


