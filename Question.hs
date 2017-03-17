-- Name of module
module Question where

-- Imports
import qualified Statement as S

data Question = Question [S.Statement]
    deriving (Eq, Show, Read)

downset :: S.Statement -> Question
downset statement@(S.Statement s) = Question ss where
    n  = length s
    ss = [ x | x <- S.allStatements n, x `S.implies` statement ]

