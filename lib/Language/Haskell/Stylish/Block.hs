--------------------------------------------------------------------------------
module Language.Haskell.Stylish.Block
  ( Block(..)
  , LineBlock
  , realSrcSpanToLineBlock
  , SpanBlock
  , adjacent
  , merge
  , groupAdjacent
  ) where

--------------------------------------------------------------------------------
import qualified GHC.Types.SrcLoc as GHC

--------------------------------------------------------------------------------
-- | Indicates a line span
data Block a =
  Block
    { blockStart :: Int
    , blockEnd   :: Int
    }
  deriving (Eq, Ord, Show)

--------------------------------------------------------------------------------
instance Semigroup (Block a) where
  (<>) = merge

--------------------------------------------------------------------------------
type LineBlock = Block String

--------------------------------------------------------------------------------
type SpanBlock = Block Char

--------------------------------------------------------------------------------
realSrcSpanToLineBlock :: GHC.RealSrcSpan -> Block String
realSrcSpanToLineBlock s = Block (GHC.srcSpanStartLine s) (GHC.srcSpanEndLine s)

--------------------------------------------------------------------------------
adjacent :: Block a -> Block a -> Bool
adjacent b1 b2 = follows b1 b2 || follows b2 b1
  where
    follows (Block _ e1) (Block s2 _) = e1 == s2 || e1 + 1 == s2

--------------------------------------------------------------------------------
merge :: Block a -> Block a -> Block a
merge (Block s1 e1) (Block s2 e2) = Block (min s1 s2) (max e1 e2)

--------------------------------------------------------------------------------
-- | Groups adjacent blocks into larger blocks
groupAdjacent :: [(Block a, b)] -> [(Block a, [b])]
groupAdjacent = foldr go []
    -- This code is ugly and not optimal, and no fucks were given.
  where
    go (b1, x) gs =
      case break (adjacent b1 . fst) gs of
        (_, [])           -> (b1, [x]) : gs
        (ys, (b2, xs):zs) -> (merge b1 b2, x : xs) : (ys ++ zs)
