{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | Utility functions for working with the GHC AST
module Language.Haskell.Stylish.GHC
  ( baseDynFlags
    -- * Outputable operators
  , showOutputable
    -- * Deconstruction
  , epAnnComments
  , deepAnnComments
  ) where

--------------------------------------------------------------------------------
import           Data.Generics                                       (Data,
                                                                      Typeable,
                                                                      everything,
                                                                      mkQ)
import           Data.List                                           (sortOn)
import qualified GHC.Driver.Ppr                                      as GHC (showPpr)
import           GHC.Driver.Session                                  (defaultDynFlags)
import qualified GHC.Driver.Session                                  as GHC
import qualified GHC.Hs                                              as GHC
import           GHC.Types.SrcLoc                                    (GenLocated (..),
                                                                      Located,
                                                                      RealLocated,
                                                                      RealSrcSpan,
                                                                      SrcSpan (..),
                                                                      srcSpanEndLine,
                                                                      srcSpanStartLine)
import qualified GHC.Types.SrcLoc                                    as GHC
import qualified GHC.Utils.Outputable                                as GHC
import qualified Language.Haskell.GhclibParserEx.GHC.Settings.Config as GHCEx

baseDynFlags :: GHC.DynFlags
baseDynFlags = defaultDynFlags GHCEx.fakeSettings GHCEx.fakeLlvmConfig

showOutputable :: GHC.Outputable a => a -> String
showOutputable = GHC.showPpr baseDynFlags

epAnnComments :: GHC.EpAnn a -> [GHC.LEpaComment]
epAnnComments GHC.EpAnnNotUsed = []
epAnnComments GHC.EpAnn {..}   = priorAndFollowing comments

deepAnnComments :: (Data a, Typeable a) => a -> [GHC.LEpaComment]
deepAnnComments = everything (++) (mkQ [] priorAndFollowing)

priorAndFollowing :: GHC.EpAnnComments -> [GHC.LEpaComment]
priorAndFollowing =
  sortOn (GHC.anchor . GHC.getLoc) . \case
    GHC.EpaComments {..}         -> priorComments
    GHC.EpaCommentsBalanced {..} -> priorComments ++ followingComments
