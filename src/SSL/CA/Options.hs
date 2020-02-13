{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UnicodeSyntax         #-}

module SSL.CA.Options
  ( Options( Options ), optsParse, topDir )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Maybe     ( Maybe )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Monoid.Unicode  ( (⊕) )

-- fluffy ------------------------------

import Fluffy.Nat  ( One )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.IO                ( pResolve )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Monad        ( (≫) )
import Data.MoreUnicode.Monoid       ( ф )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- optparse-applicative ----------------

import Options.Applicative  ( ArgumentFields, Mod, Parser
                            , action, metavar, strArgument )

-- optparse-plus -----------------------

import OptParsePlus  ( parseOpts )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun   ( DryRunLevel, HasDryRunLevel( dryRunLevel )
                                  , dryRunP )
import ProcLib.CommonOpt.Verbose  ( HasVerboseLevel( verboseLevel )
                                  , VerboseLevel, verboseP )

-- text --------------------------------

import Data.Text  ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

type family ResolvesTo α
class Resolvable α where
  resolve ∷ (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
            α → μ (ResolvesTo α)

type instance ResolvesTo Text = AbsDir
instance Resolvable Text where
  resolve ∷ (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
            Text → μ AbsDir
  resolve t = pResolve t

------------------------------------------------------------

data Options' = Options' { _dryRunL'  ∷ DryRunLevel  One
                         , _verboseL' ∷ VerboseLevel One
                         , _topDir'   ∷ Text
                         }
  deriving Show

parseOptions' ∷ Parser Options'
parseOptions' = Options' ⊳ dryRunP ⊵ verboseP ⊵ parseTopDir ф

parseTopDir ∷ Mod ArgumentFields Text → Parser Text
parseTopDir ms = strArgument (action "directory" ⊕ metavar "DIR" ⊕ ms)

------------------------------------------------------------

data Options = Options { _dryRunL  ∷ DryRunLevel  One
                       , _verboseL ∷ VerboseLevel One
                       , _topDir   ∷ AbsDir
                       }
  deriving Show

instance HasVerboseLevel One Options where
  verboseLevel = verboseL

instance HasDryRunLevel One Options where
  dryRunLevel = dryRunL  

type instance ResolvesTo Options' = Options
instance Resolvable Options' where
  resolve ∷ (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
            Options' → μ Options
  resolve (Options' d v t') = do
    t ← resolve t'
    return (Options d v t)

dryRunL ∷ Lens' Options (DryRunLevel One)
dryRunL = lens _dryRunL (\ o d → o { _dryRunL = d })

verboseL ∷ Lens' Options (VerboseLevel One)
verboseL = lens _verboseL (\ o v → o { _verboseL = v })

topDir ∷ Lens' Options AbsDir
topDir = lens _topDir (\ o d → o { _topDir = d })

optsParse ∷ (MonadIO μ, AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
            Maybe Text → Text → μ Options
optsParse progn descn = parseOpts progn descn parseOptions' ≫ resolve

-- that's all, folks! ----------------------------------------------------------
