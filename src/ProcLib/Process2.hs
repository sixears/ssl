{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module ProcLib.Process2
  ( mkIO'1, mkIO'2, mkIO'2Y )
where

-- base --------------------------------

import Data.Maybe   ( Maybe( Just, Nothing ) )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )

-- proclib -----------------------------

import ProcLib.Process                ( mkIO' )
import ProcLib.Types.CreateProcOpts   ( MockLvl( MockLvl ) )
import ProcLib.Types.ProcIO           ( ProcIO )

-- text --------------------------------

import Data.Text     ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

{- | `mkIO'` with MockLvl 1. -}
mkIO'1 ∷ ∀ ω ε μ . (MonadIO μ) ⇒ ω → Text → μ ω → ProcIO ε μ ω
mkIO'1 = mkIO' (MockLvl 1)

{- | `mkIO'` with MockLvl 2; e.g., for passive IO actions like fileExists.  -}
-- for passive IO (e.g., fileExists)
mkIO'2 ∷ ∀ ω ε μ . (MonadIO μ) ⇒ ω → Text → μ ω → ProcIO ε μ ω
mkIO'2 = mkIO' (MockLvl 2)

{- | `mkIO'` for things that are hard to provide Mocks for; this turns the
     result to a `Maybe` and thus the mock value is a Nothing. -}
mkIO'2Y ∷ ∀ ε ω α μ . MonadIO μ ⇒ Text → (α → μ ω) → α → ProcIO ε μ (Maybe ω)
mkIO'2Y name io val = mkIO'2 Nothing name (Just ⊳ io val)

-- that's all, folks! ----------------------------------------------------------
