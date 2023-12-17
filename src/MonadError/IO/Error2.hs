{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MonadError.IO.Error2
  ( mkNoExistsE, noExistsE, squashNoSuchThingBT )
where

-- base --------------------------------

import Control.Monad    ( join )
import Data.Function    ( ($) )
import Data.Functor     ( fmap )
import Data.Maybe       ( Maybe( Just, Nothing ) )
import System.IO.Error  ( doesNotExistErrorType, mkIOError )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- fpath -------------------------------

import FPath.AsFilePath  ( filepath )
import FPath.FPath2      ( FPathAs )

-- lens --------------------------------

import Control.Lens.Review  ( (#) )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool  ( 𝔹 )
import Data.MoreUnicode.Lens  ( (⫥) )

-- monaderror-io -----------------------

import MonadError            ( splitMError )
import MonadError.IO.Error   ( AsIOError( _IOErr ), squashNoSuchThingB )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError, throwError )

-- text --------------------------------

import Data.Text  ( Text )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

squashNoSuchThingBT ∷ ∀ ε μ . (AsIOError ε, MonadError ε μ) ⇒
                      ExceptT ε μ 𝔹 → μ 𝔹
squashNoSuchThingBT = join ∘ fmap squashNoSuchThingB ∘  splitMError

----------------------------------------

mkNoExistsE ∷ (AsIOError ε, FPathAs τ) ⇒ Text → τ → ε
mkNoExistsE n fp =
  _IOErr # mkIOError doesNotExistErrorType ([fmt|%t: no such file|] n) Nothing
                                           (Just (fp ⫥ filepath))

noExistsE ∷ (AsIOError ε, MonadError ε η, FPathAs τ) ⇒ Text → τ → η α
noExistsE n fp = throwError $ mkNoExistsE n fp

-- that's all, folks! ----------------------------------------------------------
