{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MonadError.IO.Error2
  ( squashNoSuchThingBT )
where

-- base --------------------------------

import Control.Monad  ( join )
import Data.Functor   ( fmap )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool  ( 𝔹 )

-- monaderror-io -----------------------

import MonadError            ( splitMError )
import MonadError.IO.Error   ( AsIOError, squashNoSuchThingB )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

squashNoSuchThingBT ∷ ∀ ε μ . (AsIOError ε, MonadError ε μ) ⇒
                      ExceptT ε μ 𝔹 → μ 𝔹
squashNoSuchThingBT = join ∘ fmap squashNoSuchThingB ∘  splitMError

-- that's all, folks! ----------------------------------------------------------
