{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MonadError2
  ( ӂ, __eitherError__ )
where

import Prelude  ( error )

-- base --------------------------------

import Data.Either    ( Either( Left, Right ) )
import Data.Function  ( ($) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

__eitherError__ ∷ Printable ε ⇒ Either ε α → α
__eitherError__ (Left e)  = error $ toString e
__eitherError__ (Right a) = a

ӂ ∷ Printable ε ⇒ Either ε α → α
ӂ = __eitherError__

-- that's all, folks! ----------------------------------------------------------
