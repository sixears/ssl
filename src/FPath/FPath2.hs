{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.FPath2
  ( FPathAs( _FPath ) )
where

-- base --------------------------------

import Data.Function  ( id )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir      ( AbsDir, AsAbsDir( _AbsDir ) )
import FPath.FPath       ( FPath )
import FPath.AsFilePath  ( AsFilePath )

--------------------------------------------------------------------------------

class (Printable α, AsFilePath α) ⇒ FPathAs α where
  _FPath ∷ Prism' FPath α

instance FPathAs FPath where
  _FPath = id

instance FPathAs AbsDir where
  _FPath = _AbsDir

-- that's all, folks! ----------------------------------------------------------
