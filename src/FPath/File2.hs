{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.File2
  ( AsFile( _File ) )
where

-- base --------------------------------

import Data.Function  ( id )

-- fpath -------------------------------

import FPath.File  ( File )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

class AsFile α where
  _File ∷ Prism' α File

instance AsFile File where
  _File = id

-- that's all, folks! ----------------------------------------------------------
