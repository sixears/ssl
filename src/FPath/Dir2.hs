{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.Dir2
  ( DirAs( _Dir' ), AsDir( _Dir ) )
where

-- base --------------------------------

import Data.Function  ( id )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- fpath -------------------------------

import FPath.Dir  ( Dir )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsDir      ( AbsDir, AsAbsDir( _AbsDir ) )
import FPath.AsFilePath  ( AsFilePath )
import FPath.RelDir      ( RelDir, AsRelDir( _RelDir ) )

--------------------------------------------------------------------------------

class (Printable α, AsFilePath α) ⇒ DirAs α where
  _Dir' ∷ Prism' Dir α
instance DirAs Dir where
  _Dir' = id
instance DirAs AbsDir where
  _Dir' = _AbsDir
instance DirAs RelDir where
  _Dir' = _RelDir

------------------------------------------------------------

class AsDir α where
  _Dir ∷ Prism' α Dir

instance AsDir Dir where
  _Dir = id

-- that's all, folks! ----------------------------------------------------------
