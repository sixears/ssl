{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module FPath.FileAs
  ( FileAs( _File' ) )
where

-- base --------------------------------

import Data.Function  ( id )

-- data-textual ------------------------

import Data.Textual  ( Printable )

-- fpath -------------------------------

import FPath.File  ( File )

-- lens --------------------------------

import Control.Lens.Prism  ( Prism' )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import FPath.AbsFile      ( AbsFile, AsAbsFile( _AbsFile ) )
import FPath.AsFilePath   ( AsFilePath )
import FPath.FPath2       ( FPathAs )
import FPath.RelFile      ( RelFile, AsRelFile( _RelFile ) )

--------------------------------------------------------------------------------

{- | Class of things that are guaranteed convertable to an File (but that an
     File might or might not be able to convert to). -}
-- FPathAs isn't required by implementation; but if you can convert to a File
-- (i.e., are an instance of FileAs); then you can *definitely* convert to an
-- FPath (i.e., should be an instance of FPathAs); adding it as a constraint
-- here reduces the need to add as a constraint on functions using that.
class (Printable α, AsFilePath α, FPathAs α) ⇒ FileAs α where
  _File' ∷ Prism' File α
instance FileAs File where
  _File' = id
instance FileAs AbsFile where
  _File' = _AbsFile
instance FileAs RelFile where
  _File' = _RelFile

-- that's all, folks! ----------------------------------------------------------
