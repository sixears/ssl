{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module ProcLib.MonadIO.Directory
  ( mkdir )
where

-- base --------------------------------

import Data.Function  ( ($) )

-- fpath --------------------------------

import FPath.AsFilePath        ( filepath )
import FPath.Dir2              ( DirAs )

-- monaderror-io -----------------------

import MonadError.IO        ( asIOError )
import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⫥) )

-- proclib -----------------------------

import ProcLib.Process       ( mkIO )
import ProcLib.Types.ProcIO  ( ProcIO )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unix --------------------------------

import System.Posix.Directory  ( createDirectory )
import System.Posix.Types      ( FileMode )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

mkdir ∷ (MonadIO μ, AsIOError ε, DirAs τ) ⇒ FileMode → τ → ProcIO ε μ ()
mkdir mode d =
  mkIO ([fmt|mkdir %T|] d) (asIOError $ createDirectory (d ⫥ filepath) mode)

-- that's all, folks! ----------------------------------------------------------
