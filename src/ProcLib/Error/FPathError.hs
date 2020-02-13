{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}

module ProcLib.Error.FPathError
  ( FPathIOExecCreateError )
where

-- base --------------------------------

import Control.Exception  ( Exception )
import Data.Either        ( Either( Left, Right ) )
import Data.Eq            ( Eq )
import Text.Show          ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- fpath --------------------------------

import FPath.Error.FPathError  ( AsFPathError( _FPathError ), FPathIOError )

-- lens --------------------------------

import Control.Lens.Prism   ( Prism', prism )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError( _IOError ) )

-- proclib -----------------------------

import ProcLib.Error.CreateProcError  ( AsCreateProcError( _CreateProcError )
                                      , CreateProcError( CreateProcError ) )
import ProcLib.Error.ExecError        ( AsExecError( _ExecError ) )
import ProcLib.Error.ExecCreateError  ( ExecCreateError( ECCreateE, ECExecE )
                                      , _ECCreateE, _ECExecE )

--------------------------------------------------------------------------------

data FPathIOExecCreateError = FPIECExecCreateError ExecCreateError
                            | FPIECFPathIOError      FPathIOError
  deriving (Eq, Show)

instance Exception FPathIOExecCreateError

_FPIECExecCreateError ∷ Prism' FPathIOExecCreateError ExecCreateError
_FPIECExecCreateError =
  prism (\ e → FPIECExecCreateError e)
        (\ e → case e of FPIECExecCreateError e' → Right e'; _ → Left e)

_FPIECFPathIOError ∷ Prism' FPathIOExecCreateError FPathIOError
_FPIECFPathIOError = prism (\ e → FPIECFPathIOError e)
                       (\ e → case e of FPIECFPathIOError e' → Right e'; _ → Left e)

instance AsFPathError FPathIOExecCreateError where
  _FPathError = _FPIECFPathIOError ∘ _FPathError

instance AsIOError FPathIOExecCreateError where
  _IOError = _FPIECFPathIOError ∘ _IOError

instance AsExecError FPathIOExecCreateError where
  _ExecError = _FPIECExecCreateError ∘ _ECExecE

instance AsCreateProcError FPathIOExecCreateError where
  _CreateProcError = _FPIECExecCreateError ∘ _ECCreateE

instance Printable FPathIOExecCreateError where
  print (FPIECExecCreateError e) = print e
  print (FPIECFPathIOError e) = print e

instance Printable CreateProcError where
  print (CreateProcError e) = print e

instance Printable ExecCreateError where
  print (ECExecE e) = print e
  print (ECCreateE e) = print e

-- that's all, folks -----------------------------------------------------------
