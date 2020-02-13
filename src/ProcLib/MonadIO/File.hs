{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module ProcLib.MonadIO.File
  ( access, stat, fexists )
where

-- base --------------------------------

import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Nothing ) )

-- fpath --------------------------------

import FPath.AsFilePath        ( filepath )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.FPath2            ( FPathAs )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import qualified MonadIO.File2

import MonadIO        ( MonadIO )
import MonadIO.File2  ( CanAccess( NoCanAccess ), CanExec, CanRead, CanWrite
                      , FExists( FExists ) )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⫥) )

-- proclib -----------------------------

import ProcLib.Process2      ( mkIO'2 )
import ProcLib.Types.ProcIO  ( ProcIO )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unix --------------------------------

import System.Posix.Files  ( FileStatus )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

{- | Note that the `Maybe` is Nothing in case of mock. -}
stat ∷ (MonadIO μ, AsIOError ε, FPathAs τ) ⇒ τ → ProcIO ε μ (Maybe FileStatus)
stat fn = mkIO'2 Nothing ([fmt|stat: %T|] fn) (MonadIO.File2.stat fn)

access ∷ ∀ ε τ μ . (MonadIO μ, AsIOError ε, FPathAs τ) ⇒
                    CanRead → CanWrite → CanExec → τ → ProcIO ε μ CanAccess
access r w x fn = mkIO'2 NoCanAccess ([fmt|access: %T|] fn)
                         (MonadIO.File2.access r w x fn)

fexists ∷ (MonadIO μ, AsFPathError ε, AsIOError ε, FPathAs τ) ⇒
          τ → ProcIO ε μ FExists
fexists f = mkIO'2 FExists ([fmt|fexist %T|] $ f ⫥ filepath)
                           (MonadIO.File2.fexists f)

-- that's all, folks! ----------------------------------------------------------
