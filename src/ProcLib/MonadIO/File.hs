{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module ProcLib.MonadIO.File
  ( access, stat, fexists, touch, writeFile )
where

-- base --------------------------------

import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-default ------------------------

import Data.Default  ( def )

-- fpath --------------------------------

import FPath.AsFilePath        ( filepath )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.FileAs            ( FileAs )
import FPath.FPath2            ( FPathAs )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError )

-- monadio-plus ------------------------

import qualified MonadIO.File2

import MonadIO        ( MonadIO )
import MonadIO.File2  ( CanAccess( NoCanAccess ), CanExec, CanRead, CanWrite
                      , FExists( FExists ), FileStat )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⫥) )

-- proclib -----------------------------

import ProcLib.Process       ( mkIO )
import ProcLib.Process2      ( mkIO'2 )
import ProcLib.Types.ProcIO  ( ProcIO )

-- text --------------------------------

import Data.Text  ( Text )

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unix --------------------------------

import System.Posix.Types  ( FileMode )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

--------------------------------------------------------------------------------

{- | Note that the `Maybe` is Nothing in case of mock. -}
stat ∷ (MonadIO μ, AsIOError ε, FPathAs τ) ⇒ τ → ProcIO ε μ FileStat
stat fn = mkIO'2 def ([fmt|stat: %T|] fn) (MonadIO.File2.stat fn)

access ∷ ∀ ε τ μ . (MonadIO μ, AsIOError ε, FPathAs τ) ⇒
                    CanRead → CanWrite → CanExec → τ → ProcIO ε μ CanAccess
access r w x fn = mkIO'2 NoCanAccess ([fmt|access: %T|] fn)
                         (MonadIO.File2.access r w x fn)

----------------------------------------

fexists ∷ (MonadIO μ, AsFPathError ε, AsIOError ε, FPathAs τ) ⇒
          τ → ProcIO ε μ FExists
fexists f = mkIO'2 FExists ([fmt|fexist %T|] $ f ⫥ filepath)
                           (MonadIO.File2.fexists f)

----------------------------------------

touch ∷ ∀ τ ε μ .
        (MonadIO μ, AsIOError ε, FileAs τ) ⇒ Maybe FileMode → τ → ProcIO ε μ ()
touch Nothing fn = mkIO ([fmt|touch %T|] fn) (MonadIO.File2.touch Nothing fn)
touch mmode@(Just mode) fn = mkIO ([fmt|touch 0%04o %T|] mode fn)
                                  (MonadIO.File2.touch mmode fn)

----------------------------------------

writeFile ∷ ∀ ε π μ . (MonadIO μ, AsIOError ε, FileAs π) ⇒
            Maybe FileMode → π → Text → ProcIO ε μ ()
writeFile (Just m) fn =
  mkIO ([fmt|write 0%04o %T|] m fn) ∘ (MonadIO.File2.writeFile (Just m) fn)
writeFile Nothing fn =
  mkIO ([fmt|write ----- %T|] fn) ∘ (MonadIO.File2.writeFile Nothing fn)

-- that's all, folks! ----------------------------------------------------------
