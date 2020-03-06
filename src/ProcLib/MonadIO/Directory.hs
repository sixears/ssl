{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE UnicodeSyntax     #-}

module ProcLib.MonadIO.Directory
  ( chdir, ensureEmptyDir, lsdir, mkdir )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe( Just, Nothing ) )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString )

-- fpath --------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.AppendableFPath   ( AppendableFPath )
import FPath.AsFilePath        ( AsFilePath )
import FPath.Dir               ( Dir( DirA ) )
import FPath.Dir2              ( DirAs )
import FPath.DirType           ( DirType )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.FPath2            ( FPathAs )
import FPath.Parseable         ( Parseable )
import FPath.RelFile           ( RelFile )
import FPath.RelType           ( RelType )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( AsIOError, userE )

-- monadio-plus ------------------------

import qualified  MonadIO.Directory

import MonadIO        ( MonadIO )
import MonadIO.File2  ( CanAccess( CanAccess, NoCanAccess ), CanExec( CanExec )
                      , CanRead( CanRead ), CanWrite( CanWrite )
                      , IsDirectory( IsDirectory, NoIsDirectory )
                      , isdir
                      )

-- more-unicode ------------------------

import Data.MoreUnicode.Monad    ( (≫) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError, throwError )
import Control.Monad.Trans   ( lift )

-- tfmt --------------------------------

import Text.Fmt  ( fmt, fmtT )

-- unix --------------------------------

import System.Posix.Types  ( FileMode )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import ProcLib.MonadIO.File        ( access, stat )
import ProcLib.Process             ( mkIO )
import ProcLib.Types.MockDefault2  ( ) -- more instances of MockDefault
import ProcLib.Types.ProcIO        ( ProcIO )


--------------------------------------------------------------------------------

mkdir ∷ (MonadIO μ, AsIOError ε, DirAs δ) ⇒ FileMode → δ → ProcIO ε μ ()
mkdir mode d =
  mkIO ([fmt|mkdir %04o %T|] mode d) (MonadIO.Directory.mkdir mode d)

----------------------------------------

chdir ∷ (MonadIO μ, AsIOError ε, DirAs δ) => δ -> ProcIO ε μ ()
chdir d = mkIO ([fmt|chdir %T|] d) (MonadIO.Directory.chdir d)

----------------------------------------

lsdir ∷ ∀ α ε μ .
        (MonadIO μ, AsFilePath (DirType α), AppendableFPath α, FPathAs α,
         Parseable (DirType α), RelType α ~ RelFile, Printable (DirType α),
         AsFPathError ε, AsIOError ε) ⇒
        DirType α → ProcIO ε μ ([α], [DirType α], [(α,ε)])
lsdir d = mkIO ([fmt|lsdir %T|] d) (MonadIO.Directory.lsdir d)

----------------------------------------

ensureEmptyDir :: ∀ ε μ . (MonadIO μ, AsIOError ε, AsFPathError ε) ⇒
                  AbsDir → ProcIO ε μ ()
ensureEmptyDir d =
  let uE ∷ (AsIOError ε, MonadError ε η, Printable τ) ⇒ τ → η ω
      uE = throwError ∘ userE ∘ toString
      nonemptyE = lift ∘ uE$ [fmtT|will not use non-empty directory %T|] d
      noaccessE = lift ∘ uE$ [fmtT|cannot rwx directory: %T|] d
   in stat d ≫ \ st →
        case isdir st of
          Nothing → mkdir 0o755 (DirA $ d)
          Just NoIsDirectory → lift $ uE $ [fmtT|is not a directory: %T|] d
          Just IsDirectory → do access CanRead CanWrite CanExec d ≫
                                  \ case
                                    NoCanAccess → noaccessE
                                    CanAccess   → lsdir @AbsFile d ≫
                                      \ case
                                        ([],[],[]) → return ()
                                        (_,_,_)    → nonemptyE


-- that's all, folks! ----------------------------------------------------------
