{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MonadIO.File2
  ( CanRead(..), CanWrite(..), CanExec(..), CanAccess(..), access
  , FExists(..), fexists, IsDirectory(..), isdir, stat )
where

-- base --------------------------------

import Data.Bool      ( bool )
import Data.Eq        ( Eq )
import Data.Function  ( ($) )
import Data.Maybe     ( Maybe )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- fpath -------------------------------

import FPath.AsFilePath  ( filepath )
import FPath.FPath2      ( FPathAs )

-- monaderror-io -----------------------

import MonadError.IO         ( asIOError )
import MonadError.IO.Error   ( AsIOError, squashNoSuchThingT )
import MonadError.IO.Error2  ( squashNoSuchThingBT )

-- more-unicode ------------------------

import Data.MoreUnicode.Functor  ( (⊳) )
import Data.MoreUnicode.Lens     ( (⫥) )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- unix --------------------------------

import System.Posix.Files  ( FileStatus
                           , fileAccess, fileExist, getFileStatus, isDirectory )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO  ( MonadIO )

--------------------------------------------------------------------------------

data FExists = FExists | NoFExists
  deriving (Eq,Show)

fexists ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FPathAs τ) ⇒ τ → μ FExists
fexists f = asIOError $ bool FExists NoFExists ⊳ fileExist (f ⫥ filepath)

------------------------------------------------------------

{- | Can we read a file? -}
data CanRead = CanRead | NoCanRead
  deriving (Eq,Show)

{- | Can we read a file? -}
data CanWrite = CanWrite | NoCanWrite
  deriving (Eq,Show)

{- | Can we exec a file? -}
data CanExec = CanExec | NoCanExec
  deriving (Eq,Show)

{- | Can we access a file? -}
data CanAccess = CanAccess | NoCanAccess
  deriving (Eq,Show)

{- | Type-safe invocation of `access (2)`.
     Non-existent files will return false.
     No values such as `NoCanRead` don't mean "file that we cannot read", but
     rather mean "readability is not a requirement".
 -}
access ∷ ∀ ε τ μ . (MonadIO μ, AsIOError ε, MonadError ε μ, FPathAs τ) ⇒
                    CanRead → CanWrite → CanExec → τ → μ CanAccess
access r w x fn =
  bool NoCanAccess CanAccess ⊳ (squashNoSuchThingBT ∘ asIOError $ 
    fileAccess (fn ⫥ filepath) (CanRead ≡ r) (CanWrite ≡ w) (CanExec ≡ x))

------------------------------------------------------------

data IsDirectory = IsDirectory | NoIsDirectory
  deriving (Eq,Show)

isdir ∷ FileStatus → IsDirectory
isdir = bool NoIsDirectory IsDirectory ∘ isDirectory

----------------------------------------

stat ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FPathAs τ) ⇒
       τ → μ (Maybe FileStatus)
stat = squashNoSuchThingT ∘ asIOError ∘ getFileStatus ∘ (⫥ filepath)

-- that's all, folks! ----------------------------------------------------------
