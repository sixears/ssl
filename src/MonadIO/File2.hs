{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}

module MonadIO.File2
  ( CanRead(..), CanWrite(..), CanExec(..), CanAccess(..), FExists(..)
  , FileStat, IsDirectory(..)
  , access, canExec, canRead, canReadExec, existsf, fexists, isdir, stat, touch
  , writeFile
  )
where

-- base --------------------------------

import Control.Monad  ( return )
import Data.Bits      ( (.&.) )
import Data.Bool      ( Bool( False, True ), bool )
import Data.Either    ( Either( Right ) )
import Data.Eq        ( Eq( (==) ) )
import Data.Function  ( ($), const )
import Data.Functor   ( fmap )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import System.Exit    ( ExitCode )
import System.IO      ( IO, IOMode( AppendMode ), withFile )
import Text.Show      ( Show( show ) )

-- base-unicode-symbols ----------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )

-- data-textual ------------------------

import Data.Textual  ( Printable( print ) )

-- data-default ------------------------

import Data.Default  ( Default( def ) )

-- fpath -------------------------------

import FPath.AbsFile     ( AbsFile )
import FPath.AsFilePath  ( filepath )
import FPath.FileAs      ( FileAs )
import FPath.FPath2      ( FPathAs )
import FPath.Parseable   ( __parse__ )

-- monaderror-io -----------------------

import MonadError            ( ѥ )
import MonadError2           ( ӂ )
import MonadError.IO         ( asIOError )
import MonadError.IO.Error   ( AsIOError, IOError
                             , ioError, squashNoSuchThingT )
import MonadError.IO.Error2  ( mkNoExistsE, squashNoSuchThingBT )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Lens     ( (⫥) )
import Data.MoreUnicode.Monad    ( (≫) )
import Data.MoreUnicode.Natural  ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestName, TestTree, testGroup, withResource )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( (@=?), testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertIsLeft, runTestsP, runTestsReplay, runTestTree )

-- temporary ---------------------------

import System.IO.Temp ( withSystemTempFile )

-- text --------------------------------

import qualified  Data.Text.IO

import Data.Text     ( Text )

-- text-printer ------------------------

import qualified  Text.Printer  as  P

-- tfmt --------------------------------

import Text.Fmt  ( fmt )

-- unix --------------------------------

import System.Posix.Files  ( FileStatus, fileAccess, fileExist, fileMode
                           , getFileStatus, isDirectory, removeLink, setFileMode
                           )
import System.Posix.Types  ( FileMode )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO  ( MonadIO )

--------------------------------------------------------------------------------

data FExists = FExists | NoFExists
  deriving (Eq,Show)

fexists ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FPathAs τ) ⇒ τ → μ FExists
fexists f = asIOError $ bool NoFExists FExists ⊳ fileExist (f ⫥ filepath)

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

----------------------------------------

canRead ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FPathAs τ) ⇒ τ → μ 𝔹
canRead fn = do
  r ← ѥ ((≡ CanAccess) ⊳ access CanRead NoCanWrite NoCanExec fn)
  squashNoSuchThingBT r

----------------------------------------

{- | Can we execute the given file?

   > Right x ← splitMError $ canExec [absfile|/usr/bin/env|]
 -}
canExec ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FPathAs τ) ⇒ τ → μ 𝔹
canExec fn = do
  r ← ѥ ((≡ CanAccess) ⊳ access NoCanRead NoCanWrite CanExec fn)
  squashNoSuchThingBT r

----------------------------------------

{- | Can we read and execute the given file? -}
canReadExec ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, FPathAs τ) ⇒ τ → μ 𝔹
canReadExec fn = do
  r ← ѥ ((≡ CanAccess) ⊳ access CanRead NoCanWrite CanExec fn)
  squashNoSuchThingBT r

------------------------------------------------------------

data IsDirectory = IsDirectory | NoIsDirectory
  deriving (Eq,Show)

isdir ∷ FileStat → Maybe IsDirectory
isdir (FileStat Nothing)   = Nothing
isdir (FileStat (Just st)) =
  Just $ bool NoIsDirectory IsDirectory $ isDirectory st

----------------------------------------

data FileStat = FileStat (Maybe FileStatus)

instance Eq FileStat where
  FileStat Nothing  == FileStat Nothing  = True
  FileStat Nothing  == FileStat (Just _) = False
  FileStat (Just _) == FileStat Nothing  = False
  x@(FileStat (Just _)) == y@(FileStat (Just _)) = (fmode x ≡ fmode y)

instance Show FileStat where
  show (FileStat (Just st)) =
      P.string $ [fmt|mode: %04o|] (fileMode st .&. 0o777)
  show (FileStat Nothing)   = P.string $ "-"

instance Printable FileStat where
  print (FileStat (Just st)) =
      P.text $ [fmt|mode: %04o|] (fileMode st .&. 0o777)
  print (FileStat Nothing)   = P.text $ "-"

stat ∷ ∀ ε τ μ .
       (MonadIO μ, AsIOError ε, MonadError ε μ, FPathAs τ) ⇒
       τ → μ FileStat
stat = FileStat ⩺ squashNoSuchThingT ∘ asIOError ∘ getFileStatus ∘ (⫥ filepath)

{- | An empty FileStat, to use as a default. -}
nullstat ∷ FileStat
nullstat = FileStat Nothing

instance Default FileStat where
  def = nullstat

fmode ∷ FileStat → Maybe FileMode
fmode (FileStat (Just st)) = Just $ fileMode st .&. 0o777
fmode (FileStat Nothing)   = Nothing

existsf ∷ FileStat → FExists
existsf (FileStat Nothing) = NoFExists
existsf _                  = FExists

----------------------------------------

{- | Like `touchFile`, but also creates the file if required; pass in a file
     mode to create the file, pass in `Nothing` to touch an existing file (but
     will fail if the file does not already exist). -}
touch ∷ ∀ τ ε μ .
        (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs τ) ⇒
        Maybe FileMode → τ → μ ()
touch mmode fn =
  let fp = fn ⫥ filepath
   in do fex ← fexists fn
         asIOError $ case mmode of
                       Nothing   →
                         case fex of
                           FExists  → withFile fp AppendMode (const $ return ())
                           NoFExists→ ioError (mkNoExistsE "touch" fn)
                       Just mode → do withFile fp AppendMode (const $ return ())
                                      setFileMode fp mode

touchTests ∷ TestTree
touchTests = do
  let gettmp ∷ IO (Either IOError AbsFile)
      gettmp = ѥ $ withSystemTempFile "MonadIO-File-touch-"
                                      (const ∘ return ∘ __parse__ @AbsFile)


      __gettmp__ ∷ IO AbsFile
      __gettmp__ = ӂ ⊳ gettmp

      __unlink__ ∷ (MonadIO μ, FileAs π) ⇒ π → μ ()
      __unlink__ = fmap ӂ ∘ ѥ ∘ unlink @IOError

      touchOK ∷ FileAs τ ⇒ TestName → Maybe FileMode → IO τ → TestTree
      touchOK nme mode t = testCase nme $ t ≫
          \ fn → ѥ (touch @_ @IOError mode fn) ≫ (@=? Right ())
      touchNOK ∷ FileAs τ ⇒ TestName → Maybe FileMode → IO τ → TestTree
      touchNOK nme mode t = testCase nme $ t ≫
          \ fn → ѥ (touch @_ @IOError mode fn) ≫ assertIsLeft
      fmodeOK ∷ FileAs τ ⇒ TestName → FileMode → IO τ → TestTree
      fmodeOK nme mode t = testCase nme $ t ≫
          \ fn → ѥ (fmode ⊳ stat @IOError  fn) ≫ (@=? Right (Just mode))
      fmodeNOK ∷ FileAs τ ⇒ TestName → IO τ → TestTree
      fmodeNOK nme t = testCase nme $ t ≫
          \ fn → ѥ (fmode ⊳ stat @IOError  fn) ≫ (@=? Right Nothing)
  testGroup "touch" [ withResource __gettmp__ __unlink__ $ \ t →
                        testGroup "tmp0" [ touchOK  "touch0" (Just 0o070) t
                                         -- this should fail because mode 070
                                         -- makes the file non-writable by
                                         -- the owner
                                         , touchNOK "touch1" (Just 0o070) t
                                         , touchNOK "touch2" Nothing      t
                                         ]
                    , withResource __gettmp__ __unlink__ $ \ t →
                        testGroup "tmp1" [ touchOK  "touch0" (Just 0o600) t
                                         , fmodeOK  "fmode0" 0o600        t
                                         , touchOK  "touch1" Nothing      t
                                         , fmodeOK  "fmode1" 0o600        t
                                         , touchOK  "touch2" (Just 0o700) t
                                         , fmodeOK  "fmode2" 0o700        t
                                         , touchOK  "touch3" Nothing      t
                                         , fmodeOK  "fmode3" 0o700        t
                                         , touchOK  "touch4" (Just 0o040) t
                                         , fmodeOK  "fmode4" 0o040        t
                                         -- this should fail because mode 040
                                         -- makes the file non-writable by
                                         -- the owner
                                         , touchNOK "touch5" Nothing      t
                                         ]
                    , withResource __gettmp__ __unlink__ $ \ t →
                        testGroup "tmp2" [ -- a non-pre-existing file should
                                           -- fail without a mode
                                           touchNOK "touch0" Nothing      t
                                         , fmodeNOK "fmode0"              t
                                         , touchNOK "touch1" Nothing      t
                                         , fmodeNOK "fmode1"              t
                                         , touchOK  "touch2" (Just 0o004) t
                                         , fmodeOK  "fmode2" 0o004        t
                                         -- this should fail because mode 004
                                         -- makes the file non-writable by
                                         -- the owner
                                         , touchNOK "touch3" Nothing      t
                                         ]
                    ]

----------------------------------------

unlink ∷ ∀ ε π μ . (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs π) ⇒ π → μ ()
unlink f = asIOError $ removeLink (f ⫥ filepath)

----------------------------------------

writeFile ∷ ∀ ε π μ .
            (MonadIO μ, AsIOError ε, MonadError ε μ, FileAs π) ⇒ π → Text → μ ()
writeFile fn = asIOError ∘ Data.Text.IO.writeFile (fn ⫥ filepath)

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "File" [ touchTests ]
                
----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
