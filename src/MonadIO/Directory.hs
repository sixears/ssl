{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE UnicodeSyntax       #-}

module MonadIO.Directory
  ( chdir, indir, indirE, lsdir, lsdir', mkdir )
where

-- base --------------------------------

import Control.Monad  ( filterM, join, sequence, return )
import Data.Bool      ( Bool( True ) )
import Data.Either    ( Either( Left, Right ) )
import Data.Foldable  ( foldr )
import Data.Function  ( ($), const )
import Data.Functor   ( fmap )
import Data.List      ( sortOn )
import Data.Maybe     ( Maybe( Just, Nothing ) )
import Data.String    ( String )
import System.Exit    ( ExitCode )
import System.IO      ( IO )
import Text.Show      ( Show )

-- base-unicode-symbols ----------------

import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( toString, toText )

-- directory ---------------------------

import System.Directory  ( listDirectory, setCurrentDirectory
                         , withCurrentDirectory )

-- exceptions --------------------------

import Control.Monad.Catch  ( bracket )

-- fpath --------------------------------

import FPath                   ( stripDir )
import FPath.AbsDir            ( AbsDir, absdir, __parseAbsDirP__ )
import FPath.AbsFile           ( AbsFile )
import FPath.AppendableFPath   ( AppendableFPath, (⫻) )
import FPath.AsFilePath        ( AsFilePath( filepath ) )
import FPath.Dir2              ( DirAs )
import FPath.DirType           ( DirType )
import FPath.FPath2            ( FPathAs )
import FPath.Error.FPathError  ( AsFPathError, FPathError, FPathIOError )
import FPath.Parseable         ( Parseable( __parse__, parse ) )
import FPath.RelDir            ( RelDir, reldir )
import FPath.RelFile           ( RelFile, relfile )
import FPath.RelType           ( RelType )

-- monaderror-io -----------------------

import MonadError           ( splitMError )
import MonadError2          ( ӂ )
import MonadError.IO        ( asIOError, eitherIOThrowT )
import MonadError.IO.Error  ( AsIOError, isNoSuchThingError )

-- monadio-plus ------------------------

import MonadIO  ( MonadIO, liftIO )

-- more-unicode ------------------------

import Data.MoreUnicode.Bool     ( 𝔹 )
import Data.MoreUnicode.Functor  ( (⊳), (⩺) )
import Data.MoreUnicode.Lens     ( (⫥) )
import Data.MoreUnicode.Monad    ( (⪼), (≫) )
import Data.MoreUnicode.Natural  ( ℕ )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- tasty -------------------------------

import Test.Tasty  ( TestTree, testGroup )

-- tasty-hunit -------------------------

import Test.Tasty.HUnit  ( Assertion, (@?=), assertBool, testCase )

-- tasty-plus --------------------------

import TastyPlus  ( assertIOException', assertRight, runTestsP
                  , runTestsReplay, runTestTree )

-- temporary ---------------------------

import System.IO.Temp ( withSystemTempDirectory )

-- text --------------------------------

import Data.Text  ( Text )

-- unix --------------------------------

import System.Posix.Directory  ( changeWorkingDirectory, createDirectory
                               , getWorkingDirectory )
import System.Posix.Types      ( FileMode )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

import MonadIO.File2  ( FileStat, IsDirectory( IsDirectory, NoIsDirectory )
                      , canRead, isdir, stat, touch )

--------------------------------------------------------------------------------

mkdir ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, DirAs δ) ⇒
        FileMode → δ → μ ()
mkdir mode d = asIOError $ createDirectory (d ⫥ filepath) mode

----------------------------------------

chdir ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, DirAs δ) => δ -> μ ()
chdir d = asIOError ∘ setCurrentDirectory $ d ⫥ filepath

----------------------------------------

indir ∷ (MonadIO μ, DirAs δ) ⇒ δ → IO α → μ α
indir d io = liftIO $ withCurrentDirectory (d ⫥ filepath) io

----------

indirE ∷ (MonadIO μ, DirAs δ, MonadError ε μ) ⇒ δ → ExceptT ε IO α → μ α
indirE d = join ∘ indir d ∘ splitMError

--------------------

{- | Note that this clears up the temp directory when done. -}
intmp ∷ MonadIO μ ⇒ Text → (AbsDir → IO α) → μ α
intmp t io = liftIO $
  withSystemTempDirectory (toString t) $ \ d →
    bracket (getWorkingDirectory ≫  \ o → changeWorkingDirectory d ⪼ return o)
            changeWorkingDirectory
            (\ _ → io $ __parseAbsDirP__ d)

----------

intmpE ∷ (MonadIO μ, MonadError ε μ) ⇒ Text → (AbsDir → ExceptT ε IO α) → μ α
intmpE t io = join  ∘ intmp t $ splitMError ∘ io

----------------------------------------

toDir ∷ (Parseable (DirType π), AsFilePath π) ⇒ π → DirType π
toDir x = __parse__ (x ⫥ filepath ⊕ "/")

----------------------------------------

{- | Given a set of filenames with their stats, compile a list of files, dirs &
     errors. -}
pathTypes ∷ (Parseable (DirType π), AsFilePath π) ⇒
            (π, Either ε FileStat)
          → ([π], [DirType π], [(π,ε)]) → ([π],[DirType π],[(π,ε)])

pathTypes (r, Left e) (fs, ds, es) = (fs, ds, (r,e) : es)
pathTypes (r, Right st) xs@(fs,ds,es) =
  case isdir st of
    Nothing            → xs -- path disappeared!
    Just IsDirectory   → (fs,toDir r:ds,es)
    Just NoIsDirectory → (r:fs,ds,es)

----------

{- | Pair a list of files with their stat outputs. -} 
stats ∷ (MonadIO μ, AsIOError ε, MonadError ε η, FPathAs τ) ⇒
        [τ] → μ [(τ, η FileStat)]
stats fns = sequence $ fmap (\ fn → (fn,) ⊳ splitMError (stat fn)) fns

----------

{- | Partition a list of files into files, dirs & errors. -}
partnTypes ∷ (MonadIO μ, AsIOError ε, FPathAs τ, Parseable (DirType τ)) ⇒
             [τ] → μ ([τ], [DirType τ], [(τ,ε)])
partnTypes = foldr pathTypes ([],[],[]) ⩺ stats

----------

lsdir ∷ ∀ α ε μ .
        (MonadIO μ, AsFilePath (DirType α), AppendableFPath α, FPathAs α,
         Parseable (DirType α), RelType α ~ RelFile,
         AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
        DirType α → μ ([α], [DirType α], [(α,ε)])
lsdir = lsdir' (return ∘ const True)

----------

lsdir' ∷ ∀ α ε μ .
         (MonadIO μ, AsFilePath (DirType α), AppendableFPath α, FPathAs α,
          Parseable (DirType α), RelType α ~ RelFile,
          AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
         (α → μ 𝔹) → DirType α → μ ([α], [DirType α], [(α,ε)])
lsdir' p d = do
  fns ← liftIO (listDirectory (d ⫥ filepath))
  xs ← sequence $ (fmap (d ⫻) ∘ parse @RelFile) ⊳ fns
  ys ← filterM p xs
  partnTypes ys


--------------------

mkTestDir ∷ (MonadIO μ, AsIOError ε, MonadError ε μ, DirAs δ) => δ -> μ ()
mkTestDir d = indirE d $ do
  mkdir 0o500 [reldir|r-x------/|]
  mkdir 0o400 [reldir|r--------/|]
  mkdir 0o100 [reldir|--x------/|]
  mkdir 0o700 [reldir|rwx------/|]
  mkdir 0o050 [reldir|---r-x---/|]
  mkdir 0o040 [reldir|---r-----/|]
  mkdir 0o010 [reldir|-----x---/|]
  mkdir 0o070 [reldir|---rwx---/|]
  touch (Just $ 0o500) [relfile|r-x|]
  touch (Just $ 0o300) [relfile|-wx|]
  touch (Just $ 0o400) [relfile|r--|]
  touch (Just $ 0o700) [relfile|rwx|]
  touch (Just $ 0o070) [relfile|---rwx|]
  return ()


----------

lsdirTests ∷ TestTree
lsdirTests =
  let noSuchThing       = assertBool "isNoSuchThing" ∘ isNoSuchThingError
      assertNoSuchThing ∷ Show α ⇒ IO α → Assertion
      assertNoSuchThing = assertIOException' noSuchThing

      lsdir_ ∷ AbsDir → IO ([AbsFile], [AbsDir], [(AbsFile, FPathIOError)])
      lsdir_ = eitherIOThrowT ∘ lsdir

      listdir ∷ (MonadIO μ, MonadError ε μ, AsFPathError ε, AsIOError ε) ⇒
                μ (AbsDir, ([AbsFile], [AbsDir], [(AbsFile, ε)]))
      listdir = intmpE "MonadIO.Directory" $ \ d → do
        mkTestDir d
        (d,) ⊳ lsdir d
      listdir' ∷ IO (Either FPathIOError
                            (AbsDir,
                                 ([AbsFile],[AbsDir],[(AbsFile,FPathIOError)])))
      listdir' = splitMError listdir

      listdirr ∷ (MonadIO μ, MonadError ε μ, AsFPathError ε, AsIOError ε) ⇒
                 μ (AbsDir, ([AbsFile], [AbsDir], [(AbsFile, ε)]))
      listdirr = intmpE "MonadIO.Directory" $ \ d → do
        mkTestDir d
        (d,) ⊳ lsdir' canRead d
      listdirr' ∷ IO (Either FPathIOError
                             (AbsDir,
                                 ([AbsFile],[AbsDir],[(AbsFile,FPathIOError)])))
      listdirr' = splitMError listdirr

      -- strip the /tmp/… prefix and sort for test determinism
      stripDir_ d = ӂ ∘ stripDir @_ @FPathError d
      strip ∷ (AbsDir, ([AbsFile], [AbsDir], α)) -> ([RelFile], [RelDir], α)
      strip (p, (fs,ds,z)) = (sortOn toText $ stripDir_ p ⊳ fs,
                              sortOn toText $ stripDir_ p ⊳ ds, z)

      expFiles = [ [relfile|---rwx|], [relfile|-wx|], [relfile|r--|]
                 , [relfile|r-x|] , [relfile|rwx|] ]
      expDirs  = [ [reldir|-----x---/|], [reldir|---r-----/|]
                 , [reldir|---r-x---/|], [reldir|---rwx---/|]
                 , [reldir|--x------/|], [reldir|r--------/|]
                 , [reldir|r-x------/|], [reldir|rwx------/|]
                 ]
      -- only those things that should be returned by listDirReadable; that is,
      -- owner-readable files, owner-readable-and-executable dirs
      expFilesR = [ [relfile|r--|], [relfile|r-x|] , [relfile|rwx|] ]
      expDirsR  = [ [reldir|r--------/|], [reldir|r-x------/|]
                  , [reldir|rwx------/|] ]
  

   in testGroup "listDir"
        [ testCase "nonesuch" $ assertNoSuchThing(lsdir_ [absdir|/nonesuch/|])
        , testCase "listDir isRight" $
            listdir' ≫ assertRight (assertBool "isRight" ∘ const True)
        , testCase "listDir"  $
            fmap (strip ⊳) listdir' ≫ (@?= Right (expFiles, expDirs, []))

        , testCase "listDirReadable"  $
            fmap (strip ⊳) listdirr' ≫ (@?= Right (expFilesR, expDirsR, []))

        ]

--------------------------------------------------------------------------------
--                                   tests                                    --
--------------------------------------------------------------------------------

tests ∷ TestTree
tests = testGroup "Directory" [ lsdirTests ]
                
----------------------------------------

_test ∷ IO ExitCode
_test = runTestTree tests

--------------------

_tests ∷ String → IO ExitCode
_tests = runTestsP tests

_testr ∷ String → ℕ → IO ExitCode
_testr = runTestsReplay tests

-- that's all, folks! ----------------------------------------------------------
