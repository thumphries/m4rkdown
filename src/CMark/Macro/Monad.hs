-- | Copyright 2017, Ambiata, All Rights Reserved.
-- |
-- | Redistribution and use in source and binary forms, with or without
-- | modification, are permitted provided that the following conditions are
-- | met:
-- |
-- |   1. Redistributions of source code must retain the above copyright
-- |      notice, this list of conditions and the following disclaimer.
-- |
-- |   2. Redistributions in binary form must reproduce the above copyright
-- |      notice, this list of conditions and the following disclaimer in the
-- |      documentation and/or other materials provided with the distribution.
-- |
-- |   3. Neither the name of the copyright holder nor the names of
-- |      its contributors may be used to endorse or promote products derived
-- |      from this software without specific prior written permission.
-- |
-- | THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- | "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- | LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- | A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- | HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- | SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- | LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- | DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- | THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- | (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- | OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CMark.Macro.Monad (
    FixT
  , runFixT
  , once
  , fixpoint
  , progress
  , fixOfMaybe
  ) where


import           Control.Monad.Trans.Class


newtype FixT m a =
  FixT {
      unFixT :: m (a, Progress)
    }

runFixT :: FixT m a -> m (a, Progress)
runFixT =
  unFixT
{-# INLINE runFixT #-}

data Progress =
    RunAgain
  | NoProgress

instance Monad m => Functor (FixT m) where
  fmap f p = p >>= (return . f)

instance Monad m => Applicative (FixT m) where
  pure = return
  (<*>) f x = do
    !f' <- f
    !x' <- x
    return $! f' x'

instance Monad m => Monad (FixT m) where
  return a = FixT $ return (a, NoProgress)
  (>>=) p q = FixT $ do
    (res,prog)   <- runFixT $! p
    (res',prog') <- runFixT $! q res
    return (res', eitherProgress prog prog')

instance MonadTrans FixT where
  lift m = FixT $ do
    v <- m
    return (v, NoProgress)

eitherProgress :: Progress -> Progress -> Progress
eitherProgress RunAgain _ = RunAgain
eitherProgress _ RunAgain = RunAgain
eitherProgress _ _        = NoProgress
{-# INLINE eitherProgress #-}

-- | Return a value and proclaim: "it might be worth running again"
progress :: Monad m => a -> FixT m a
progress a
 = FixT $ return (a, RunAgain)
{-# INLINE progress #-}

-- | Apply the transform until it no longer makes progress
fixpoint :: Monad m => (a -> FixT m a) -> a -> m a
fixpoint f a
 = do (a',prog) <- runFixT $! f a
      case prog of
       RunAgain   -> fixpoint f a'
       NoProgress -> return a'
{-# INLINE fixpoint #-}

-- | Run a FixT once, regardless of whether it believes it makes progress or not
once :: Monad m => FixT m a -> m a
once f
 = do (r,_) <- runFixT f
      return r
{-# INLINE once #-}

-- | Take a function that returns @Just@ on progress and @Nothing@ on completion.
fixOfMaybe :: Monad m => (a -> m (Maybe a)) -> a -> FixT m a
fixOfMaybe f a
 = do a' <- lift $ f $! a
      case a' of
       Just a''-> progress a''
       Nothing -> return a
{-# INLINE fixOfMaybe #-}
