{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module CMark.Macro (
    Macro
  , runMacro
  , MacroT (..)
  , runMacroT
    -- * Macro combinators
  , fixpoint
    -- * Transformations
  , replaceString
  , rewriteNode
    -- * Queries
  , Query
    -- * Low-level traversals
  , everywhereM
  , everywhereTopDownM
  , everythingM
  ) where


import           Control.Monad
import           Control.Monad.IO.Class (MonadIO (..))
import           Control.Monad.Trans.Class (MonadTrans (..))

import           CMark
import           CMark.Macro.Monad (FixT, runFixT)
import qualified CMark.Macro.Monad as FixT

import           Data.Functor.Identity (Identity, runIdentity)
import           Data.Text (Text)

{--

interface notes

worthwhile to add Text -> m Text shorthand to skip cmark
possibly with CmarkOptions since we aren't robust to changes in that
interface anywya

make it really easy to write html macros even if it means introducing
extra types to express the arg parsing

examples:
text replacement hijinks
react-style macros - apply standard bulma thingos
custom codeblocks
text replace after prior macro expansion
graft footnotes onto cmark with codeblocks
includes

--}


{--

types of macros:

selectors:

- Code block macros
- HTML element macros

behaviour:
- Update in place
- Fold over and produce a result
- Fold over and delete

basic use: replace certain things
expert use: replace certain things with effectful functions (Either, or IO)
expert use: gather certain things and delete them from the tree in one swoop
anticipate fixpoints without re-parsing
anticipate staged macros - collect from all, then update all


API should produce composable functions over CMark nodes

monadic syntax (pipeline) abstracting FixT - expose shorthand for fix

Foldl style CPS embedding over 'everything' - lift stuff into Macro monad

MacroT - allow conflation of state and folds

consider something implicit? add StateT to the stack?

--}

newtype MacroT m a =
  MacroT {
      unMacroT :: FixT m a
    } deriving (Functor, Applicative, Monad)

instance MonadTrans MacroT where
  lift m = MacroT (lift m)

instance MonadIO m => MonadIO (MacroT m) where
  liftIO m = MacroT (lift (liftIO m))

runMacroT :: Functor m => MacroT m a -> m a
runMacroT =
  fmap fst . runFixT . unMacroT

type Macro a = MacroT Identity a

runMacro :: Macro a -> a
runMacro =
  fst . runIdentity . runFixT . unMacroT

-- -----------------------------------------------------------------------------
-- Macro combinators

-- | Apply the macro until it no longer makes progress
fixpoint :: Monad m => a -> (a -> MacroT m a) -> MacroT m a
fixpoint a f =
  lift $
    FixT.fixpoint (unMacroT . f) a

-- -----------------------------------------------------------------------------
-- Transformations

replaceString :: Text -> Text -> Node -> MacroT m Node
replaceString inp out node =
  undefined

-- rewriteElement :: (Text -> [(Text, Text)] -> [Node] -> Maybe Node) -> Node -> MacroT m Node

-- rewriteCodeBlock :: (Text -> [Node] -> Maybe Node) -> Node -> MacroT m Node

rewriteNode :: (Node -> Maybe Node) -> Node -> MacroT m Node
rewriteNode =
  undefined

-- -----------------------------------------------------------------------------
-- Queries

-- these are just fancy foldMs that can be composed in applicative style
-- have to reimplement FoldM to use everythingM instead
-- the point is to let us extract a bunch of stuff in a single pass
-- to be used in future macro phases

newtype Query m a =
  Query {
      unQuery :: FixT m a -- FoldM m Node a
    } deriving (Functor, Applicative)

-- runQuery :: Applicative m => Query m a -> Node -> m a
-- runQuery q n =
--   F.foldM (unQuery q) (pure n)

-- queryNodes :: Query m [Node]




-- -----------------------------------------------------------------------------
-- Low-level traversals

-- | Apply a monadic transformation everywhere in bottom-up manner
everywhereM :: Monad m => (Node -> m Node) -> Node -> m Node
everywhereM f =
  go >=> f
  where
    go node =
      case node of
        Node mpos ntype ns ->
          Node mpos ntype <$> traverse (go >=> f) ns

-- | Apply a monadic transformation everywhere in top-down manner
everywhereTopDownM :: Monad m => (Node -> m Node) -> Node -> m Node
everywhereTopDownM f =
  f >=> go
  where
    go node =
      case node of
        Node mpos ntype ns ->
          Node mpos ntype <$> traverse (f >=> go) ns

-- | Monadically summarise all nodes in top-down, left-to-right order
everythingM :: Monad m => (r -> r -> m r) -> (Node -> m r) -> (Node -> m r)
everythingM f gather =
  \node ->
    case node of
      Node mpos ntype ns -> do
        l <- gather node
        foldM (\r n -> f r =<< gather n) l ns
