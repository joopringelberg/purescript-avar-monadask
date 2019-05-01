module Control.Monad.AvarMonadAsk (get, gets, put, modify) where

import Effect.Aff.AVar (AVar, put, read, take) as AV
import Effect.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Reader (class MonadAsk, ask)
import Prelude ((>>=), (<<<), bind, pure, ($), Unit, (>=>))

-- | Returns the AVar holding the state.
getAVarWithState :: forall s m. MonadAsk (AV.AVar s) m => m (AV.AVar s)
getAVarWithState = ask

-- | Returns the state. Blocks as long as the AVar is empty. Does not block a filled AVar: it is not left empty.
-- | Compares with StateT get.
get :: forall s m.
  MonadAff m =>
  MonadAsk (AV.AVar s) m =>
  m s
get = do
  (as :: AV.AVar s) <- getAVarWithState
  liftAff $ AV.read as

-- | Returns the result of applying a function to the state.
-- | Compares with StateT gets.
gets :: forall s m a.
  MonadAff m =>
  MonadAsk (AV.AVar s) m =>
  (s -> a) -> m a
gets f = get >>= pure <<< f

-- | Puts a new state in the AVar holding the state. Uses replaceAVarContent: so blocks as long as the AVar
-- | passed in is empty.
-- | Compares with StateT put.
put :: forall s m. MonadAff m => MonadAsk (AV.AVar s) m => s -> m Unit
put state = getAVarWithState >>= (liftAff <<< (replaceAVarContent state))

-- | Make an AVar empty. Blocks as long as the AVar passed in is empty.
clearVar :: forall a m. MonadAff m => AV.AVar a -> m (AV.AVar a)
clearVar v = do
  _ <- liftAff $ AV.take v
  pure v

-- | Replace the content of an AVar. Blocks as long as the AVar passed in is empty (waits till it is filled).
-- | Then, if it is filled, blocks while the content is being replaced.
replaceAVarContent :: forall a m.
  MonadAff m =>
  a -> AV.AVar a -> m Unit
replaceAVarContent value = liftAff <<< clearVar >=> liftAff <<< AV.put value

-- | Replace the content of the AVar holding the state, with the result of applying a function to its contents. Blocks as long as the AVar is empty; blocks while the content is modified.
-- | Compares with StateT modify.
modify :: forall s m.
  MonadAff m =>
  MonadAsk (AV.AVar s) m =>
  (s -> s) -> m Unit
modify f = get >>= put <<< f
