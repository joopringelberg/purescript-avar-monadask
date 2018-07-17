module Control.Monad.AvarMonadAsk (get, gets, put, modify) where

import Control.Monad.Aff.AVar (AVAR, AVar, putVar, readVar, takeVar)
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Control.Monad.Reader (class MonadAsk, ask)
import Prelude ((>>=), (<<<), bind, pure, ($), Unit, (>=>))

-- | Returns the AVar holding the state.
getAVarWithState :: forall s m. MonadAsk (AVar s) m => m (AVar s)
getAVarWithState = ask

-- | Returns the state. Does not block the AVar: it is not left empty.
-- | Compares with StateT get.
get :: forall s m e.
  MonadAff (avar :: AVAR | e) m =>
  MonadAsk (AVar s) m =>
  m s
get = do
  (as :: AVar s) <- getAVarWithState
  liftAff $ readVar as

-- | Returns the result of applying a function to the state.
-- | Compares with StateT gets.
gets :: forall s m e a.
  MonadAff (avar :: AVAR | e) m =>
  MonadAsk (AVar s) m =>
  (s -> a) -> m a
gets f = get >>= pure <<< f

-- | Puts a new state in the AVar holding the state.
-- | Compares with StateT put.
put :: forall e s m. MonadAff (avar :: AVAR | e) m => MonadAsk (AVar s) m => s -> m Unit
put state = getAVarWithState >>= (liftAff <<< (replaceAVarContent state))

-- | Make an AVar empty. Blocks as long as the AVar passed in is empty.
clearVar :: forall e a m. MonadAff (avar :: AVAR | e) m => AVar a -> m (AVar a)
clearVar v = do
  _ <- liftAff $ takeVar v
  pure v

-- | Replace the content of an AVar. Blocks as long as the AVar passed in is empty.
replaceAVarContent :: forall e a m.
  MonadAff (avar :: AVAR | e) m =>
  a -> AVar a -> m Unit
replaceAVarContent value = liftAff <<< clearVar >=> liftAff <<< putVar value

-- | Replace the content of the AVar holding the state, with the result of applying a function to its contents. Does not block.
-- | Compares with StateT modify.
modify :: forall e s m.
  MonadAff (avar :: AVAR | e) m =>
  MonadAsk (AVar s) m =>
  (s -> s) -> m Unit
modify f = get >>= put <<< f
