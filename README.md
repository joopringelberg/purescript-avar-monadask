# purescript-avar-monadask

AVar in MonadAsk provides shared memory for parallel asynchronous processes.

The module `Control.Monad.AvarMonadAsk` provides an interface similar to `Control.Monad.State.Class`:

*  get
*  gets
*  put
*  modify

However, the state is kept in an AVar that is accessed through the `ask` of MonadAsk. Monads that are member of class `MonadAff` can access this AVar with `get` or `gets`, set its content with `put` and change it using `modify`.
