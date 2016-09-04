module Control.Observable.Lift
  ( liftCallback
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Observable (Observable, OBSERVABLE, free, observable)

-- | Lift a callback function into an Observable.
liftCallback :: forall a e b.
  (
    (a -> Eff (observable :: OBSERVABLE | e) Unit) ->
    Eff (observable :: OBSERVABLE | e) b
  )
  -> Eff (observable :: OBSERVABLE | e) (Observable a)
liftCallback source = observable \sink -> do
  source sink.next
  free []
