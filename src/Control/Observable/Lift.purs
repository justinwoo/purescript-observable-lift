module Control.Observable.Lift
  ( liftCallback
  , liftAff
  ) where

import Prelude
import Control.Monad.Aff (cancel, Aff, runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (error)
import Control.Observable (Observable, OBSERVABLE, free, observable)


-- | Lift a callback function into an Observable.
liftCallback :: forall e a b.
  (
    (a -> Eff (observable :: OBSERVABLE | e) Unit) ->
    Eff (observable :: OBSERVABLE | e) b
  )
  -> Eff (observable :: OBSERVABLE | e) (Observable a)
liftCallback source = observable \sink -> do
  source sink.next
  free []

-- | Lift an Aff into an Observable.
liftAff :: forall e a. Aff (observable :: OBSERVABLE | e) a -> Eff (observable :: OBSERVABLE | e) (Observable a)
liftAff aff = observable \sink -> do
    canceler <- runAff sink.error sink.next aff
    free [{unsubscribe: void $ runAff (const $ pure unit) (const $ pure unit) (canceler `cancel` (error "Unsubscribed"))}]
