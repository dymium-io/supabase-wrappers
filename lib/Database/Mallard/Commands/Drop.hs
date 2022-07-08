{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Mallard.Commands.Drop
    ( dropSchema
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import qualified Data.Text                  as T
import qualified Database.Mallard
import qualified Hasql.Pool                 as H

data AppState
    = AppState
        { _statePool        :: H.Pool
        , _appPostgreSchema :: !T.Text
        }

$(makeClassy ''AppState)

instance Database.Mallard.HasPostgreConnection AppState where postgreConnection = statePool
instance Database.Mallard.HasPostgreSchema     AppState where postgreSchema = appPostgreSchema

-- | Drop postgres schema and corresponding migrations.
dropSchema
    :: (MonadIO m, MonadThrow m)
    => H.Pool -- ^ Connection to the database upon which migrations will be applied.
    -> T.Text -- ^ Schema to drop.
    -> m ()
dropSchema pool schema = do

    let initState = AppState pool schema

    void $ runStateT Database.Mallard.dropSchema initState
