{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}

module Database.Mallard.Commands.ConfirmChecksums
    ( confirmChecksums
    ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.State.Strict
import           Data.Text                  (Text)
import           Database.Mallard
import qualified Hasql.Pool                 as H
import           Path
import           Path.IO

data AppState
    = AppState
        { _statePool   :: H.Pool
        , _stateSchema :: !Text
        }

$(makeClassy ''AppState)

instance HasPostgreConnection AppState where postgreConnection = statePool
instance HasPostgreSchema AppState where postgreSchema = stateSchema

-- | Compairs the checksums of each migration in the provided directory to see
-- if they have changed since being applied to the database.
confirmChecksums
    :: (MonadIO m, MonadThrow m)
    => H.Pool -- ^ Connection to the database upon which migrations will be applied.
    -> Path b Dir -- ^ Directory which contains migration scripts.
    -> Text -- ^ Postgres Schema where migration belongs
    -> m ()
confirmChecksums pool dir postgresSchema = do
    absDir <- makeAbsolute dir
    let initState = AppState pool postgresSchema

    void $ flip runStateT initState $ do
        (mPlanned, _) <- importDirectory absDir
        mApplied <- getAppliedMigrations

        cs <- checkAppliedMigrations mPlanned mApplied
        mapM_ showComparison cs

showComparison :: (MonadIO m) => DigestComparison -> m ()
showComparison (DigestComparison n p _ a match) = liftIO $ do
    putStrLn $ "" <> show n <> " : " <> (if match then "Valid" else "Invalid")
    putStrLn   ""
    putStrLn $ "    " <> show p
    putStrLn $ "    " <> show a
    putStrLn   ""
