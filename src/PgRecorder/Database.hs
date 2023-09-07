{-# LANGUAGE OverloadedStrings #-}
{-| This module encapsulates knowledge about the SQL commands and the Hasql interface.
-}
module PgRecorder.Database
  ( callProcedure
  , listen
  , unlisten
  , waitForNotifications
  , PgIdentifier
  , toPgIdentifier
  ) where

import Protolude
import qualified Hasql.Pool as Pool (Pool, UsageError, use)
-- Hasql.Session(query) is now Hasql.Session(statement)
import Hasql.Session(statement)
-- Hasql.Query(statement) is now Hasql.Statement(Statement)
-- | We were using Hasql.Query, but this should now use Hasql.Statement
-- | See how this was addressed in another repository
-- |   https://github.com/tvh/hasql-migration/pull/5/files
import Hasql.Statement
import qualified Hasql.Connection as Connection (Connection, withLibPQConnection)
import qualified Hasql.Decoders as HD
import qualified Hasql.Encoders as HE
import qualified Database.PostgreSQL.LibPQ as PQ
import qualified Data.Either.Combinators
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL8
import qualified Data.ByteString.Search (replace)
import qualified Data.Functor.Contravariant (contramap)

-- B.toStrict :: ByteString -> BL.ByteString

newtype Error = NotifyError Text deriving Show

-- | A wrapped bytestring that represents a properly escaped and quoted PostgreSQL identifier
newtype PgIdentifier = PgIdentifier ByteString deriving (Show)

-- | Given a PgIdentifier returns the wrapped bytestring
fromPgIdentifier :: PgIdentifier -> ByteString
fromPgIdentifier (PgIdentifier bs) = bs

-- | Given a bytestring returns a properly quoted and escaped PgIdentifier
toPgIdentifier :: ByteString -> PgIdentifier
toPgIdentifier x = PgIdentifier $ "\"" <> strictlyReplaceQuotes (trimNullChars x) <> "\""
  where
    trimNullChars :: ByteString -> ByteString
    trimNullChars = B.takeWhile (/= '\x0')
    strictlyReplaceQuotes :: ByteString -> ByteString
    strictlyReplaceQuotes = toS::ByteString . replace "\"" ("\"\"" :: ByteString)



-- | Given a Hasql Pool, a channel and a message sends a notify command to the database
callProcedure :: Pool.Pool -> PgIdentifier -> ByteString -> ByteString -> IO (Either Error ())
callProcedure pool procedure channel mesg =
   mapError <$> Pool.use pool (statement (toS channel, toS mesg) callStatement)
   where
     mapError :: Either Pool.UsageError () -> Either Error ()
     mapError = Data.Either.Combinators.mapLeft (NotifyError . show)
     -- Note: HD.unit was renamed to HD.noResult
     -- See https://github.com/nikita-volkov/hasql/commit/db4261ddb5c334b70829064426269eadfaa56bfd
     callStatement = Statement ("SELECT " <> fromPgIdentifier procedure <> "($1, $2)") encoder HD.noResult False
     -- HE.value is now HE.param
     -- encoder = Data.Functor.Contravariant.contramap fst (HE.param HE.text) <> Data.Functor.Contravariant.contramap snd (HE.param HE.text)
     encoder = Data.Functor.Contravariant.contramap fst (HE.param (HE.nonNullable (HE.text))) 
            <> Data.Functor.Contravariant.contramap snd (HE.param (HE.nonNullable (HE.text)))

-- | Given a Hasql Connection and a channel sends a listen command to the database
listen :: Connection.Connection -> PgIdentifier -> IO ()
listen con channel =
  void $ Connection.withLibPQConnection con execListen
  where
    execListen pqCon = void $ PQ.exec pqCon $ "LISTEN " <> fromPgIdentifier channel

-- | Given a Hasql Connection and a channel sends a unlisten command to the database
unlisten :: Connection.Connection -> PgIdentifier -> IO ()
unlisten con channel =
  void $ Connection.withLibPQConnection con execListen
  where
    execListen pqCon = void $ PQ.exec pqCon $ "UNLISTEN " <> fromPgIdentifier channel


{- | Given a function that handles notifications and a Hasql connection forks a thread that listens on the database connection and calls the handler everytime a message arrives.

   The message handler passed as first argument needs two parameters channel and payload.
-}

waitForNotifications :: (ByteString -> ByteString -> IO()) -> Connection.Connection -> IO ()
waitForNotifications sendNotification con =
  Connection.withLibPQConnection con $ void . forever . pqFetch
  where
    pqFetch pqCon = do
      mNotification <- PQ.notifies pqCon
      case mNotification of
        Nothing -> do
          mfd <- PQ.socket pqCon
          case mfd of
            Nothing  -> panic "Error checking for PostgreSQL notifications"
            Just fd -> do
              void $ threadWaitRead fd
              void $ PQ.consumeInput pqCon
        Just notification ->
           sendNotification (PQ.notifyRelname notification) (PQ.notifyExtra notification)
