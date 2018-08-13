{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}

module API where

import           Control.Monad.Freer
import           Effects.Database
import           GHC.Generics
import           Servant
import           Types               (ShortLink, URL)

newtype LinkRequest =
  LinkRequest { url :: URL }
  deriving (Show, Generic)

type ShortLinkAPI =
  "addLink" :> ReqBody '[JSON] LinkRequest :> PostCreated '[JSON] ShortLink
  :<|> Capture "shortLink" ShortLink :> Get '[JSON] NoContent

transformTestEffToHandler :: Eff '[Database] ~> Handler
transformTestEffToHandler = \action -> do
  let stateAct = runDatabasePure mockdb action
  result <- liftIO (runWithServantHandler (runEff stateAct))
  Handler $ either throwError return result
