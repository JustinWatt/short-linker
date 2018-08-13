{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Effects.Database where

import           Control.Monad.Freer       (Eff, Member, reinterpret, send)
import           Control.Monad.Freer.State
import           Data.Function             ((&))
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as M
import           Data.Text                 (Text)
import           Lens.Micro
import           Lens.Micro.TH             (makeLenses)
import           Types (ShortLink(..), URL(..))

data Database r where
  AddSite   :: URL       -> Database ShortLink
  FetchSite :: ShortLink -> Database (Maybe URL)

addSite :: Member Database effs => URL -> Eff effs ShortLink
addSite = send . AddSite

fetchSite :: Member Database effs => ShortLink -> Eff effs (Maybe URL)
fetchSite = send . FetchSite

data MockDb =
  MockDb { _db   :: Map ShortLink URL
         , _keys :: [Text]
         } deriving (Show)

makeLenses ''MockDb

popOrError :: String -> [a] -> a
popOrError errorMessage [] = error errorMessage
popOrError _ (x:_)         = x

runDatabasePure ::
  forall effs a. MockDb -> Eff (Database ': effs) a -> Eff effs (a, MockDb)
runDatabasePure dbState req =
  reinterpret go req
  & runState dbState
  where
    go :: Database v -> Eff (State MockDb ': effs) v
    go (AddSite u)   = do
      key <- popOrError "Not Enough Keys" <$> gets _keys
      _   <- modify $ \s -> s
                & keys %~ tail
                & db %~ M.insert (ShortLink key) u
      return (ShortLink key)

    go (FetchSite s) = M.lookup s <$> gets _db
