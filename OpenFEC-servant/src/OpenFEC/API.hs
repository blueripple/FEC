{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
module OpenFEC.API where


import           Servant
import           Servant.API.Generic
import           Servant.Client
import           Servant.Client.Generic

import           Control.Exception      (throwIO)
import qualified Data.Aeson             as A
import           Data.Text              (Text)

import qualified OpenFEC.QueryTypes     as QT

data FEC_Routes route = FEC_Routes
  {
    _candidates :: route :- "candidates" :> QueryParam "api_key" Text :> QueryParams "party" Text :> QueryParams "office" Text :> QueryParams "election_year" Int :> Get '[JSON] A.Value
  }
  deriving (Generic)

--fecApi :: Proxy (ToServantApi FEC_Routes)
--fecApi = genericApi (Proxy :: Proxy FEC_Routes)

fecClients :: FEC_Routes (AsClientT ClientM)
fecClients = genericClient

getCandidates :: QT.APIKey -> [QT.Party] -> [QT.Office] -> [QT.ElectionYear] -> ClientM A.Value
getCandidates apiKey parties offices years =
  let ttql = QT.toTextQueryList
  in (_candidates fecClients) (Just apiKey) (ttql parties) (ttql offices) years

{-
testCandidates :: IO ()
testCandidates = do
  let apiKey = "jjt0sf4z7FVpSAXoTndlnLKl0sDZUFcf3PQjpXrW"
      races = toTextQueryList [House]
      parties = toTextQueryList [Democrat, Republican]
      years = [2016]
  manager <- newManager tlsManagerSettings
  res <- runClientM getHouseDems (mkClientEnv manager (BaseUrl Https "api.open.fec.gov" 443 "/v1"))
-}




