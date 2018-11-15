{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module OpenFEC.API where


import           Servant.API
import           Servant.Client
--import           Servant.Common.Req

import           Network.HTTP.Client     (Manager, defaultManagerSettings,
                                          newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Web.HttpApiData         (ToHttpApiData (..))

--import           Control.Error       (EitherT)
import           Control.Lens
import           Control.Monad           ((>>=))
import           Data.Aeson              (FromJSON)
import qualified Data.Aeson              as A
import           Data.Maybe              (maybe)
import           Data.Monoid             ((<>))
import           Data.Proxy              (Proxy (Proxy))
import qualified Data.Swagger            as SWAG
import qualified Data.Text               as T
import           GHC.TypeLits            (KnownSymbol, Symbol)


type FEC_API = "swagger" :> Get '[JSON] SWAG.Swagger
  :<|> "candidates" :> QueryParam "api_key" String :> QueryParam "party" [Text] :> QueryParam "office" [Text] :> QueryParam "election_year" [Int] :> Get '[JSON] A.Value

getSwagger :: ClientM SWAG.Swagger
getCandidates :: APIKey -> Maybe [String] -> Maybe [String] -> Maybe [Int] -> ClientM A.Value
getSwagger :<|> getCandidates = client fecAPI

testCandidates :: IO ()
testCandidates = do
  let getHouseDems = getCandidates "jjt0sf4z7FVpSAXoTndlnLKl0sDZUFcf3PQjpXrW" 
  manager <- newManager tlsManagerSettings
  res <- runClientM getHoueDems (mkClientEnv manager (BaseUrl Https "api.open.fec.gov" 443 ""))
  



