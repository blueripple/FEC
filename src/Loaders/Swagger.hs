{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Loaders.Swagger where

import           Servant.API
import           Servant.Client
--import           Servant.Common.Req

import           Network.HTTP.Client     (Manager, defaultManagerSettings,
                                          newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Web.HttpApiData         (ToHttpApiData (..))

--import           Control.Error       (EitherT)
import           Control.Lens
import           Data.Aeson              (FromJSON)
import           Data.Maybe              (maybe)
import           Data.Monoid             ((<>))
import           Data.Proxy              (Proxy (Proxy))
import qualified Data.Swagger            as SWAG
import qualified Data.Text               as T
import           GHC.TypeLits            (KnownSymbol, Symbol)


type FEC_API = "swagger" :> Get '[JSON] SWAG.Swagger

fecAPI :: Proxy FEC_API
fecAPI = Proxy

getSwagger :: ClientM SWAG.Swagger
getSwagger = client fecAPI

testSwagger :: IO ()
testSwagger = do
  manager <- newManager tlsManagerSettings
  res <- runClientM getSwagger (mkClientEnv manager (BaseUrl Https "api.open.fec.gov" 443 ""))
  case res of
    Left err       -> putStrLn $ "Error: " ++ show err
    Right swagger -> do
      let contactInfoM = swagger ^. (SWAG.info . SWAG.contact)
      putStrLn $ "Contact Info: " ++ show (maybe mempty id contactInfoM)
      let endPointPaths = swagger ^. SWAG.paths
      putStrLn $ "Endpoints: " ++ show endPointPaths



