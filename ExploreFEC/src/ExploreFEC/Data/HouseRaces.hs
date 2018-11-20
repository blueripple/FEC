{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module ExploreFEC.Data.HouseRaces where

import           Servant.API
import           Servant.Client

import           Data.Text               (Text)
import           Network.HTTP.Client     (Manager, defaultManagerSettings,
                                          managerModifyRequest, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified OpenFEC.QueryTypes      as QT
import           Web.HttpApiData         (ToHttpApiData (..))

{-
import           Control.Lens
import           Control.Monad           ((>>=))
import qualified Data.Aeson              as Aeson
import qualified Data.Aeson.Lens         as Aeson
import           Data.Maybe              (maybe)
import           Data.Monoid             ((<>))
import           Data.Proxy              (Proxy (Proxy))
import qualified Data.Swagger            as SWAG
import qualified Data.Text               as T
import           GHC.TypeLits            (KnownSymbol, Symbol)
-}

import qualified OpenFEC.API             as FEC
--import qualified OpenFEC.Types              as FEC



testHouseRaces :: IO ()
testHouseRaces = do
  let apiKey :: QT.APIKey = "jjt0sf4z7FVpSAXoTndlnLKl0sDZUFcf3PQjpXrW"
      raceTypes = [QT.House]
      parties = [QT.Democrat, QT.Republican, QT.Green]
      electionYears = [2018]
      candidatesQuery = FEC.getCandidates apiKey parties raceTypes electionYears
      managerSettings = tlsManagerSettings { managerModifyRequest = \req -> print req >> return req }
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager (BaseUrl Https "api.open.fec.gov" 443 "/v1")
  result <- runClientM candidatesQuery clientEnv
  case result of
    Left err   -> putStrLn $ "Query returned an error: " ++ show err
    Right json -> putStrLn $ show json
  return ()


{-  res <- runClientM getSwagger (mkClientEnv manager (BaseUrl Https "api.open.fec.gov" 443 ""))
  case res of
    Left err       -> putStrLn $ "Error: " ++ show err
    Right swagger -> do
      let contactInfoM = swagger ^. (SWAG.info . SWAG.contact)
      putStrLn $ "Contact Info: " ++ show (maybe mempty id contactInfoM)
      let getOpsM = view SWAG.get
          opsSummaryM x = x >>= SWAG._operationSummary
          showGetOps path pathInfo =
            let hasGet = getOpsM pathInfo
                getTxt = maybe (T.pack "No GET") (const $ T.pack "GET") hasGet
                suffix = ": " <> getTxt <> (maybe ("") ("=" <>) . opsSummaryM . getOpsM $ pathInfo)
            in [(T.pack path) <> suffix]
          endPointPaths = iconcatMap showGetOps (swagger ^. SWAG.paths)
      putStrLn $ "Endpoints: " ++ show endPointPaths
-}



