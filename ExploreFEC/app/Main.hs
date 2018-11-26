{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified OpenFEC.API             as FEC
import qualified OpenFEC.QueryTypes      as FEC
import qualified OpenFEC.Types           as FEC

import qualified Data.Text               as T
import           Network.HTTP.Client     (Manager, defaultManagerSettings,
                                          managerModifyRequest, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (mkClientEnv, runClientM)



main :: IO ()
main = do
  let raceTypes = [FEC.House]
      parties = [FEC.Democrat, FEC.Republican, FEC.Green]
      electionYears = [2018]
--      query = FEC.getCandidates raceType parties electionYears
--      query = FEC.getFilings "H8NY11113" True [2018]
--      query = FEC.getCommittees "H8NY11113" []
      query = FEC.getReports "C00652248" [] [] []
      managerSettings = tlsManagerSettings { managerModifyRequest = \req -> print req >> return req }
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager FEC.baseUrl
  result <- runClientM query clientEnv
  case result of
    Left err          -> putStrLn $ "Query returned an error: " ++ show err
    Right reportM -> putStrLn $ maybe "Query Error/No Data" (T.unpack . FEC.reportTable) reportM
  return ()


