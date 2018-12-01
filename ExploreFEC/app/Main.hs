{-# LANGUAGE OverloadedStrings #-}
module Main where


import qualified OpenFEC.API               as FEC
import qualified OpenFEC.Types             as FEC

import           ExploreFEC.Data.Spending  (getHouseRaceSpending,
                                            getPresidentialRaceSpending,
                                            getSenateRaceSpending)

import           Control.Monad             (sequence)
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (encodeFile)
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Network.HTTP.Client       (Manager, defaultManagerSettings,
                                            managerModifyRequest, newManager)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Servant.Client            (ClientM, mkClientEnv, runClientM)
import qualified Text.PrettyPrint.Tabulate as PP



main :: IO ()
main = do
  let raceTypes = [FEC.House]
      parties = [FEC.Democrat, FEC.Republican, FEC.Green]
      electionYears = [2018]
--      query = FEC.getCandidates raceType parties electionYears
--      query = FEC.getFilings "H8NY11113" True [2018]
--      query = FEC.getCommittees "H8NY11113" []
--      query = FEC.getReports "C00652248" [] [] []
--      query = FEC.getDisbursements "C00652248" 2018
--      query = FEC.getIndependentExpendituresByCandidate "H6NY11174" [2018]
--      query = FEC.getPartyExpenditures "H0CA27085" []
      query = getHouseRaceSpending "NY" 11 2018
--      query = getSenateRaceSpending "FL" 2018
      managerSettings = tlsManagerSettings --{ managerModifyRequest = \req -> print req >> return req }
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager FEC.baseUrl
  result <- runClientM query clientEnv
  case result of
    Left err -> putStrLn $ "Query returned an error: " ++ show err
    Right x  -> encodeFile "NY-11.json" x
--    Right x  -> PP.printTable x
  return ()


