module Main where


import qualified OpenFEC.API             as FEC
import qualified OpenFEC.QueryTypes      as QT

import           Network.HTTP.Client     (Manager, defaultManagerSettings,
                                          managerModifyRequest, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)

import           Servant.Client          (mkClientEnv, runClientM)

main :: IO ()
main = do
  let raceTypes = [QT.House]
      parties = [QT.Democrat, QT.Republican, QT.Green]
      electionYears = [2018]
      query = FEC.getCandidates raceTypes parties electionYears
      managerSettings = tlsManagerSettings -- { managerModifyRequest = \req -> print req >> return req }
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager FEC.baseUrl
  result <- runClientM query clientEnv
  case result of
    Left err     -> putStrLn $ "Query returned an error: " ++ show err
    Right candsM -> putStrLn $ show candsM
  return ()


