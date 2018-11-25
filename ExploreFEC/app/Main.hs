module Main where


import qualified OpenFEC.API             as FEC
import qualified OpenFEC.QueryTypes      as FEC
import qualified OpenFEC.Types as FEC

import           Network.HTTP.Client     (Manager, defaultManagerSettings,
                                          managerModifyRequest, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client          (mkClientEnv, runClientM)
import qualified Data.Text as T



main :: IO ()
main = do
  let raceTypes = [FEC.House]
      parties = [FEC.Democrat, FEC.Republican, FEC.Green]
      electionYears = [2018]
      query = FEC.getCandidates raceTypes parties electionYears
      managerSettings = tlsManagerSettings -- { managerModifyRequest = \req -> print req >> return req }
  manager <- newManager managerSettings
  let clientEnv = mkClientEnv manager FEC.baseUrl
  result <- runClientM query clientEnv
  case result of
    Left err     -> putStrLn $ "Query returned an error: " ++ show err
    Right candsM -> putStrLn $ maybe "Query Error/No Data" (T.unpack . FEC.candidateTable) candsM
  return ()


