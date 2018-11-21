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
import           Data.Vector            (Vector, empty)

import qualified OpenFEC.QueryTypes     as QT
import           OpenFEC.Types          (Candidate, candidateFromResultJSON)

baseUrl = BaseUrl Https "api.open.fec.gov" 443 "/v1"

data FEC_Routes route = FEC_Routes
  {
    _candidates :: route :- "candidates" :> QueryParam "api_key" Text :> QueryParams "office" Text :> QueryParams "party" Text :> QueryParams "election_year" Int :> QueryParam "per_page" Int :> QueryParam "page" Int :> Get '[JSON] QT.Page
  }
  deriving (Generic)

fecClients :: FEC_Routes (AsClientT ClientM)
fecClients = genericClient

-- specific useful queries

getCandidatesPage :: [QT.Office] -> [QT.Party] -> [QT.ElectionYear] -> QT.PageNumber -> ClientM QT.Page
getCandidatesPage offices parties years page =
  let ttql = QT.toTextQueryList
  in (_candidates fecClients) (Just QT.fecApiKey) (ttql offices) (ttql parties) years (Just QT.fecMaxPerPage) (Just page)

getCandidates :: [QT.Office] -> [QT.Party] -> [QT.ElectionYear] -> ClientM (Maybe (Vector Candidate))
getCandidates offices parties years =
    let getOnePage = getCandidatesPage offices parties years
    in QT.getAllPages QT.SkipFailed getOnePage candidateFromResultJSON



