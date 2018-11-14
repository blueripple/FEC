{-
   OpenFEC

   This API allows you to explore the way candidates and committees fund their campaigns.    The FEC API is a RESTful web service supporting full-text and field-specific searches on FEC data. [Bulk downloads](https://www.fec.gov/data/advanced/?tab=bulk-data) are available on the current site. Information is tied to the underlying forms by file ID and image ID. Data is updated nightly.    There is a lot of data, but a good place to start is to use search to find interesting candidates and committees. Then, you can use their IDs to find report or line item details with the other endpoints. If you are interested in individual donors, check out contributor information in schedule_a.    Get an [API key here](https://api.data.gov/signup/). That will enable you to place up to 1,000 calls an hour. Each call is limited to 100 results per page. You can email questions, comments or a request to get a key for 120 calls per minute to [APIinfo@fec.gov](mailto:apiinfo@fec.gov). You can also ask questions and discuss the data in the [FEC data Google Group](https://groups.google.com/forum/#!forum/fec-data). API changes will also be added to this group in advance of the change.       The model definitions and schema are available at [/swagger](/swagger/). This is useful for making wrappers and exploring the data.    A few restrictions limit the way you can use FEC data. For example, you can’t use contributor lists for commercial purposes or to solicit donations. [Learn more here](https://transition.fec.gov/pages/brochures/saleuse.shtml).    [View our source code](https://github.com/fecgov/openFEC). We welcome issues and pull requests!

   OpenAPI spec version: 2.0
   OpenFEC API version: 1.0
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : OpenFEC.API.CommunicationCost
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OpenFEC.API.CommunicationCost where

import OpenFEC.Core
import OpenFEC.MimeTypes
import OpenFEC.Model as M

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Data as P (Typeable, TypeRep, typeOf, typeRep)
import qualified Data.Foldable as P
import qualified Data.Map as Map
import qualified Data.Maybe as P
import qualified Data.Proxy as P (Proxy(..))
import qualified Data.Set as Set
import qualified Data.String as P
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as TI
import qualified Network.HTTP.Client.MultipartFormData as NH
import qualified Network.HTTP.Media as ME
import qualified Network.HTTP.Types as NH
import qualified Web.FormUrlEncoded as WH
import qualified Web.HttpApiData as WH

import Data.Text (Text)
import GHC.Base ((<|>))

import Prelude ((==),(/=),($), (.),(<$>),(<*>),(>>=),Maybe(..),Bool(..),Char,Double,FilePath,Float,Int,Integer,String,fmap,undefined,mempty,maybe,pure,Monad,Applicative,Functor)
import qualified Prelude as P

-- * Operations


-- ** CommunicationCost

-- *** committeeCommitteeIdCommunicationCostsByCandidateGet

-- | @GET \/committee\/{committee_id}\/communication_costs\/by_candidate\/@
-- 
-- Communication cost aggregated by candidate ID and committee ID.
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
committeeCommitteeIdCommunicationCostsByCandidateGet 
  :: ApiKey -- ^ "apiKey" -   API key for https://api.data.gov. Get one at https://api.data.gov/signup. 
  -> CommitteeIdText -- ^ "committeeId" -   A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits. 
  -> OpenFECRequest CommitteeCommitteeIdCommunicationCostsByCandidateGet MimeNoContent CommunicationCostByCandidatePage MimeJSON
committeeCommitteeIdCommunicationCostsByCandidateGet (ApiKey apiKey) (CommitteeIdText committeeId) =
  _mkRequest "GET" ["/committee/",toPath committeeId,"/communication_costs/by_candidate/"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setQuery` toQuery ("api_key", Just apiKey)

data CommitteeCommitteeIdCommunicationCostsByCandidateGet  

-- | /Optional Param/ "district" - Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
instance HasOptionalParam CommitteeCommitteeIdCommunicationCostsByCandidateGet DistrictText where
  applyOptionalParam req (DistrictText xs) =
    req `setQuery` toQuery ("district", Just xs)

-- | /Optional Param/ "sort_null_only" - Toggle that filters out all rows having sort column that is non-null
instance HasOptionalParam CommitteeCommitteeIdCommunicationCostsByCandidateGet SortNullOnly where
  applyOptionalParam req (SortNullOnly xs) =
    req `setQuery` toQuery ("sort_null_only", Just xs)

-- | /Optional Param/ "office" - Federal office candidate runs for: H, S or P
instance HasOptionalParam CommitteeCommitteeIdCommunicationCostsByCandidateGet Office2 where
  applyOptionalParam req (Office2 xs) =
    req `setQuery` toQuery ("office", Just xs)

-- | /Optional Param/ "sort" - Provide a field to sort by. Use - for descending order.
instance HasOptionalParam CommitteeCommitteeIdCommunicationCostsByCandidateGet SortText where
  applyOptionalParam req (SortText xs) =
    req `setQuery` toQuery ("sort", Just xs)

-- | /Optional Param/ "candidate_id" -  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office. 
instance HasOptionalParam CommitteeCommitteeIdCommunicationCostsByCandidateGet CandidateId where
  applyOptionalParam req (CandidateId xs) =
    req `setQuery` toQueryColl MultiParamArray ("candidate_id", Just xs)

-- | /Optional Param/ "page" - For paginating through results, starting at page 1
instance HasOptionalParam CommitteeCommitteeIdCommunicationCostsByCandidateGet Page where
  applyOptionalParam req (Page xs) =
    req `setQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "support_oppose" - Support or opposition
instance HasOptionalParam CommitteeCommitteeIdCommunicationCostsByCandidateGet SupportOppose where
  applyOptionalParam req (SupportOppose xs) =
    req `setQuery` toQuery ("support_oppose", Just xs)

-- | /Optional Param/ "state" - US state or territory where a candidate runs for office
instance HasOptionalParam CommitteeCommitteeIdCommunicationCostsByCandidateGet StateText where
  applyOptionalParam req (StateText xs) =
    req `setQuery` toQuery ("state", Just xs)

-- | /Optional Param/ "per_page" - The number of results returned per page. Defaults to 20.
instance HasOptionalParam CommitteeCommitteeIdCommunicationCostsByCandidateGet PerPage where
  applyOptionalParam req (PerPage xs) =
    req `setQuery` toQuery ("per_page", Just xs)

-- | /Optional Param/ "cycle" -  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year. 
instance HasOptionalParam CommitteeCommitteeIdCommunicationCostsByCandidateGet Cycle where
  applyOptionalParam req (Cycle xs) =
    req `setQuery` toQueryColl MultiParamArray ("cycle", Just xs)

-- | /Optional Param/ "sort_hide_null" - Hide null values on sorted column(s).
instance HasOptionalParam CommitteeCommitteeIdCommunicationCostsByCandidateGet SortHideNull where
  applyOptionalParam req (SortHideNull xs) =
    req `setQuery` toQuery ("sort_hide_null", Just xs)

-- | /Optional Param/ "election_full" - Aggregate values over full election period
instance HasOptionalParam CommitteeCommitteeIdCommunicationCostsByCandidateGet ElectionFull where
  applyOptionalParam req (ElectionFull xs) =
    req `setQuery` toQuery ("election_full", Just xs)
-- | @application/json@
instance Produces CommitteeCommitteeIdCommunicationCostsByCandidateGet MimeJSON


-- *** communicationCostsByCandidateGet

-- | @GET \/communication_costs\/by_candidate\/@
-- 
-- Communication cost aggregated by candidate ID and committee ID.
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
communicationCostsByCandidateGet 
  :: ApiKey -- ^ "apiKey" -   API key for https://api.data.gov. Get one at https://api.data.gov/signup. 
  -> OpenFECRequest CommunicationCostsByCandidateGet MimeNoContent CommunicationCostByCandidatePage MimeJSON
communicationCostsByCandidateGet (ApiKey apiKey) =
  _mkRequest "GET" ["/communication_costs/by_candidate/"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setQuery` toQuery ("api_key", Just apiKey)

data CommunicationCostsByCandidateGet  

-- | /Optional Param/ "district" - Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
instance HasOptionalParam CommunicationCostsByCandidateGet DistrictText where
  applyOptionalParam req (DistrictText xs) =
    req `setQuery` toQuery ("district", Just xs)

-- | /Optional Param/ "sort_null_only" - Toggle that filters out all rows having sort column that is non-null
instance HasOptionalParam CommunicationCostsByCandidateGet SortNullOnly where
  applyOptionalParam req (SortNullOnly xs) =
    req `setQuery` toQuery ("sort_null_only", Just xs)

-- | /Optional Param/ "office" - Federal office candidate runs for: H, S or P
instance HasOptionalParam CommunicationCostsByCandidateGet Office2 where
  applyOptionalParam req (Office2 xs) =
    req `setQuery` toQuery ("office", Just xs)

-- | /Optional Param/ "sort" - Provide a field to sort by. Use - for descending order.
instance HasOptionalParam CommunicationCostsByCandidateGet SortText where
  applyOptionalParam req (SortText xs) =
    req `setQuery` toQuery ("sort", Just xs)

-- | /Optional Param/ "candidate_id" -  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office. 
instance HasOptionalParam CommunicationCostsByCandidateGet CandidateId where
  applyOptionalParam req (CandidateId xs) =
    req `setQuery` toQueryColl MultiParamArray ("candidate_id", Just xs)

-- | /Optional Param/ "page" - For paginating through results, starting at page 1
instance HasOptionalParam CommunicationCostsByCandidateGet Page where
  applyOptionalParam req (Page xs) =
    req `setQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "support_oppose" - Support or opposition
instance HasOptionalParam CommunicationCostsByCandidateGet SupportOppose where
  applyOptionalParam req (SupportOppose xs) =
    req `setQuery` toQuery ("support_oppose", Just xs)

-- | /Optional Param/ "state" - US state or territory where a candidate runs for office
instance HasOptionalParam CommunicationCostsByCandidateGet StateText where
  applyOptionalParam req (StateText xs) =
    req `setQuery` toQuery ("state", Just xs)

-- | /Optional Param/ "per_page" - The number of results returned per page. Defaults to 20.
instance HasOptionalParam CommunicationCostsByCandidateGet PerPage where
  applyOptionalParam req (PerPage xs) =
    req `setQuery` toQuery ("per_page", Just xs)

-- | /Optional Param/ "cycle" -  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year. 
instance HasOptionalParam CommunicationCostsByCandidateGet Cycle where
  applyOptionalParam req (Cycle xs) =
    req `setQuery` toQueryColl MultiParamArray ("cycle", Just xs)

-- | /Optional Param/ "sort_hide_null" - Hide null values on sorted column(s).
instance HasOptionalParam CommunicationCostsByCandidateGet SortHideNull where
  applyOptionalParam req (SortHideNull xs) =
    req `setQuery` toQuery ("sort_hide_null", Just xs)

-- | /Optional Param/ "election_full" - Aggregate values over full election period
instance HasOptionalParam CommunicationCostsByCandidateGet ElectionFull where
  applyOptionalParam req (ElectionFull xs) =
    req `setQuery` toQuery ("election_full", Just xs)
-- | @application/json@
instance Produces CommunicationCostsByCandidateGet MimeJSON


-- *** communicationCostsGet

-- | @GET \/communication-costs\/@
-- 
--  52 U.S.C. 30118 allows \"communications by a corporation to its stockholders and executive or administrative personnel and their families or by a labor organization to its members and their families on any subject,\" including the express advocacy of the election or defeat of any Federal candidate.  The costs of such communications must be reported to the Federal Election Commission under certain circumstances. 
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
communicationCostsGet 
  :: ApiKey -- ^ "apiKey" -   API key for https://api.data.gov. Get one at https://api.data.gov/signup. 
  -> OpenFECRequest CommunicationCostsGet MimeNoContent CommunicationCostPage MimeJSON
communicationCostsGet (ApiKey apiKey) =
  _mkRequest "GET" ["/communication-costs/"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setQuery` toQuery ("api_key", Just apiKey)

data CommunicationCostsGet  

-- | /Optional Param/ "max_date" - Maximum date
instance HasOptionalParam CommunicationCostsGet MaxDate where
  applyOptionalParam req (MaxDate xs) =
    req `setQuery` toQuery ("max_date", Just xs)

-- | /Optional Param/ "sort_null_only" - Toggle that filters out all rows having sort column that is non-null
instance HasOptionalParam CommunicationCostsGet SortNullOnly where
  applyOptionalParam req (SortNullOnly xs) =
    req `setQuery` toQuery ("sort_null_only", Just xs)

-- | /Optional Param/ "sort" - Provide a field to sort by. Use - for descending order.
instance HasOptionalParam CommunicationCostsGet SortText where
  applyOptionalParam req (SortText xs) =
    req `setQuery` toQuery ("sort", Just xs)

-- | /Optional Param/ "support_oppose_indicator" - Support or opposition
instance HasOptionalParam CommunicationCostsGet SupportOpposeIndicator where
  applyOptionalParam req (SupportOpposeIndicator xs) =
    req `setQuery` toQueryColl MultiParamArray ("support_oppose_indicator", Just xs)

-- | /Optional Param/ "min_date" - Minimum date
instance HasOptionalParam CommunicationCostsGet MinDate where
  applyOptionalParam req (MinDate xs) =
    req `setQuery` toQuery ("min_date", Just xs)

-- | /Optional Param/ "page" - For paginating through results, starting at page 1
instance HasOptionalParam CommunicationCostsGet Page where
  applyOptionalParam req (Page xs) =
    req `setQuery` toQuery ("page", Just xs)
instance HasOptionalParam CommunicationCostsGet MaxImageNumber where
  applyOptionalParam req (MaxImageNumber xs) =
    req `setQuery` toQuery ("max_image_number", Just xs)

-- | /Optional Param/ "image_number" - The image number of the page where the schedule item is reported
instance HasOptionalParam CommunicationCostsGet ImageNumber where
  applyOptionalParam req (ImageNumber xs) =
    req `setQuery` toQueryColl MultiParamArray ("image_number", Just xs)

-- | /Optional Param/ "max_amount" - Filter for all amounts less than a value.
instance HasOptionalParam CommunicationCostsGet MaxAmount where
  applyOptionalParam req (MaxAmount xs) =
    req `setQuery` toQuery ("max_amount", Just xs)

-- | /Optional Param/ "sort_hide_null" - Hide null values on sorted column(s).
instance HasOptionalParam CommunicationCostsGet SortHideNull where
  applyOptionalParam req (SortHideNull xs) =
    req `setQuery` toQuery ("sort_hide_null", Just xs)

-- | /Optional Param/ "candidate_id" -  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office. 
instance HasOptionalParam CommunicationCostsGet CandidateId where
  applyOptionalParam req (CandidateId xs) =
    req `setQuery` toQueryColl MultiParamArray ("candidate_id", Just xs)

-- | /Optional Param/ "min_amount" - Filter for all amounts greater than a value.
instance HasOptionalParam CommunicationCostsGet MinAmount where
  applyOptionalParam req (MinAmount xs) =
    req `setQuery` toQuery ("min_amount", Just xs)

-- | /Optional Param/ "line_number" - Filter for form and line number using the following format: `FORM-LINENUMBER`.  For example an argument such as `F3X-16` would filter down to all entries from form `F3X` line number `16`.
instance HasOptionalParam CommunicationCostsGet LineNumber where
  applyOptionalParam req (LineNumber xs) =
    req `setQuery` toQuery ("line_number", Just xs)
instance HasOptionalParam CommunicationCostsGet MinImageNumber where
  applyOptionalParam req (MinImageNumber xs) =
    req `setQuery` toQuery ("min_image_number", Just xs)

-- | /Optional Param/ "per_page" - The number of results returned per page. Defaults to 20.
instance HasOptionalParam CommunicationCostsGet PerPage where
  applyOptionalParam req (PerPage xs) =
    req `setQuery` toQuery ("per_page", Just xs)

-- | /Optional Param/ "committee_id" -  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits. 
instance HasOptionalParam CommunicationCostsGet CommitteeId where
  applyOptionalParam req (CommitteeId xs) =
    req `setQuery` toQueryColl MultiParamArray ("committee_id", Just xs)
-- | @application/json@
instance Produces CommunicationCostsGet MimeJSON

