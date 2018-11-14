{-
   OpenFEC

   This API allows you to explore the way candidates and committees fund their campaigns.    The FEC API is a RESTful web service supporting full-text and field-specific searches on FEC data. [Bulk downloads](https://www.fec.gov/data/advanced/?tab=bulk-data) are available on the current site. Information is tied to the underlying forms by file ID and image ID. Data is updated nightly.    There is a lot of data, but a good place to start is to use search to find interesting candidates and committees. Then, you can use their IDs to find report or line item details with the other endpoints. If you are interested in individual donors, check out contributor information in schedule_a.    Get an [API key here](https://api.data.gov/signup/). That will enable you to place up to 1,000 calls an hour. Each call is limited to 100 results per page. You can email questions, comments or a request to get a key for 120 calls per minute to [APIinfo@fec.gov](mailto:apiinfo@fec.gov). You can also ask questions and discuss the data in the [FEC data Google Group](https://groups.google.com/forum/#!forum/fec-data). API changes will also be added to this group in advance of the change.       The model definitions and schema are available at [/swagger](/swagger/). This is useful for making wrappers and exploring the data.    A few restrictions limit the way you can use FEC data. For example, you can’t use contributor lists for commercial purposes or to solicit donations. [Learn more here](https://transition.fec.gov/pages/brochures/saleuse.shtml).    [View our source code](https://github.com/fecgov/openFEC). We welcome issues and pull requests!

   OpenAPI spec version: 2.0
   OpenFEC API version: 1.0
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : OpenFEC.API.Legal
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OpenFEC.API.Legal where

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


-- ** Legal

-- *** legalSearchGet

-- | @GET \/legal\/search\/@
-- 
--  Search legal documents by type, or across all document types using keywords, parameter values and ranges. 
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
legalSearchGet 
  :: ApiKey -- ^ "apiKey" -   API key for https://api.data.gov. Get one at https://api.data.gov/signup. 
  -> OpenFECRequest LegalSearchGet MimeNoContent InlineResponseDefault1 MimeJSON
legalSearchGet (ApiKey apiKey) =
  _mkRequest "GET" ["/legal/search/"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setQuery` toQuery ("api_key", Just apiKey)

data LegalSearchGet  

-- | /Optional Param/ "af_min_fd_date" - Filter cases by earliest Final Determination date
instance HasOptionalParam LegalSearchGet AfMinFdDate where
  applyOptionalParam req (AfMinFdDate xs) =
    req `setQuery` toQuery ("af_min_fd_date", Just xs)

-- | /Optional Param/ "af_fd_fine_amount" - Filter cases by Final Determination fine amount
instance HasOptionalParam LegalSearchGet AfFdFineAmount where
  applyOptionalParam req (AfFdFineAmount xs) =
    req `setQuery` toQuery ("af_fd_fine_amount", Just xs)

-- | /Optional Param/ "mur_no" - Filter MURs by case number
instance HasOptionalParam LegalSearchGet MurNo where
  applyOptionalParam req (MurNo xs) =
    req `setQuery` toQueryColl MultiParamArray ("mur_no", Just xs)

-- | /Optional Param/ "case_min_open_date" - Filter cases by earliest date opened
instance HasOptionalParam LegalSearchGet CaseMinOpenDate where
  applyOptionalParam req (CaseMinOpenDate xs) =
    req `setQuery` toQuery ("case_min_open_date", Just xs)

-- | /Optional Param/ "ao_entity_name" - Search by name of commenter or representative
instance HasOptionalParam LegalSearchGet AoEntityName where
  applyOptionalParam req (AoEntityName xs) =
    req `setQuery` toQueryColl MultiParamArray ("ao_entity_name", Just xs)

-- | /Optional Param/ "case_election_cycles" - Filter cases by election cycles
instance HasOptionalParam LegalSearchGet CaseElectionCycles where
  applyOptionalParam req (CaseElectionCycles xs) =
    req `setQuery` toQuery ("case_election_cycles", Just xs)

-- | /Optional Param/ "from_hit" - Get results starting from this index.
instance HasOptionalParam LegalSearchGet FromHit where
  applyOptionalParam req (FromHit xs) =
    req `setQuery` toQuery ("from_hit", Just xs)

-- | /Optional Param/ "case_max_close_date" - Filter cases by latest date closed
instance HasOptionalParam LegalSearchGet CaseMaxCloseDate where
  applyOptionalParam req (CaseMaxCloseDate xs) =
    req `setQuery` toQuery ("case_max_close_date", Just xs)

-- | /Optional Param/ "af_rtb_fine_amount" - Filter cases by Reason to Believe fine amount
instance HasOptionalParam LegalSearchGet AfRtbFineAmount where
  applyOptionalParam req (AfRtbFineAmount xs) =
    req `setQuery` toQuery ("af_rtb_fine_amount", Just xs)

-- | /Optional Param/ "ao_is_pending" - AO is pending
instance HasOptionalParam LegalSearchGet AoIsPending where
  applyOptionalParam req (AoIsPending xs) =
    req `setQuery` toQuery ("ao_is_pending", Just xs)

-- | /Optional Param/ "ao_min_request_date" - Earliest request date of advisory opinion
instance HasOptionalParam LegalSearchGet AoMinRequestDate where
  applyOptionalParam req (AoMinRequestDate xs) =
    req `setQuery` toQuery ("ao_min_request_date", Just xs)

-- | /Optional Param/ "ao_min_issue_date" - Earliest issue date of advisory opinion
instance HasOptionalParam LegalSearchGet AoMinIssueDate where
  applyOptionalParam req (AoMinIssueDate xs) =
    req `setQuery` toQuery ("ao_min_issue_date", Just xs)

-- | /Optional Param/ "ao_name" - Force advisory opinion name
instance HasOptionalParam LegalSearchGet AoName where
  applyOptionalParam req (AoName xs) =
    req `setQuery` toQueryColl MultiParamArray ("ao_name", Just xs)

-- | /Optional Param/ "ao_no" - Force advisory opinion number
instance HasOptionalParam LegalSearchGet AoNo where
  applyOptionalParam req (AoNo xs) =
    req `setQuery` toQueryColl MultiParamArray ("ao_no", Just xs)

-- | /Optional Param/ "af_committee_id" - Admin fine committee ID
instance HasOptionalParam LegalSearchGet AfCommitteeId where
  applyOptionalParam req (AfCommitteeId xs) =
    req `setQuery` toQuery ("af_committee_id", Just xs)

-- | /Optional Param/ "case_no" - Enforcement matter case number
instance HasOptionalParam LegalSearchGet CaseNo where
  applyOptionalParam req (CaseNo xs) =
    req `setQuery` toQueryColl MultiParamArray ("case_no", Just xs)

-- | /Optional Param/ "af_name" - Admin fine committee name
instance HasOptionalParam LegalSearchGet AfName where
  applyOptionalParam req (AfName xs) =
    req `setQuery` toQueryColl MultiParamArray ("af_name", Just xs)

-- | /Optional Param/ "mur_election_cycles" - Filter MURs by election cycles
instance HasOptionalParam LegalSearchGet MurElectionCycles where
  applyOptionalParam req (MurElectionCycles xs) =
    req `setQuery` toQuery ("mur_election_cycles", Just xs)

-- | /Optional Param/ "mur_max_open_date" - Filter MURs by latest date opened
instance HasOptionalParam LegalSearchGet MurMaxOpenDate where
  applyOptionalParam req (MurMaxOpenDate xs) =
    req `setQuery` toQuery ("mur_max_open_date", Just xs)

-- | /Optional Param/ "case_document_category" - Filter cases by category of associated documents
instance HasOptionalParam LegalSearchGet CaseDocumentCategory where
  applyOptionalParam req (CaseDocumentCategory xs) =
    req `setQuery` toQueryColl MultiParamArray ("case_document_category", Just xs)

-- | /Optional Param/ "af_max_fd_date" - Filter cases by latest Final Determination date
instance HasOptionalParam LegalSearchGet AfMaxFdDate where
  applyOptionalParam req (AfMaxFdDate xs) =
    req `setQuery` toQuery ("af_max_fd_date", Just xs)

-- | /Optional Param/ "ao_status" - Status of AO (pending, withdrawn, or final)
instance HasOptionalParam LegalSearchGet AoStatus where
  applyOptionalParam req (AoStatus xs) =
    req `setQuery` toQuery ("ao_status", Just xs)

-- | /Optional Param/ "ao_statutory_citation" - Search for statutory citations
instance HasOptionalParam LegalSearchGet AoStatutoryCitation where
  applyOptionalParam req (AoStatutoryCitation xs) =
    req `setQuery` toQueryColl MultiParamArray ("ao_statutory_citation", Just xs)

-- | /Optional Param/ "case_respondents" - Filter cases by respondents
instance HasOptionalParam LegalSearchGet CaseRespondents where
  applyOptionalParam req (CaseRespondents xs) =
    req `setQuery` toQuery ("case_respondents", Just xs)

-- | /Optional Param/ "ao_max_issue_date" - Latest issue date of advisory opinion
instance HasOptionalParam LegalSearchGet AoMaxIssueDate where
  applyOptionalParam req (AoMaxIssueDate xs) =
    req `setQuery` toQuery ("ao_max_issue_date", Just xs)

-- | /Optional Param/ "mur_respondents" - Filter MURs by respondents
instance HasOptionalParam LegalSearchGet MurRespondents where
  applyOptionalParam req (MurRespondents xs) =
    req `setQuery` toQuery ("mur_respondents", Just xs)

-- | /Optional Param/ "af_min_rtb_date" - Filter cases by earliest Reason to Believe date
instance HasOptionalParam LegalSearchGet AfMinRtbDate where
  applyOptionalParam req (AfMinRtbDate xs) =
    req `setQuery` toQuery ("af_min_rtb_date", Just xs)

-- | /Optional Param/ "af_report_year" - Admin fine report year
instance HasOptionalParam LegalSearchGet AfReportYear where
  applyOptionalParam req (AfReportYear xs) =
    req `setQuery` toQuery ("af_report_year", Just xs)

-- | /Optional Param/ "mur_document_category" - Filter MURs by category of associated documents
instance HasOptionalParam LegalSearchGet MurDocumentCategory where
  applyOptionalParam req (MurDocumentCategory xs) =
    req `setQuery` toQueryColl MultiParamArray ("mur_document_category", Just xs)

-- | /Optional Param/ "type" - Document type to refine search by
instance HasOptionalParam LegalSearchGet ParamType where
  applyOptionalParam req (ParamType xs) =
    req `setQuery` toQuery ("type", Just xs)

-- | /Optional Param/ "mur_dispositions" - Filter MURs by dispositions
instance HasOptionalParam LegalSearchGet MurDispositions where
  applyOptionalParam req (MurDispositions xs) =
    req `setQuery` toQueryColl MultiParamArray ("mur_dispositions", Just xs)

-- | /Optional Param/ "case_dispositions" - Filter cases by dispositions
instance HasOptionalParam LegalSearchGet CaseDispositions where
  applyOptionalParam req (CaseDispositions xs) =
    req `setQuery` toQueryColl MultiParamArray ("case_dispositions", Just xs)

-- | /Optional Param/ "ao_citation_require_all" - Require all citations to be in document (default behavior is any)
instance HasOptionalParam LegalSearchGet AoCitationRequireAll where
  applyOptionalParam req (AoCitationRequireAll xs) =
    req `setQuery` toQuery ("ao_citation_require_all", Just xs)

-- | /Optional Param/ "case_max_open_date" - Filter cases by latest date opened
instance HasOptionalParam LegalSearchGet CaseMaxOpenDate where
  applyOptionalParam req (CaseMaxOpenDate xs) =
    req `setQuery` toQuery ("case_max_open_date", Just xs)

-- | /Optional Param/ "hits_returned" - Number of results to return (max 10).
instance HasOptionalParam LegalSearchGet HitsReturned where
  applyOptionalParam req (HitsReturned xs) =
    req `setQuery` toQuery ("hits_returned", Just xs)

-- | /Optional Param/ "af_max_rtb_date" - Filter cases by latest Reason to Believe date
instance HasOptionalParam LegalSearchGet AfMaxRtbDate where
  applyOptionalParam req (AfMaxRtbDate xs) =
    req `setQuery` toQuery ("af_max_rtb_date", Just xs)

-- | /Optional Param/ "ao_category" - Category of the document
instance HasOptionalParam LegalSearchGet AoCategory where
  applyOptionalParam req (AoCategory xs) =
    req `setQuery` toQueryColl MultiParamArray ("ao_category", Just xs)

-- | /Optional Param/ "mur_min_open_date" - Filter MURs by earliest date opened
instance HasOptionalParam LegalSearchGet MurMinOpenDate where
  applyOptionalParam req (MurMinOpenDate xs) =
    req `setQuery` toQuery ("mur_min_open_date", Just xs)

-- | /Optional Param/ "ao_regulatory_citation" - Search for regulatory citations
instance HasOptionalParam LegalSearchGet AoRegulatoryCitation where
  applyOptionalParam req (AoRegulatoryCitation xs) =
    req `setQuery` toQueryColl MultiParamArray ("ao_regulatory_citation", Just xs)

-- | /Optional Param/ "case_min_close_date" - Filter cases by earliest date closed
instance HasOptionalParam LegalSearchGet CaseMinCloseDate where
  applyOptionalParam req (CaseMinCloseDate xs) =
    req `setQuery` toQuery ("case_min_close_date", Just xs)

-- | /Optional Param/ "mur_max_close_date" - Filter MURs by latest date closed
instance HasOptionalParam LegalSearchGet MurMaxCloseDate where
  applyOptionalParam req (MurMaxCloseDate xs) =
    req `setQuery` toQuery ("mur_max_close_date", Just xs)

-- | /Optional Param/ "ao_max_request_date" - Latest request date of advisory opinion
instance HasOptionalParam LegalSearchGet AoMaxRequestDate where
  applyOptionalParam req (AoMaxRequestDate xs) =
    req `setQuery` toQuery ("ao_max_request_date", Just xs)

-- | /Optional Param/ "q" - Text to search legal documents for.
instance HasOptionalParam LegalSearchGet QText where
  applyOptionalParam req (QText xs) =
    req `setQuery` toQuery ("q", Just xs)

-- | /Optional Param/ "ao_requestor_type" - Code of the advisory opinion requestor type.
instance HasOptionalParam LegalSearchGet AoRequestorType where
  applyOptionalParam req (AoRequestorType xs) =
    req `setQuery` toQueryColl MultiParamArray ("ao_requestor_type", Just xs)

-- | /Optional Param/ "mur_min_close_date" - Filter MURs by earliest date closed
instance HasOptionalParam LegalSearchGet MurMinCloseDate where
  applyOptionalParam req (MurMinCloseDate xs) =
    req `setQuery` toQuery ("mur_min_close_date", Just xs)

-- | /Optional Param/ "ao_requestor" - The requestor of the advisory opinion
instance HasOptionalParam LegalSearchGet AoRequestor where
  applyOptionalParam req (AoRequestor xs) =
    req `setQuery` toQuery ("ao_requestor", Just xs)
-- | @application/json@
instance Produces LegalSearchGet MimeJSON

