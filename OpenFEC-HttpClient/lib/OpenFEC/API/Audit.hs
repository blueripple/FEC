{-
   OpenFEC

   This API allows you to explore the way candidates and committees fund their campaigns.    The FEC API is a RESTful web service supporting full-text and field-specific searches on FEC data. [Bulk downloads](https://www.fec.gov/data/advanced/?tab=bulk-data) are available on the current site. Information is tied to the underlying forms by file ID and image ID. Data is updated nightly.    There is a lot of data, but a good place to start is to use search to find interesting candidates and committees. Then, you can use their IDs to find report or line item details with the other endpoints. If you are interested in individual donors, check out contributor information in schedule_a.    Get an [API key here](https://api.data.gov/signup/). That will enable you to place up to 1,000 calls an hour. Each call is limited to 100 results per page. You can email questions, comments or a request to get a key for 120 calls per minute to [APIinfo@fec.gov](mailto:apiinfo@fec.gov). You can also ask questions and discuss the data in the [FEC data Google Group](https://groups.google.com/forum/#!forum/fec-data). API changes will also be added to this group in advance of the change.       The model definitions and schema are available at [/swagger](/swagger/). This is useful for making wrappers and exploring the data.    A few restrictions limit the way you can use FEC data. For example, you can’t use contributor lists for commercial purposes or to solicit donations. [Learn more here](https://transition.fec.gov/pages/brochures/saleuse.shtml).    [View our source code](https://github.com/fecgov/openFEC). We welcome issues and pull requests!

   OpenAPI spec version: 2.0
   OpenFEC API version: 1.0
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : OpenFEC.API.Audit
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OpenFEC.API.Audit where

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


-- ** Audit

-- *** auditCaseGet

-- | @GET \/audit-case\/@
-- 
--  This endpoint contains Final Audit Reports approved by the Commission since inception. The search can be based on information about the audited committee (Name, FEC ID Number, Type,  Election Cycle) or the issues covered in the report. 
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
auditCaseGet 
  :: ApiKey -- ^ "apiKey" -   API key for https://api.data.gov. Get one at https://api.data.gov/signup. 
  -> OpenFECRequest AuditCaseGet MimeNoContent AuditCasePage MimeJSON
auditCaseGet (ApiKey apiKey) =
  _mkRequest "GET" ["/audit-case/"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setQuery` toQuery ("api_key", Just apiKey)

data AuditCaseGet  

-- | /Optional Param/ "sort_null_only" - Toggle that filters out all rows having sort column that is non-null
instance HasOptionalParam AuditCaseGet SortNullOnly where
  applyOptionalParam req (SortNullOnly xs) =
    req `setQuery` toQuery ("sort_null_only", Just xs)

-- | /Optional Param/ "sort" - Provide a field to sort by. Use - for descending order.
instance HasOptionalParam AuditCaseGet Sort where
  applyOptionalParam req (Sort xs) =
    req `setQuery` toQueryColl MultiParamArray ("sort", Just xs)

-- | /Optional Param/ "max_election_cycle" -  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year. 
instance HasOptionalParam AuditCaseGet MaxElectionCycle where
  applyOptionalParam req (MaxElectionCycle xs) =
    req `setQuery` toQuery ("max_election_cycle", Just xs)

-- | /Optional Param/ "page" - For paginating through results, starting at page 1
instance HasOptionalParam AuditCaseGet Page where
  applyOptionalParam req (Page xs) =
    req `setQuery` toQuery ("page", Just xs)

-- | /Optional Param/ "sub_category_id" -  The finding id of an audit. Finding are a category of broader issues. Each category has an unique ID. 
instance HasOptionalParam AuditCaseGet SubCategoryId where
  applyOptionalParam req (SubCategoryId xs) =
    req `setQuery` toQuery ("sub_category_id", Just xs)

-- | /Optional Param/ "cycle" -  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year. 
instance HasOptionalParam AuditCaseGet Cycle where
  applyOptionalParam req (Cycle xs) =
    req `setQuery` toQueryColl MultiParamArray ("cycle", Just xs)

-- | /Optional Param/ "sort_hide_null" - Hide null values on sorted column(s).
instance HasOptionalParam AuditCaseGet SortHideNull where
  applyOptionalParam req (SortHideNull xs) =
    req `setQuery` toQuery ("sort_hide_null", Just xs)

-- | /Optional Param/ "candidate_id" -  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office. 
instance HasOptionalParam AuditCaseGet CandidateId where
  applyOptionalParam req (CandidateId xs) =
    req `setQuery` toQueryColl MultiParamArray ("candidate_id", Just xs)

-- | /Optional Param/ "min_election_cycle" -  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year. 
instance HasOptionalParam AuditCaseGet MinElectionCycle where
  applyOptionalParam req (MinElectionCycle xs) =
    req `setQuery` toQuery ("min_election_cycle", Just xs)

-- | /Optional Param/ "primary_category_id" -  Audit category ID (table PK) 
instance HasOptionalParam AuditCaseGet PrimaryCategoryId where
  applyOptionalParam req (PrimaryCategoryId xs) =
    req `setQuery` toQuery ("primary_category_id", Just xs)

-- | /Optional Param/ "committee_type" - The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account 
instance HasOptionalParam AuditCaseGet CommitteeType where
  applyOptionalParam req (CommitteeType xs) =
    req `setQuery` toQueryColl MultiParamArray ("committee_type", Just xs)

-- | /Optional Param/ "audit_id" -  The audit issue. Each subcategory has an unique ID 
instance HasOptionalParam AuditCaseGet AuditId where
  applyOptionalParam req (AuditId xs) =
    req `setQuery` toQueryColl MultiParamArray ("audit_id", Just xs)

-- | /Optional Param/ "committee_designation" - Type of committee:         - H or S - Congressional         - P - Presidential         - X or Y or Z - Party         - N or Q - PAC         - I - Independent expenditure         - O - Super PAC  
instance HasOptionalParam AuditCaseGet CommitteeDesignation where
  applyOptionalParam req (CommitteeDesignation xs) =
    req `setQuery` toQuery ("committee_designation", Just xs)

-- | /Optional Param/ "audit_case_id" -  Primary/foreign key for audit tables 
instance HasOptionalParam AuditCaseGet AuditCaseId where
  applyOptionalParam req (AuditCaseId xs) =
    req `setQuery` toQueryColl MultiParamArray ("audit_case_id", Just xs)

-- | /Optional Param/ "q" - The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
instance HasOptionalParam AuditCaseGet Q where
  applyOptionalParam req (Q xs) =
    req `setQuery` toQueryColl MultiParamArray ("q", Just xs)

-- | /Optional Param/ "qq" - Name of candidate running for office
instance HasOptionalParam AuditCaseGet Qq where
  applyOptionalParam req (Qq xs) =
    req `setQuery` toQueryColl MultiParamArray ("qq", Just xs)

-- | /Optional Param/ "per_page" - The number of results returned per page. Defaults to 20.
instance HasOptionalParam AuditCaseGet PerPage where
  applyOptionalParam req (PerPage xs) =
    req `setQuery` toQuery ("per_page", Just xs)

-- | /Optional Param/ "committee_id" -  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits. 
instance HasOptionalParam AuditCaseGet CommitteeId where
  applyOptionalParam req (CommitteeId xs) =
    req `setQuery` toQueryColl MultiParamArray ("committee_id", Just xs)
-- | @application/json@
instance Produces AuditCaseGet MimeJSON


-- *** auditCategoryGet

-- | @GET \/audit-category\/@
-- 
--  This lists the options for the categories and subcategories available in the /audit-search/ endpoint. 
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
auditCategoryGet 
  :: ApiKey -- ^ "apiKey" -   API key for https://api.data.gov. Get one at https://api.data.gov/signup. 
  -> OpenFECRequest AuditCategoryGet MimeNoContent AuditCategoryPage MimeJSON
auditCategoryGet (ApiKey apiKey) =
  _mkRequest "GET" ["/audit-category/"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setQuery` toQuery ("api_key", Just apiKey)

data AuditCategoryGet  

-- | /Optional Param/ "per_page" - The number of results returned per page. Defaults to 20.
instance HasOptionalParam AuditCategoryGet PerPage where
  applyOptionalParam req (PerPage xs) =
    req `setQuery` toQuery ("per_page", Just xs)

-- | /Optional Param/ "sort_null_only" - Toggle that filters out all rows having sort column that is non-null
instance HasOptionalParam AuditCategoryGet SortNullOnly where
  applyOptionalParam req (SortNullOnly xs) =
    req `setQuery` toQuery ("sort_null_only", Just xs)

-- | /Optional Param/ "primary_category_id" -  Audit category ID (table PK) 
instance HasOptionalParam AuditCategoryGet PrimaryCategoryId[Text] where
  applyOptionalParam req (PrimaryCategoryId[Text] xs) =
    req `setQuery` toQueryColl MultiParamArray ("primary_category_id", Just xs)

-- | /Optional Param/ "sort" - Provide a field to sort by. Use - for descending order.
instance HasOptionalParam AuditCategoryGet SortText where
  applyOptionalParam req (SortText xs) =
    req `setQuery` toQuery ("sort", Just xs)

-- | /Optional Param/ "primary_category_name" - Primary Audit Category     - No Findings or Issues/Not a Committee     - Net Outstanding Campaign/Convention Expenditures/Obligations     - Payments/Disgorgements     - Allocation Issues     - Prohibited Contributions     - Disclosure     - Recordkeeping     - Repayment to US Treasury     - Other     - Misstatement of Financial Activity     - Excessive Contributions     - Failure to File Reports/Schedules/Notices     - Loans     - Referred Findings Not Listed 
instance HasOptionalParam AuditCategoryGet PrimaryCategoryName where
  applyOptionalParam req (PrimaryCategoryName xs) =
    req `setQuery` toQueryColl MultiParamArray ("primary_category_name", Just xs)

-- | /Optional Param/ "sort_hide_null" - Hide null values on sorted column(s).
instance HasOptionalParam AuditCategoryGet SortHideNull where
  applyOptionalParam req (SortHideNull xs) =
    req `setQuery` toQuery ("sort_hide_null", Just xs)

-- | /Optional Param/ "page" - For paginating through results, starting at page 1
instance HasOptionalParam AuditCategoryGet Page where
  applyOptionalParam req (Page xs) =
    req `setQuery` toQuery ("page", Just xs)
-- | @application/json@
instance Produces AuditCategoryGet MimeJSON


-- *** auditPrimaryCategoryGet

-- | @GET \/audit-primary-category\/@
-- 
--  This lists the options for the primary categories available in the /audit-search/ endpoint. 
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
auditPrimaryCategoryGet 
  :: ApiKey -- ^ "apiKey" -   API key for https://api.data.gov. Get one at https://api.data.gov/signup. 
  -> OpenFECRequest AuditPrimaryCategoryGet MimeNoContent AuditPrimaryCategoryPage MimeJSON
auditPrimaryCategoryGet (ApiKey apiKey) =
  _mkRequest "GET" ["/audit-primary-category/"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setQuery` toQuery ("api_key", Just apiKey)

data AuditPrimaryCategoryGet  

-- | /Optional Param/ "per_page" - The number of results returned per page. Defaults to 20.
instance HasOptionalParam AuditPrimaryCategoryGet PerPage where
  applyOptionalParam req (PerPage xs) =
    req `setQuery` toQuery ("per_page", Just xs)

-- | /Optional Param/ "sort_null_only" - Toggle that filters out all rows having sort column that is non-null
instance HasOptionalParam AuditPrimaryCategoryGet SortNullOnly where
  applyOptionalParam req (SortNullOnly xs) =
    req `setQuery` toQuery ("sort_null_only", Just xs)

-- | /Optional Param/ "primary_category_id" -  Audit category ID (table PK) 
instance HasOptionalParam AuditPrimaryCategoryGet PrimaryCategoryId[Text] where
  applyOptionalParam req (PrimaryCategoryId[Text] xs) =
    req `setQuery` toQueryColl MultiParamArray ("primary_category_id", Just xs)

-- | /Optional Param/ "sort" - Provide a field to sort by. Use - for descending order.
instance HasOptionalParam AuditPrimaryCategoryGet SortText where
  applyOptionalParam req (SortText xs) =
    req `setQuery` toQuery ("sort", Just xs)

-- | /Optional Param/ "primary_category_name" - Primary Audit Category     - No Findings or Issues/Not a Committee     - Net Outstanding Campaign/Convention Expenditures/Obligations     - Payments/Disgorgements     - Allocation Issues     - Prohibited Contributions     - Disclosure     - Recordkeeping     - Repayment to US Treasury     - Other     - Misstatement of Financial Activity     - Excessive Contributions     - Failure to File Reports/Schedules/Notices     - Loans     - Referred Findings Not Listed 
instance HasOptionalParam AuditPrimaryCategoryGet PrimaryCategoryName where
  applyOptionalParam req (PrimaryCategoryName xs) =
    req `setQuery` toQueryColl MultiParamArray ("primary_category_name", Just xs)

-- | /Optional Param/ "sort_hide_null" - Hide null values on sorted column(s).
instance HasOptionalParam AuditPrimaryCategoryGet SortHideNull where
  applyOptionalParam req (SortHideNull xs) =
    req `setQuery` toQuery ("sort_hide_null", Just xs)

-- | /Optional Param/ "page" - For paginating through results, starting at page 1
instance HasOptionalParam AuditPrimaryCategoryGet Page where
  applyOptionalParam req (Page xs) =
    req `setQuery` toQuery ("page", Just xs)
-- | @application/json@
instance Produces AuditPrimaryCategoryGet MimeJSON


-- *** namesAuditCandidatesGet

-- | @GET \/names\/audit_candidates\/@
-- 
--  Search for candidates or committees by name. If you're looking for information on a particular person or group, using a name to find the `candidate_id` or `committee_id` on this endpoint can be a helpful first step. 
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
namesAuditCandidatesGet 
  :: ApiKey -- ^ "apiKey" -   API key for https://api.data.gov. Get one at https://api.data.gov/signup. 
  -> Q -- ^ "q" -  Name (candidate or committee) to search for
  -> OpenFECRequest NamesAuditCandidatesGet MimeNoContent AuditCandidateSearchList MimeJSON
namesAuditCandidatesGet (ApiKey apiKey) (Q q) =
  _mkRequest "GET" ["/names/audit_candidates/"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setQuery` toQuery ("api_key", Just apiKey)
    `setQuery` toQueryColl MultiParamArray ("q", Just q)

data NamesAuditCandidatesGet  
-- | @application/json@
instance Produces NamesAuditCandidatesGet MimeJSON


-- *** namesAuditCommitteesGet

-- | @GET \/names\/audit_committees\/@
-- 
--  Search for candidates or committees by name. If you're looking for information on a particular person or group, using a name to find the `candidate_id` or `committee_id` on this endpoint can be a helpful first step. 
-- 
-- AuthMethod: 'AuthApiKeyApiKey'
-- 
namesAuditCommitteesGet 
  :: ApiKey -- ^ "apiKey" -   API key for https://api.data.gov. Get one at https://api.data.gov/signup. 
  -> Q -- ^ "q" -  Name (candidate or committee) to search for
  -> OpenFECRequest NamesAuditCommitteesGet MimeNoContent AuditCommitteeSearchList MimeJSON
namesAuditCommitteesGet (ApiKey apiKey) (Q q) =
  _mkRequest "GET" ["/names/audit_committees/"]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyApiKey)
    `setQuery` toQuery ("api_key", Just apiKey)
    `setQuery` toQueryColl MultiParamArray ("q", Just q)

data NamesAuditCommitteesGet  
-- | @application/json@
instance Produces NamesAuditCommitteesGet MimeJSON

