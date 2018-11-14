{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports #-}

module OpenFEC.Types (
  AuditCandidateSearch (..),
  AuditCandidateSearchList (..),
  AuditCase (..),
  AuditCaseCategoryRelation (..),
  AuditCaseCategoryRelationPage (..),
  AuditCasePage (..),
  AuditCaseSubCategory (..),
  AuditCaseSubCategoryPage (..),
  AuditCategory (..),
  AuditCategoryPage (..),
  AuditCategoryRelation (..),
  AuditCategoryRelationPage (..),
  AuditCommitteeSearch (..),
  AuditCommitteeSearchList (..),
  AuditPrimaryCategory (..),
  AuditPrimaryCategoryPage (..),
  BaseF3Filing (..),
  BaseF3FilingPage (..),
  BaseF3PFiling (..),
  BaseF3PFilingPage (..),
  BaseF3XFiling (..),
  BaseF3XFilingPage (..),
  CalendarDate (..),
  CalendarDatePage (..),
  Candidate (..),
  CandidateCommitteeTotalsHouseSenate (..),
  CandidateCommitteeTotalsHouseSenatePage (..),
  CandidateCommitteeTotalsPresidential (..),
  CandidateCommitteeTotalsPresidentialPage (..),
  CandidateDetail (..),
  CandidateDetailPage (..),
  CandidateFlags (..),
  CandidateFlagsPage (..),
  CandidateHistory (..),
  CandidateHistoryPage (..),
  CandidateHistoryTotal (..),
  CandidateHistoryTotalPage (..),
  CandidatePage (..),
  CandidateSearch (..),
  CandidateSearchList (..),
  CandidateTotal (..),
  CandidateTotalPage (..),
  Committee (..),
  CommitteeDetail (..),
  CommitteeDetailPage (..),
  CommitteeHistory (..),
  CommitteeHistoryPage (..),
  CommitteePage (..),
  CommitteeReports (..),
  CommitteeReportsHouseSenate (..),
  CommitteeReportsHouseSenatePage (..),
  CommitteeReportsIEOnly (..),
  CommitteeReportsIEOnlyPage (..),
  CommitteeReportsPacParty (..),
  CommitteeReportsPacPartyPage (..),
  CommitteeReportsPage (..),
  CommitteeReportsPresidential (..),
  CommitteeReportsPresidentialPage (..),
  CommitteeSearch (..),
  CommitteeSearchList (..),
  CommitteeTotals (..),
  CommitteeTotalsHouseSenate (..),
  CommitteeTotalsHouseSenatePage (..),
  CommitteeTotalsIEOnly (..),
  CommitteeTotalsIEOnlyPage (..),
  CommitteeTotalsPacParty (..),
  CommitteeTotalsPacPartyPage (..),
  CommitteeTotalsPage (..),
  CommitteeTotalsPresidential (..),
  CommitteeTotalsPresidentialPage (..),
  CommunicationCost (..),
  CommunicationCostByCandidate (..),
  CommunicationCostByCandidatePage (..),
  CommunicationCostPage (..),
  EFilings (..),
  EFilingsPage (..),
  EfilingsAmendments (..),
  EfilingsAmendmentsPage (..),
  Election (..),
  ElectionDate (..),
  ElectionDatePage (..),
  ElectionPage (..),
  ElectionSearch (..),
  ElectionSearchPage (..),
  ElectionSummary (..),
  Electioneering (..),
  ElectioneeringByCandidate (..),
  ElectioneeringByCandidatePage (..),
  ElectioneeringPage (..),
  ElectionsList (..),
  ElectionsListPage (..),
  EntityReceiptDisbursementTotals (..),
  EntityReceiptDisbursementTotalsPage (..),
  Filings (..),
  FilingsPage (..),
  Inline_response_default (..),
  Inline_response_default_1 (..),
  Inline_response_default_1_admin_fines (..),
  Inline_response_default_1_adrs (..),
  Inline_response_default_1_advisory_opinions (..),
  Inline_response_default_1_ao_citations (..),
  Inline_response_default_1_citations (..),
  Inline_response_default_1_commission_votes (..),
  Inline_response_default_1_dispositions (..),
  Inline_response_default_1_documents (..),
  Inline_response_default_1_documents_1 (..),
  Inline_response_default_1_entities (..),
  Inline_response_default_1_murs (..),
  Inline_response_default_1_participants (..),
  Inline_response_default_1_regulations (..),
  Inline_response_default_1_regulatory_citations (..),
  Inline_response_default_1_statutes (..),
  Inline_response_default_1_statutory_citations (..),
  Inline_response_default_2 (..),
  Inline_response_default_3 (..),
  Inline_response_default_3_results (..),
  Inline_response_default_4 (..),
  Inline_response_default_4_results (..),
  Inline_response_default_5 (..),
  Inline_response_default_5_results (..),
  OffsetInfo (..),
  OperationsLog (..),
  OperationsLogPage (..),
  RadAnalyst (..),
  RadAnalystPage (..),
  ReportDate (..),
  ReportDatePage (..),
  ReportType (..),
  ScheduleA (..),
  ScheduleAByEmployer (..),
  ScheduleAByEmployerPage (..),
  ScheduleAByOccupation (..),
  ScheduleAByOccupationPage (..),
  ScheduleABySize (..),
  ScheduleABySizeCandidate (..),
  ScheduleABySizeCandidatePage (..),
  ScheduleABySizePage (..),
  ScheduleAByState (..),
  ScheduleAByStateCandidate (..),
  ScheduleAByStateCandidatePage (..),
  ScheduleAByStatePage (..),
  ScheduleAByStateRecipientTotals (..),
  ScheduleAByStateRecipientTotalsPage (..),
  ScheduleAByZip (..),
  ScheduleAByZipPage (..),
  ScheduleAEfile (..),
  ScheduleAEfilePage (..),
  ScheduleAPage (..),
  ScheduleB (..),
  ScheduleBByPurpose (..),
  ScheduleBByPurposePage (..),
  ScheduleBByRecipient (..),
  ScheduleBByRecipientID (..),
  ScheduleBByRecipientIDPage (..),
  ScheduleBByRecipientPage (..),
  ScheduleBEfile (..),
  ScheduleBEfilePage (..),
  ScheduleBPage (..),
  ScheduleE (..),
  ScheduleEByCandidate (..),
  ScheduleEByCandidatePage (..),
  ScheduleEEfile (..),
  ScheduleEEfilePage (..),
  ScheduleEPage (..),
  SeekInfo (..),
  StateElectionOfficeInfo (..),
  StateElectionOfficeInfoPage (..),
  TotalsCommittee (..),
  TotalsCommitteePage (..),
  -- Added
  Date
  ) where

import           Data.Aeson         (FromJSON (..), ToJSON (..), Value,
                                     genericParseJSON, genericToJSON)
import           Data.Aeson.Types   (Options (..), defaultOptions)
import           Data.Function      ((&))
import           Data.List          (stripPrefix)
import qualified Data.Map           as Map
import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time.Calendar (Day)
import           GHC.Generics       (Generic)

type Date = Day

-- |
data AuditCandidateSearch = AuditCandidateSearch
  { auditCandidateSearchId   :: Text -- ^
  , auditCandidateSearchName :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCandidateSearch where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCandidateSearch")
instance ToJSON AuditCandidateSearch where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCandidateSearch")

-- |
data AuditCandidateSearchList = AuditCandidateSearchList
  { auditCandidateSearchListResults :: [AuditCandidateSearch] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCandidateSearchList where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCandidateSearchList")
instance ToJSON AuditCandidateSearchList where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCandidateSearchList")

-- |
data AuditCase = AuditCase
  { auditCaseAudit'Underscorecase'Underscoreid :: Text -- ^
  , auditCaseAudit'Underscoreid :: Int -- ^
  , auditCaseCandidate'Underscoreid :: Text -- ^
  , auditCaseCandidate'Underscorename :: Text -- ^
  , auditCaseCommittee'Underscoredescription :: Text -- ^
  , auditCaseCommittee'Underscoredesignation :: Text -- ^
  , auditCaseCommittee'Underscoreid :: Text -- ^
  , auditCaseCommittee'Underscorename :: Text -- ^
  , auditCaseCommittee'Underscoretype :: Text -- ^
  , auditCaseCycle :: Int -- ^
  , auditCaseFar'Underscorerelease'Underscoredate :: Date -- ^
  , auditCaseLink'Underscoreto'Underscorereport :: Text -- ^  URL for retrieving the PDF document
  , auditCasePrimary'Underscorecategory'Underscorelist :: [AuditCaseCategoryRelation] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCase where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCase")
instance ToJSON AuditCase where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCase")

-- |
data AuditCaseCategoryRelation = AuditCaseCategoryRelation
  { auditCaseCategoryRelationPrimary'Underscorecategory'Underscoreid :: Text -- ^
  , auditCaseCategoryRelationPrimary'Underscorecategory'Underscorename :: Text -- ^
  , auditCaseCategoryRelationSub'Underscorecategory'Underscorelist :: [AuditCaseSubCategory] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCaseCategoryRelation where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCaseCategoryRelation")
instance ToJSON AuditCaseCategoryRelation where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCaseCategoryRelation")

-- |
data AuditCaseCategoryRelationPage = AuditCaseCategoryRelationPage
  { auditCaseCategoryRelationPagePagination :: OffsetInfo -- ^
  , auditCaseCategoryRelationPageResults    :: [AuditCaseCategoryRelation] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCaseCategoryRelationPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCaseCategoryRelationPage")
instance ToJSON AuditCaseCategoryRelationPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCaseCategoryRelationPage")

-- |
data AuditCasePage = AuditCasePage
  { auditCasePagePagination :: OffsetInfo -- ^
  , auditCasePageResults    :: [AuditCase] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCasePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCasePage")
instance ToJSON AuditCasePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCasePage")

-- |
data AuditCaseSubCategory = AuditCaseSubCategory
  { auditCaseSubCategorySub'Underscorecategory'Underscoreid   :: Text -- ^
  , auditCaseSubCategorySub'Underscorecategory'Underscorename :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCaseSubCategory where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCaseSubCategory")
instance ToJSON AuditCaseSubCategory where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCaseSubCategory")

-- |
data AuditCaseSubCategoryPage = AuditCaseSubCategoryPage
  { auditCaseSubCategoryPagePagination :: OffsetInfo -- ^
  , auditCaseSubCategoryPageResults    :: [AuditCaseSubCategory] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCaseSubCategoryPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCaseSubCategoryPage")
instance ToJSON AuditCaseSubCategoryPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCaseSubCategoryPage")

-- |
data AuditCategory = AuditCategory
  { auditCategoryPrimary'Underscorecategory'Underscoreid :: Text -- ^
  , auditCategoryPrimary'Underscorecategory'Underscorename :: Text -- ^
  , auditCategorySub'Underscorecategory'Underscorelist :: [AuditCategoryRelation] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCategory where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCategory")
instance ToJSON AuditCategory where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCategory")

-- |
data AuditCategoryPage = AuditCategoryPage
  { auditCategoryPagePagination :: OffsetInfo -- ^
  , auditCategoryPageResults    :: [AuditCategory] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCategoryPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCategoryPage")
instance ToJSON AuditCategoryPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCategoryPage")

-- |
data AuditCategoryRelation = AuditCategoryRelation
  { auditCategoryRelationSub'Underscorecategory'Underscoreid   :: Text -- ^
  , auditCategoryRelationSub'Underscorecategory'Underscorename :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCategoryRelation where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCategoryRelation")
instance ToJSON AuditCategoryRelation where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCategoryRelation")

-- |
data AuditCategoryRelationPage = AuditCategoryRelationPage
  { auditCategoryRelationPagePagination :: OffsetInfo -- ^
  , auditCategoryRelationPageResults    :: [AuditCategoryRelation] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCategoryRelationPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCategoryRelationPage")
instance ToJSON AuditCategoryRelationPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCategoryRelationPage")

-- |
data AuditCommitteeSearch = AuditCommitteeSearch
  { auditCommitteeSearchId   :: Text -- ^
  , auditCommitteeSearchName :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCommitteeSearch where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCommitteeSearch")
instance ToJSON AuditCommitteeSearch where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCommitteeSearch")

-- |
data AuditCommitteeSearchList = AuditCommitteeSearchList
  { auditCommitteeSearchListResults :: [AuditCommitteeSearch] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCommitteeSearchList where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCommitteeSearchList")
instance ToJSON AuditCommitteeSearchList where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCommitteeSearchList")

-- |
data AuditPrimaryCategory = AuditPrimaryCategory
  { auditPrimaryCategoryPrimary'Underscorecategory'Underscoreid   :: Text -- ^
  , auditPrimaryCategoryPrimary'Underscorecategory'Underscorename :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditPrimaryCategory where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditPrimaryCategory")
instance ToJSON AuditPrimaryCategory where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditPrimaryCategory")

-- |
data AuditPrimaryCategoryPage = AuditPrimaryCategoryPage
  { auditPrimaryCategoryPagePagination :: OffsetInfo -- ^
  , auditPrimaryCategoryPageResults    :: [AuditPrimaryCategory] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditPrimaryCategoryPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditPrimaryCategoryPage")
instance ToJSON AuditPrimaryCategoryPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditPrimaryCategoryPage")

-- |
data BaseF3Filing = BaseF3Filing
  { baseF3FilingAmended'Underscoreaddress :: Text -- ^
  , baseF3FilingAmended'Underscoreby :: Int -- ^
  , baseF3FilingAmendment :: Text -- ^
  , baseF3FilingAmendment'Underscorechain :: [Int] -- ^
  , baseF3FilingBeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , baseF3FilingCandidate'Underscorefirst'Underscorename :: Text -- ^
  , baseF3FilingCandidate'Underscoreid :: Text -- ^
  , baseF3FilingCandidate'Underscorelast'Underscorename :: Text -- ^
  , baseF3FilingCandidate'Underscoremiddle'Underscorename :: Text -- ^
  , baseF3FilingCandidate'Underscorename :: Text -- ^
  , baseF3FilingCandidate'Underscoreprefix :: Text -- ^
  , baseF3FilingCandidate'Underscoresuffix :: Text -- ^
  , baseF3FilingCash'Underscoreon'Underscorehand'Underscorebeginning'Underscoreperiod :: Int -- ^
  , baseF3FilingCity :: Text -- ^
  , baseF3FilingCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , baseF3FilingCommittee'Underscorename :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , baseF3FilingCoverage'Underscoreend'Underscoredate :: Date -- ^
  , baseF3FilingCoverage'Underscorestart'Underscoredate :: Date -- ^
  , baseF3FilingCsv'Underscoreurl :: Text -- ^
  , baseF3FilingDistrict :: Int -- ^
  , baseF3FilingDocument'Underscoredescription :: Text -- ^
  , baseF3FilingElection'Underscoredate :: Date -- ^
  , baseF3FilingElection'Underscorestate :: Text -- ^
  , baseF3FilingF3z1 :: Int -- ^
  , baseF3FilingFec'Underscorefile'Underscoreid :: Text -- ^
  , baseF3FilingFec'Underscoreurl :: Text -- ^
  , baseF3FilingFile'Underscorenumber :: Int -- ^
  , baseF3FilingGeneral'Underscoreelection :: Text -- ^
  , baseF3FilingIs'Underscoreamended :: Bool -- ^
  , baseF3FilingMost'Underscorerecent :: Bool -- ^
  , baseF3FilingMost'Underscorerecent'Underscorefiling :: Int -- ^
  , baseF3FilingPdf'Underscoreurl :: Text -- ^
  , baseF3FilingPrefix :: Text -- ^
  , baseF3FilingPrimary'Underscoreelection :: Text -- ^
  , baseF3FilingReceipt'Underscoredate :: Date -- ^
  , baseF3FilingReport :: Text -- ^
  , baseF3FilingReport'Underscoretype :: Text -- ^
  , baseF3FilingReport'Underscoreyear :: Int -- ^
  , baseF3FilingRpt'Underscorepgi :: Text -- ^ Election type  Convention, Primary, General, Special, Runoff etc.
  , baseF3FilingRunoff'Underscoreelection :: Text -- ^
  , baseF3FilingSign'Underscoredate :: Date -- ^
  , baseF3FilingSpecial'Underscoreelection :: Text -- ^
  , baseF3FilingState :: Text -- ^
  , baseF3FilingStreet'Underscore1 :: Text -- ^
  , baseF3FilingStreet'Underscore2 :: Text -- ^
  , baseF3FilingSuffix :: Text -- ^
  , baseF3FilingSummary'Underscorelines :: Text -- ^
  , baseF3FilingTreasurer'Underscorefirst'Underscorename :: Text -- ^
  , baseF3FilingTreasurer'Underscorelast'Underscorename :: Text -- ^
  , baseF3FilingTreasurer'Underscoremiddle'Underscorename :: Text -- ^
  , baseF3FilingTreasurer'Underscorename :: Text -- ^
  , baseF3FilingZip :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON BaseF3Filing where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "baseF3Filing")
instance ToJSON BaseF3Filing where
  toJSON = genericToJSON (removeFieldLabelPrefix False "baseF3Filing")

-- |
data BaseF3FilingPage = BaseF3FilingPage
  { baseF3FilingPagePagination :: OffsetInfo -- ^
  , baseF3FilingPageResults    :: [BaseF3Filing] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON BaseF3FilingPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "baseF3FilingPage")
instance ToJSON BaseF3FilingPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "baseF3FilingPage")

-- |
data BaseF3PFiling = BaseF3PFiling
  { baseF3PFilingAmended'Underscoreby :: Int -- ^
  , baseF3PFilingAmendment :: Text -- ^
  , baseF3PFilingAmendment'Underscorechain :: [Int] -- ^
  , baseF3PFilingBeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , baseF3PFilingCash'Underscoreon'Underscorehand'Underscorebeginning'Underscoreperiod :: Float -- ^
  , baseF3PFilingCash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Float -- ^
  , baseF3PFilingCity :: Text -- ^
  , baseF3PFilingCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , baseF3PFilingCommittee'Underscorename :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , baseF3PFilingCoverage'Underscoreend'Underscoredate :: Date -- ^
  , baseF3PFilingCoverage'Underscorestart'Underscoredate :: Date -- ^
  , baseF3PFilingCsv'Underscoreurl :: Text -- ^
  , baseF3PFilingDebts'Underscoreowed'Underscoreby'Underscorecommittee :: Float -- ^
  , baseF3PFilingDebts'Underscoreowed'Underscoreto'Underscorecommittee :: Float -- ^
  , baseF3PFilingDocument'Underscoredescription :: Text -- ^
  , baseF3PFilingElection'Underscoredate :: Date -- ^
  , baseF3PFilingElection'Underscorestate :: Text -- ^
  , baseF3PFilingExpenditure'Underscoresubject'Underscoreto'Underscorelimits :: Float -- ^
  , baseF3PFilingFec'Underscorefile'Underscoreid :: Text -- ^
  , baseF3PFilingFec'Underscoreurl :: Text -- ^
  , baseF3PFilingFile'Underscorenumber :: Int -- ^
  , baseF3PFilingGeneral'Underscoreelection :: Text -- ^
  , baseF3PFilingIs'Underscoreamended :: Bool -- ^
  , baseF3PFilingMost'Underscorerecent :: Bool -- ^
  , baseF3PFilingMost'Underscorerecent'Underscorefiling :: Int -- ^
  , baseF3PFilingNet'Underscorecontributions'Underscorecycle'Underscoreto'Underscoredate :: Float -- ^
  , baseF3PFilingNet'Underscoreoperating'Underscoreexpenditures'Underscorecycle'Underscoreto'Underscoredate :: Float -- ^
  , baseF3PFilingPdf'Underscoreurl :: Text -- ^
  , baseF3PFilingPrefix :: Text -- ^
  , baseF3PFilingPrimary'Underscoreelection :: Text -- ^
  , baseF3PFilingReceipt'Underscoredate :: Date -- ^
  , baseF3PFilingReport :: Text -- ^
  , baseF3PFilingReport'Underscoretype :: Text -- ^
  , baseF3PFilingReport'Underscoreyear :: Int -- ^
  , baseF3PFilingRpt'Underscorepgi :: Text -- ^ Election type  Convention, Primary, General, Special, Runoff etc.
  , baseF3PFilingSign'Underscoredate :: Date -- ^
  , baseF3PFilingState :: Text -- ^
  , baseF3PFilingStreet'Underscore1 :: Text -- ^
  , baseF3PFilingStreet'Underscore2 :: Text -- ^
  , baseF3PFilingSubtotal'Underscoresummary'Underscoreperiod :: Text -- ^
  , baseF3PFilingSuffix :: Text -- ^
  , baseF3PFilingSummary'Underscorelines :: Text -- ^
  , baseF3PFilingTreasurer'Underscorefirst'Underscorename :: Text -- ^
  , baseF3PFilingTreasurer'Underscorelast'Underscorename :: Text -- ^
  , baseF3PFilingTreasurer'Underscoremiddle'Underscorename :: Text -- ^
  , baseF3PFilingTreasurer'Underscorename :: Text -- ^
  , baseF3PFilingZip :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON BaseF3PFiling where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "baseF3PFiling")
instance ToJSON BaseF3PFiling where
  toJSON = genericToJSON (removeFieldLabelPrefix False "baseF3PFiling")

-- |
data BaseF3PFilingPage = BaseF3PFilingPage
  { baseF3PFilingPagePagination :: OffsetInfo -- ^
  , baseF3PFilingPageResults    :: [BaseF3PFiling] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON BaseF3PFilingPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "baseF3PFilingPage")
instance ToJSON BaseF3PFilingPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "baseF3PFilingPage")

-- |
data BaseF3XFiling = BaseF3XFiling
  { baseF3XFilingAmend'Underscoreaddress                                :: Text -- ^
  , baseF3XFilingAmended'Underscoreby                                   :: Int -- ^
  , baseF3XFilingAmendment                                              :: Text -- ^
  , baseF3XFilingAmendment'Underscorechain                              :: [Int] -- ^
  , baseF3XFilingBeginning'Underscoreimage'Underscorenumber             :: Text -- ^
  , baseF3XFilingCity                                                   :: Text -- ^
  , baseF3XFilingCommittee'Underscoreid                                 :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , baseF3XFilingCommittee'Underscorename                               :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , baseF3XFilingCoverage'Underscoreend'Underscoredate                  :: Date -- ^
  , baseF3XFilingCoverage'Underscorestart'Underscoredate                :: Date -- ^
  , baseF3XFilingCsv'Underscoreurl                                      :: Text -- ^
  , baseF3XFilingDocument'Underscoredescription                         :: Text -- ^
  , baseF3XFilingElection'Underscoredate                                :: Date -- ^
  , baseF3XFilingElection'Underscorestate                               :: Text -- ^
  , baseF3XFilingFec'Underscorefile'Underscoreid                        :: Text -- ^
  , baseF3XFilingFec'Underscoreurl                                      :: Text -- ^
  , baseF3XFilingFile'Underscorenumber                                  :: Int -- ^
  , baseF3XFilingIs'Underscoreamended                                   :: Bool -- ^
  , baseF3XFilingMost'Underscorerecent                                  :: Bool -- ^
  , baseF3XFilingMost'Underscorerecent'Underscorefiling                 :: Int -- ^
  , baseF3XFilingPdf'Underscoreurl                                      :: Text -- ^
  , baseF3XFilingQualified'Underscoremulticandidate'Underscorecommittee :: Text -- ^
  , baseF3XFilingReceipt'Underscoredate                                 :: Date -- ^
  , baseF3XFilingReport                                                 :: Text -- ^
  , baseF3XFilingReport'Underscoretype                                  :: Text -- ^
  , baseF3XFilingReport'Underscoreyear                                  :: Int -- ^
  , baseF3XFilingRpt'Underscorepgi                                      :: Text -- ^ Election type  Convention, Primary, General, Special, Runoff etc.
  , baseF3XFilingSign'Underscoredate                                    :: Date -- ^
  , baseF3XFilingState                                                  :: Text -- ^
  , baseF3XFilingStreet'Underscore1                                     :: Text -- ^
  , baseF3XFilingStreet'Underscore2                                     :: Text -- ^
  , baseF3XFilingSummary'Underscorelines                                :: Text -- ^
  , baseF3XFilingZip                                                    :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON BaseF3XFiling where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "baseF3XFiling")
instance ToJSON BaseF3XFiling where
  toJSON = genericToJSON (removeFieldLabelPrefix False "baseF3XFiling")

-- |
data BaseF3XFilingPage = BaseF3XFilingPage
  { baseF3XFilingPagePagination :: OffsetInfo -- ^
  , baseF3XFilingPageResults    :: [BaseF3XFiling] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON BaseF3XFilingPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "baseF3XFilingPage")
instance ToJSON BaseF3XFilingPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "baseF3XFilingPage")

-- |
data CalendarDate = CalendarDate
  { calendarDateAll'Underscoreday                        :: Bool -- ^
  , calendarDateCalendar'Underscorecategory'Underscoreid :: Int -- ^  Each type of event has a calendar category with an integer id. Options are: Open Meetings: 32, Executive Sessions: 39, Public Hearings: 40, Conferences: 33, Roundtables: 34, Election Dates: 36, Federal Holidays: 37, FEA Periods: 38, Commission Meetings: 20, Reporting Deadlines: 21, Conferences and Outreach: 22, AOs and Rules: 23, Other: 24, Quarterly: 25, Monthly: 26, Pre and Post-Elections: 27, EC Periods:28, and IE Periods: 29
  , calendarDateCategory                                 :: Text -- ^  Each type of event has a calendar category with an integer id. Options are: Open Meetings: 32, Executive Sessions: 39, Public Hearings: 40, Conferences: 33, Roundtables: 34, Election Dates: 36, Federal Holidays: 37, FEA Periods: 38, Commission Meetings: 20, Reporting Deadlines: 21, Conferences and Outreach: 22, AOs and Rules: 23, Other: 24, Quarterly: 25, Monthly: 26, Pre and Post-Elections: 27, EC Periods:28, and IE Periods: 29
  , calendarDateDescription                              :: Text -- ^
  , calendarDateEnd'Underscoredate                       :: Text -- ^
  , calendarDateEvent'Underscoreid                       :: Int -- ^ An unique ID for an event. Useful for downloading a single event to your calendar. This ID is not a permanent, persistent ID.
  , calendarDateLocation                                 :: Text -- ^ Can be state address or room.
  , calendarDateStart'Underscoredate                     :: Text -- ^
  , calendarDateState                                    :: [Text] -- ^ The state field only applies to election dates and reporting deadlines, reporting periods and all other dates do not have the array of states to filter on
  , calendarDateSummary                                  :: Text -- ^
  , calendarDateUrl                                      :: Text -- ^ A url for that event
  } deriving (Show, Eq, Generic)

instance FromJSON CalendarDate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "calendarDate")
instance ToJSON CalendarDate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "calendarDate")

-- |
data CalendarDatePage = CalendarDatePage
  { calendarDatePagePagination :: OffsetInfo -- ^
  , calendarDatePageResults    :: [CalendarDate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CalendarDatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "calendarDatePage")
instance ToJSON CalendarDatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "calendarDatePage")

-- |
data Candidate = Candidate
  { candidateActive'Underscorethrough                     :: Int -- ^ Last year a candidate was active. This field is specific to the candidate_id so if the same person runs for another office, there may be a different record for them.
  , candidateCandidate'Underscoreid                       :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateCandidate'Underscorestatus                   :: Text -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateCycles                                       :: [Int] -- ^  Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the `election_full` flag.
  , candidateDistrict                                     :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateDistrict'Underscorenumber                    :: Int -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateElection'Underscoredistricts                 :: [Text] -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateElection'Underscoreyears                     :: [Int] -- ^ Years in which a candidate ran for office.
  , candidateFederal'Underscorefunds'Underscoreflag       :: Bool -- ^
  , candidateFirst'Underscorefile'Underscoredate          :: Date -- ^ The day the FEC received the candidate's first filing. This is a F2 candidate registration.
  , candidateHas'Underscoreraised'Underscorefunds         :: Bool -- ^
  , candidateIncumbent'Underscorechallenge                :: Text -- ^ One-letter code ('I', 'C', 'O') explaining if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateIncumbent'Underscorechallenge'Underscorefull :: Text -- ^ Explains if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateLast'Underscoref2'Underscoredate             :: Date -- ^ The day the FEC received the candidate's most recent Form 2
  , candidateLast'Underscorefile'Underscoredate           :: Date -- ^ The day the FEC received the candidate's most recent filing
  , candidateLoad'Underscoredate                          :: Date -- ^ Date the information was loaded into the FEC systems. This can be affected by reseting systems and other factors, refer to receipt_date for the day that the FEC received the paper or electronic document. Keep in mind that paper filings take more time to process and there can be a lag between load_date and receipt_date. This field can be helpful to identify paper records that have been processed recently.
  , candidateName                                         :: Text -- ^ Name of candidate running for office
  , candidateOffice                                       :: Text -- ^ Federal office candidate runs for: H, S or P
  , candidateOffice'Underscorefull                        :: Text -- ^ Federal office candidate runs for: House, Senate or presidential
  , candidateParty                                        :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , candidateParty'Underscorefull                         :: Text -- ^ Party affiliated with a candidate or committee
  , candidatePrincipal'Underscorecommittees               :: [Committee] -- ^
  , candidateState                                        :: Text -- ^ US state or territory where a candidate runs for office
  } deriving (Show, Eq, Generic)

instance FromJSON Candidate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidate")
instance ToJSON Candidate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidate")

-- |
data CandidateCommitteeTotalsHouseSenate = CandidateCommitteeTotalsHouseSenate
  { candidateCommitteeTotalsHouseSenateAll'Underscoreother'Underscoreloans :: Double -- ^
  , candidateCommitteeTotalsHouseSenateCandidate'Underscorecontribution :: Double -- ^
  , candidateCommitteeTotalsHouseSenateCandidate'Underscoreid :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateCommitteeTotalsHouseSenateContribution'Underscorerefunds :: Double -- ^
  , candidateCommitteeTotalsHouseSenateContributions :: Double -- ^ Contribution
  , candidateCommitteeTotalsHouseSenateCoverage'Underscoreend'Underscoredate :: Integer -- ^
  , candidateCommitteeTotalsHouseSenateCoverage'Underscorestart'Underscoredate :: Integer -- ^
  , candidateCommitteeTotalsHouseSenateCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , candidateCommitteeTotalsHouseSenateDisbursements :: Double -- ^ Disbursements
  , candidateCommitteeTotalsHouseSenateExempt'Underscorelegal'Underscoreaccounting'Underscoredisbursement :: Double -- ^
  , candidateCommitteeTotalsHouseSenateFederal'Underscorefunds :: Double -- ^
  , candidateCommitteeTotalsHouseSenateFull'Underscoreelection :: Bool -- ^
  , candidateCommitteeTotalsHouseSenateFundraising'Underscoredisbursements :: Double -- ^
  , candidateCommitteeTotalsHouseSenateIndividual'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateIndividual'Underscoreitemized'Underscorecontributions :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less.
  , candidateCommitteeTotalsHouseSenateIndividual'Underscoreunitemized'Underscorecontributions :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total.
  , candidateCommitteeTotalsHouseSenateLast'Underscorebeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , candidateCommitteeTotalsHouseSenateLast'Underscorecash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLast'Underscoredebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLast'Underscoredebts'Underscoreowed'Underscoreto'Underscorecommittee :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLast'Underscorenet'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLast'Underscorenet'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLast'Underscorereport'Underscoretype'Underscorefull :: Text -- ^
  , candidateCommitteeTotalsHouseSenateLast'Underscorereport'Underscoreyear :: Int -- ^
  , candidateCommitteeTotalsHouseSenateLoan'Underscorerepayments :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLoan'Underscorerepayments'Underscorecandidate'Underscoreloans :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLoan'Underscorerepayments'Underscoreother'Underscoreloans :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLoans :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLoans'Underscoremade'Underscoreby'Underscorecandidate :: Double -- ^
  , candidateCommitteeTotalsHouseSenateNet'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateNet'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOffsets'Underscoreto'Underscorefundraising'Underscoreexpenditures :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOffsets'Underscoreto'Underscorelegal'Underscoreaccounting :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOperating'Underscoreexpenditures :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOther'Underscoredisbursements :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOther'Underscorepolitical'Underscorecommittee'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOther'Underscorereceipts :: Double -- ^
  , candidateCommitteeTotalsHouseSenatePolitical'Underscoreparty'Underscorecommittee'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateReceipts :: Double -- ^
  , candidateCommitteeTotalsHouseSenateRefunded'Underscoreindividual'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateTotal'Underscoreoffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , candidateCommitteeTotalsHouseSenateTransaction'Underscorecoverage'Underscoredate :: Integer -- ^
  , candidateCommitteeTotalsHouseSenateTransfers'Underscorefrom'Underscoreother'Underscoreauthorized'Underscorecommittee :: Double -- ^
  , candidateCommitteeTotalsHouseSenateTransfers'Underscoreto'Underscoreother'Underscoreauthorized'Underscorecommittee :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateCommitteeTotalsHouseSenate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateCommitteeTotalsHouseSenate")
instance ToJSON CandidateCommitteeTotalsHouseSenate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateCommitteeTotalsHouseSenate")

-- |
data CandidateCommitteeTotalsHouseSenatePage = CandidateCommitteeTotalsHouseSenatePage
  { candidateCommitteeTotalsHouseSenatePagePagination :: OffsetInfo -- ^
  , candidateCommitteeTotalsHouseSenatePageResults :: [CandidateCommitteeTotalsHouseSenate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateCommitteeTotalsHouseSenatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateCommitteeTotalsHouseSenatePage")
instance ToJSON CandidateCommitteeTotalsHouseSenatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateCommitteeTotalsHouseSenatePage")

-- |
data CandidateCommitteeTotalsPresidential = CandidateCommitteeTotalsPresidential
  { candidateCommitteeTotalsPresidentialCandidate'Underscorecontribution :: Double -- ^
  , candidateCommitteeTotalsPresidentialCandidate'Underscoreid :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateCommitteeTotalsPresidentialContribution'Underscorerefunds :: Double -- ^
  , candidateCommitteeTotalsPresidentialContributions :: Double -- ^ Contribution
  , candidateCommitteeTotalsPresidentialCoverage'Underscoreend'Underscoredate :: Integer -- ^
  , candidateCommitteeTotalsPresidentialCoverage'Underscorestart'Underscoredate :: Integer -- ^
  , candidateCommitteeTotalsPresidentialCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , candidateCommitteeTotalsPresidentialDisbursements :: Double -- ^ Disbursements
  , candidateCommitteeTotalsPresidentialExempt'Underscorelegal'Underscoreaccounting'Underscoredisbursement :: Double -- ^
  , candidateCommitteeTotalsPresidentialFederal'Underscorefunds :: Double -- ^
  , candidateCommitteeTotalsPresidentialFull'Underscoreelection :: Bool -- ^
  , candidateCommitteeTotalsPresidentialFundraising'Underscoredisbursements :: Double -- ^
  , candidateCommitteeTotalsPresidentialIndividual'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialIndividual'Underscoreitemized'Underscorecontributions :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less.
  , candidateCommitteeTotalsPresidentialIndividual'Underscoreunitemized'Underscorecontributions :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total.
  , candidateCommitteeTotalsPresidentialLast'Underscorebeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , candidateCommitteeTotalsPresidentialLast'Underscorecash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^
  , candidateCommitteeTotalsPresidentialLast'Underscoredebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^
  , candidateCommitteeTotalsPresidentialLast'Underscoredebts'Underscoreowed'Underscoreto'Underscorecommittee :: Double -- ^
  , candidateCommitteeTotalsPresidentialLast'Underscorereport'Underscoretype'Underscorefull :: Text -- ^
  , candidateCommitteeTotalsPresidentialLast'Underscorereport'Underscoreyear :: Int -- ^
  , candidateCommitteeTotalsPresidentialLoan'Underscorerepayments'Underscoremade :: Double -- ^
  , candidateCommitteeTotalsPresidentialLoans'Underscorereceived :: Double -- ^
  , candidateCommitteeTotalsPresidentialLoans'Underscorereceived'Underscorefrom'Underscorecandidate :: Double -- ^
  , candidateCommitteeTotalsPresidentialNet'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialNet'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , candidateCommitteeTotalsPresidentialOffsets'Underscoreto'Underscorefundraising'Underscoreexpenditures :: Double -- ^
  , candidateCommitteeTotalsPresidentialOffsets'Underscoreto'Underscorelegal'Underscoreaccounting :: Double -- ^
  , candidateCommitteeTotalsPresidentialOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , candidateCommitteeTotalsPresidentialOperating'Underscoreexpenditures :: Double -- ^
  , candidateCommitteeTotalsPresidentialOther'Underscoredisbursements :: Double -- ^
  , candidateCommitteeTotalsPresidentialOther'Underscoreloans'Underscorereceived :: Double -- ^
  , candidateCommitteeTotalsPresidentialOther'Underscorepolitical'Underscorecommittee'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialOther'Underscorereceipts :: Double -- ^
  , candidateCommitteeTotalsPresidentialPolitical'Underscoreparty'Underscorecommittee'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialReceipts :: Double -- ^
  , candidateCommitteeTotalsPresidentialRefunded'Underscoreindividual'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialRepayments'Underscoreloans'Underscoremade'Underscoreby'Underscorecandidate :: Double -- ^
  , candidateCommitteeTotalsPresidentialRepayments'Underscoreother'Underscoreloans :: Double -- ^
  , candidateCommitteeTotalsPresidentialTotal'Underscoreoffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , candidateCommitteeTotalsPresidentialTransaction'Underscorecoverage'Underscoredate :: Integer -- ^
  , candidateCommitteeTotalsPresidentialTransfers'Underscorefrom'Underscoreaffiliated'Underscorecommittee :: Double -- ^
  , candidateCommitteeTotalsPresidentialTransfers'Underscoreto'Underscoreother'Underscoreauthorized'Underscorecommittee :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateCommitteeTotalsPresidential where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateCommitteeTotalsPresidential")
instance ToJSON CandidateCommitteeTotalsPresidential where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateCommitteeTotalsPresidential")

-- |
data CandidateCommitteeTotalsPresidentialPage = CandidateCommitteeTotalsPresidentialPage
  { candidateCommitteeTotalsPresidentialPagePagination :: OffsetInfo -- ^
  , candidateCommitteeTotalsPresidentialPageResults :: [CandidateCommitteeTotalsPresidential] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateCommitteeTotalsPresidentialPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateCommitteeTotalsPresidentialPage")
instance ToJSON CandidateCommitteeTotalsPresidentialPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateCommitteeTotalsPresidentialPage")

-- |
data CandidateDetail = CandidateDetail
  { candidateDetailActive'Underscorethrough                     :: Int -- ^ Last year a candidate was active. This field is specific to the candidate_id so if the same person runs for another office, there may be a different record for them.
  , candidateDetailAddress'Underscorecity                       :: Text -- ^ City of candidate's address, as reported on their Form 2.
  , candidateDetailAddress'Underscorestate                      :: Text -- ^ State of candidate's address, as reported on their Form 2.
  , candidateDetailAddress'Underscorestreet'Underscore1         :: Text -- ^ Street of candidate's address, as reported on their Form 2.
  , candidateDetailAddress'Underscorestreet'Underscore2         :: Text -- ^ Additional street information of candidate's address, as reported on their Form 2.
  , candidateDetailAddress'Underscorezip                        :: Text -- ^ Zip code of candidate's address, as reported on their Form 2.
  , candidateDetailCandidate'Underscoreid                       :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateDetailCandidate'Underscoreinactive                 :: Bool -- ^ True indicates that a candidate is inactive.
  , candidateDetailCandidate'Underscorestatus                   :: Text -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateDetailCycles                                       :: [Int] -- ^  Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the `election_full` flag.
  , candidateDetailDistrict                                     :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateDetailDistrict'Underscorenumber                    :: Int -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateDetailElection'Underscoredistricts                 :: [Text] -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateDetailElection'Underscoreyears                     :: [Int] -- ^ Years in which a candidate ran for office.
  , candidateDetailFederal'Underscorefunds'Underscoreflag       :: Bool -- ^
  , candidateDetailFirst'Underscorefile'Underscoredate          :: Date -- ^ The day the FEC received the candidate's first filing. This is a F2 candidate registration.
  , candidateDetailFlags                                        :: Text -- ^
  , candidateDetailHas'Underscoreraised'Underscorefunds         :: Bool -- ^
  , candidateDetailIncumbent'Underscorechallenge                :: Text -- ^ One-letter code ('I', 'C', 'O') explaining if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateDetailIncumbent'Underscorechallenge'Underscorefull :: Text -- ^ Explains if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateDetailLast'Underscoref2'Underscoredate             :: Date -- ^ The day the FEC received the candidate's most recent Form 2
  , candidateDetailLast'Underscorefile'Underscoredate           :: Date -- ^ The day the FEC received the candidate's most recent filing
  , candidateDetailLoad'Underscoredate                          :: Date -- ^ Date the information was loaded into the FEC systems. This can be affected by reseting systems and other factors, refer to receipt_date for the day that the FEC received the paper or electronic document. Keep in mind that paper filings take more time to process and there can be a lag between load_date and receipt_date. This field can be helpful to identify paper records that have been processed recently.
  , candidateDetailName                                         :: Text -- ^ Name of candidate running for office
  , candidateDetailOffice                                       :: Text -- ^ Federal office candidate runs for: H, S or P
  , candidateDetailOffice'Underscorefull                        :: Text -- ^ Federal office candidate runs for: House, Senate or presidential
  , candidateDetailParty                                        :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , candidateDetailParty'Underscorefull                         :: Text -- ^ Party affiliated with a candidate or committee
  , candidateDetailState                                        :: Text -- ^ US state or territory where a candidate runs for office
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateDetail where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateDetail")
instance ToJSON CandidateDetail where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateDetail")

-- |
data CandidateDetailPage = CandidateDetailPage
  { candidateDetailPagePagination :: OffsetInfo -- ^
  , candidateDetailPageResults    :: [CandidateDetail] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateDetailPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateDetailPage")
instance ToJSON CandidateDetailPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateDetailPage")

-- |
data CandidateFlags = CandidateFlags
  { candidateFlagsCandidate'Underscoreid                 :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateFlagsFederal'Underscorefunds'Underscoreflag :: Bool -- ^ A boolean the describes if a presidential candidate has accepted federal funds. The flag will be false for House and Senate candidates.
  , candidateFlagsHas'Underscoreraised'Underscorefunds   :: Bool -- ^ A boolean that describes if a candidate's committee has ever received any receipts for their campaign for this particular office. (Candidates have separate candidate IDs for each office.)
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateFlags where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateFlags")
instance ToJSON CandidateFlags where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateFlags")

-- |
data CandidateFlagsPage = CandidateFlagsPage
  { candidateFlagsPagePagination :: OffsetInfo -- ^
  , candidateFlagsPageResults    :: [CandidateFlags] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateFlagsPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateFlagsPage")
instance ToJSON CandidateFlagsPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateFlagsPage")

-- |
data CandidateHistory = CandidateHistory
  { candidateHistoryActive'Underscorethrough                     :: Int -- ^ Last year a candidate was active. This field is specific to the candidate_id so if the same person runs for another office, there may be a different record for them.
  , candidateHistoryAddress'Underscorecity                       :: Text -- ^ City of candidate's address, as reported on their Form 2.
  , candidateHistoryAddress'Underscorestate                      :: Text -- ^ State of candidate's address, as reported on their Form 2.
  , candidateHistoryAddress'Underscorestreet'Underscore1         :: Text -- ^ Street of candidate's address, as reported on their Form 2.
  , candidateHistoryAddress'Underscorestreet'Underscore2         :: Text -- ^ Additional street information of candidate's address, as reported on their Form 2.
  , candidateHistoryAddress'Underscorezip                        :: Text -- ^ Zip code of candidate's address, as reported on their Form 2.
  , candidateHistoryCandidate'Underscoreelection'Underscoreyear  :: Int -- ^ The last year of the cycle for this election.
  , candidateHistoryCandidate'Underscoreid                       :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateHistoryCandidate'Underscoreinactive                 :: Bool -- ^ True indicates that a candidate is inactive.
  , candidateHistoryCandidate'Underscorestatus                   :: Text -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateHistoryCycles                                       :: [Int] -- ^  Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the `election_full` flag.
  , candidateHistoryDistrict                                     :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateHistoryDistrict'Underscorenumber                    :: Int -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateHistoryElection'Underscoredistricts                 :: [Text] -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateHistoryElection'Underscoreyears                     :: [Int] -- ^ Years in which a candidate ran for office.
  , candidateHistoryFirst'Underscorefile'Underscoredate          :: Date -- ^ The day the FEC received the candidate's first filing. This is a F2 candidate registration.
  , candidateHistoryFlags                                        :: Text -- ^
  , candidateHistoryIncumbent'Underscorechallenge                :: Text -- ^ One-letter code ('I', 'C', 'O') explaining if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateHistoryIncumbent'Underscorechallenge'Underscorefull :: Text -- ^ Explains if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateHistoryLast'Underscoref2'Underscoredate             :: Date -- ^ The day the FEC received the candidate's most recent Form 2
  , candidateHistoryLast'Underscorefile'Underscoredate           :: Date -- ^ The day the FEC received the candidate's most recent filing
  , candidateHistoryLoad'Underscoredate                          :: Date -- ^ Date the information was loaded into the FEC systems. This can be affected by reseting systems and other factors, refer to receipt_date for the day that the FEC received the paper or electronic document. Keep in mind that paper filings take more time to process and there can be a lag between load_date and receipt_date. This field can be helpful to identify paper records that have been processed recently.
  , candidateHistoryName                                         :: Text -- ^ Name of candidate running for office
  , candidateHistoryOffice                                       :: Text -- ^ Federal office candidate runs for: H, S or P
  , candidateHistoryOffice'Underscorefull                        :: Text -- ^ Federal office candidate runs for: House, Senate or presidential
  , candidateHistoryParty                                        :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , candidateHistoryParty'Underscorefull                         :: Text -- ^ Party affiliated with a candidate or committee
  , candidateHistoryState                                        :: Text -- ^ US state or territory where a candidate runs for office
  , candidateHistoryTwo'Underscoreyear'Underscoreperiod          :: Int -- ^  Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the `election_full` flag.
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateHistory where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateHistory")
instance ToJSON CandidateHistory where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateHistory")

-- |
data CandidateHistoryPage = CandidateHistoryPage
  { candidateHistoryPagePagination :: OffsetInfo -- ^
  , candidateHistoryPageResults    :: [CandidateHistory] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateHistoryPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateHistoryPage")
instance ToJSON CandidateHistoryPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateHistoryPage")

-- |
data CandidateHistoryTotal = CandidateHistoryTotal
  { candidateHistoryTotalActive'Underscorethrough :: Int -- ^ Last year a candidate was active. This field is specific to the candidate_id so if the same person runs for another office, there may be a different record for them.
  , candidateHistoryTotalAddress'Underscorecity :: Text -- ^ City of candidate's address, as reported on their Form 2.
  , candidateHistoryTotalAddress'Underscorestate :: Text -- ^ State of candidate's address, as reported on their Form 2.
  , candidateHistoryTotalAddress'Underscorestreet'Underscore1 :: Text -- ^ Street of candidate's address, as reported on their Form 2.
  , candidateHistoryTotalAddress'Underscorestreet'Underscore2 :: Text -- ^ Additional street information of candidate's address, as reported on their Form 2.
  , candidateHistoryTotalAddress'Underscorezip :: Text -- ^ Zip code of candidate's address, as reported on their Form 2.
  , candidateHistoryTotalCandidate'Underscoreelection'Underscoreyear :: Int -- ^ The last year of the cycle for this election.
  , candidateHistoryTotalCandidate'Underscoreid :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateHistoryTotalCandidate'Underscoreinactive :: Bool -- ^ True indicates that a candidate is inactive.
  , candidateHistoryTotalCandidate'Underscorestatus :: Text -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateHistoryTotalCash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^
  , candidateHistoryTotalCoverage'Underscoreend'Underscoredate :: Date -- ^ Ending date of the reporting period
  , candidateHistoryTotalCoverage'Underscorestart'Underscoredate :: Date -- ^ Beginning date of the reporting period
  , candidateHistoryTotalCycle :: Int -- ^
  , candidateHistoryTotalCycles :: [Int] -- ^  Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the `election_full` flag.
  , candidateHistoryTotalDebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^
  , candidateHistoryTotalDisbursements :: Double -- ^
  , candidateHistoryTotalDistrict :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateHistoryTotalDistrict'Underscorenumber :: Int -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateHistoryTotalElection'Underscoredistricts :: [Text] -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateHistoryTotalElection'Underscoreyear :: Int -- ^
  , candidateHistoryTotalElection'Underscoreyears :: [Int] -- ^ Years in which a candidate ran for office.
  , candidateHistoryTotalFederal'Underscorefunds'Underscoreflag :: Bool -- ^ A boolean the describes if a presidential candidate has accepted federal funds. The flag will be false for House and Senate candidates.
  , candidateHistoryTotalFirst'Underscorefile'Underscoredate :: Date -- ^ The day the FEC received the candidate's first filing. This is a F2 candidate registration.
  , candidateHistoryTotalFlags :: Text -- ^
  , candidateHistoryTotalHas'Underscoreraised'Underscorefunds :: Bool -- ^ A boolean that describes if a candidate's committee has ever received any receipts for their campaign for this particular office. (Candidates have separate candidate IDs for each office.)
  , candidateHistoryTotalIncumbent'Underscorechallenge :: Text -- ^ One-letter code ('I', 'C', 'O') explaining if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateHistoryTotalIncumbent'Underscorechallenge'Underscorefull :: Text -- ^ Explains if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateHistoryTotalIs'Underscoreelection :: Bool -- ^
  , candidateHistoryTotalLast'Underscoref2'Underscoredate :: Date -- ^ The day the FEC received the candidate's most recent Form 2
  , candidateHistoryTotalLast'Underscorefile'Underscoredate :: Date -- ^ The day the FEC received the candidate's most recent filing
  , candidateHistoryTotalLoad'Underscoredate :: Date -- ^ Date the information was loaded into the FEC systems. This can be affected by reseting systems and other factors, refer to receipt_date for the day that the FEC received the paper or electronic document. Keep in mind that paper filings take more time to process and there can be a lag between load_date and receipt_date. This field can be helpful to identify paper records that have been processed recently.
  , candidateHistoryTotalName :: Text -- ^ Name of candidate running for office
  , candidateHistoryTotalOffice :: Text -- ^ Federal office candidate runs for: H, S or P
  , candidateHistoryTotalOffice'Underscorefull :: Text -- ^ Federal office candidate runs for: House, Senate or presidential
  , candidateHistoryTotalParty :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , candidateHistoryTotalParty'Underscorefull :: Text -- ^ Party affiliated with a candidate or committee
  , candidateHistoryTotalReceipts :: Double -- ^
  , candidateHistoryTotalState :: Text -- ^ US state or territory where a candidate runs for office
  , candidateHistoryTotalTwo'Underscoreyear'Underscoreperiod :: Int -- ^  Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the `election_full` flag.
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateHistoryTotal where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateHistoryTotal")
instance ToJSON CandidateHistoryTotal where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateHistoryTotal")

-- |
data CandidateHistoryTotalPage = CandidateHistoryTotalPage
  { candidateHistoryTotalPagePagination :: OffsetInfo -- ^
  , candidateHistoryTotalPageResults    :: [CandidateHistoryTotal] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateHistoryTotalPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateHistoryTotalPage")
instance ToJSON CandidateHistoryTotalPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateHistoryTotalPage")

-- |
data CandidatePage = CandidatePage
  { candidatePagePagination :: OffsetInfo -- ^
  , candidatePageResults    :: [Candidate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidatePage")
instance ToJSON CandidatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidatePage")

-- |
data CandidateSearch = CandidateSearch
  { candidateSearchId                      :: Text -- ^
  , candidateSearchName                    :: Text -- ^
  , candidateSearchOffice'Underscoresought :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateSearch where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateSearch")
instance ToJSON CandidateSearch where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateSearch")

-- |
data CandidateSearchList = CandidateSearchList
  { candidateSearchListResults :: [CandidateSearch] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateSearchList where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateSearchList")
instance ToJSON CandidateSearchList where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateSearchList")

-- |
data CandidateTotal = CandidateTotal
  { candidateTotalCandidate'Underscoreid :: Text -- ^
  , candidateTotalCash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^
  , candidateTotalCoverage'Underscoreend'Underscoredate :: Date -- ^ Ending date of the reporting period
  , candidateTotalCoverage'Underscorestart'Underscoredate :: Date -- ^ Beginning date of the reporting period
  , candidateTotalCycle :: Int -- ^
  , candidateTotalDebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^
  , candidateTotalDisbursements :: Double -- ^
  , candidateTotalElection'Underscoreyear :: Int -- ^
  , candidateTotalFederal'Underscorefunds'Underscoreflag :: Bool -- ^ A boolean the describes if a presidential candidate has accepted federal funds. The flag will be false for House and Senate candidates.
  , candidateTotalHas'Underscoreraised'Underscorefunds :: Bool -- ^ A boolean that describes if a candidate's committee has ever received any receipts for their campaign for this particular office. (Candidates have separate candidate IDs for each office.)
  , candidateTotalIs'Underscoreelection :: Bool -- ^
  , candidateTotalReceipts :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateTotal where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateTotal")
instance ToJSON CandidateTotal where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateTotal")

-- |
data CandidateTotalPage = CandidateTotalPage
  { candidateTotalPagePagination :: OffsetInfo -- ^
  , candidateTotalPageResults    :: [CandidateTotal] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CandidateTotalPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidateTotalPage")
instance ToJSON CandidateTotalPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidateTotalPage")

-- |
data Committee = Committee
  { committeeCandidate'Underscoreids                    :: [Text] -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , committeeCommittee'Underscoreid                     :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeCommittee'Underscoretype                   :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeCommittee'Underscoretype'Underscorefull    :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeCycles                                     :: [Int] -- ^  A two year election cycle that the committee was active- (after original registration date but before expiration date in FEC Form 1s) The cycle begins with an odd year and is named for its ending, even year.
  , committeeDesignation                                :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeDesignation'Underscorefull                 :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeFiling'Underscorefrequency                 :: Text -- ^ The one-letter      code of the filing frequency:          - A Administratively terminated          - D Debt          - M Monthly filer          - Q Quarterly filer          - T Terminated          - W Waived
  , committeeFirst'Underscorefile'Underscoredate        :: Date -- ^ The day the FEC received the committee's first filing. This is usually a Form 1 committee registration.
  , committeeLast'Underscoref1'Underscoredate           :: Date -- ^ The day the FEC received the committee's most recent Form 1
  , committeeLast'Underscorefile'Underscoredate         :: Date -- ^ The day the FEC received the committee's most recent filing
  , committeeName                                       :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeOrganization'Underscoretype                :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , committeeOrganization'Underscoretype'Underscorefull :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , committeeParty                                      :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , committeeParty'Underscorefull                       :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , committeeState                                      :: Text -- ^ State of the committee's address as filed on the Form 1
  , committeeTreasurer'Underscorename                   :: Text -- ^ Name of the Committee's treasurer. If multiple treasurers for the committee, the most recent treasurer will be shown.
  } deriving (Show, Eq, Generic)

instance FromJSON Committee where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committee")
instance ToJSON Committee where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committee")

-- |
data CommitteeDetail = CommitteeDetail
  { committeeDetailCandidate'Underscoreids                     :: [Text] -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , committeeDetailCity                                        :: Text -- ^ City of committee as reported on the Form 1
  , committeeDetailCommittee'Underscoreid                      :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeDetailCommittee'Underscoretype                    :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeDetailCommittee'Underscoretype'Underscorefull     :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeDetailCustodian'Underscorecity                    :: Text -- ^ City of committee custodian as reported on the Form 1
  , committeeDetailCustodian'Underscorename'Underscore1        :: Text -- ^
  , committeeDetailCustodian'Underscorename'Underscore2        :: Text -- ^
  , committeeDetailCustodian'Underscorename'Underscorefull     :: Text -- ^ Name of custodian
  , committeeDetailCustodian'Underscorename'Underscoremiddle   :: Text -- ^
  , committeeDetailCustodian'Underscorename'Underscoreprefix   :: Text -- ^
  , committeeDetailCustodian'Underscorename'Underscoresuffix   :: Text -- ^
  , committeeDetailCustodian'Underscorename'Underscoretitle    :: Text -- ^
  , committeeDetailCustodian'Underscorephone                   :: Text -- ^ Phone number of the committee custodian as reported on the Form 1
  , committeeDetailCustodian'Underscorestate                   :: Text -- ^ State of the committee custodian as reported on the Form 1
  , committeeDetailCustodian'Underscorestreet'Underscore1      :: Text -- ^ Street address of the committee custodian as reported on the Form 1
  , committeeDetailCustodian'Underscorestreet'Underscore2      :: Text -- ^ Second line of the street address of the committee custodian as reported on the Form 1
  , committeeDetailCustodian'Underscorezip                     :: Text -- ^ Zip code of the committee custodian as reported on the Form 1
  , committeeDetailCycles                                      :: [Int] -- ^  A two year election cycle that the committee was active- (after original registration date but before expiration date in FEC Form 1s) The cycle begins with an odd year and is named for its ending, even year.
  , committeeDetailDesignation                                 :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeDetailDesignation'Underscorefull                  :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeDetailEmail                                       :: Text -- ^ Email as reported on the Form 1
  , committeeDetailFax                                         :: Text -- ^ Fax as reported on the Form 1
  , committeeDetailFiling'Underscorefrequency                  :: Text -- ^ The one-letter      code of the filing frequency:          - A Administratively terminated          - D Debt          - M Monthly filer          - Q Quarterly filer          - T Terminated          - W Waived
  , committeeDetailFirst'Underscorefile'Underscoredate         :: Date -- ^ The day the FEC received the committee's first filing. This is usually a Form 1 committee registration.
  , committeeDetailForm'Underscoretype                         :: Text -- ^ Form where the information was reported
  , committeeDetailLast'Underscorefile'Underscoredate          :: Date -- ^ The day the FEC received the committee's most recent filing
  , committeeDetailLeadership'Underscorepac                    :: Text -- ^ Indicates if the committee is a leadership PAC
  , committeeDetailLobbyist'Underscoreregistrant'Underscorepac :: Text -- ^ Indicates if the committee is a lobbyist registrant PAC
  , committeeDetailName                                        :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeDetailOrganization'Underscoretype                 :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , committeeDetailOrganization'Underscoretype'Underscorefull  :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , committeeDetailParty                                       :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , committeeDetailParty'Underscorefull                        :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , committeeDetailParty'Underscoretype                        :: Text -- ^ Code for the type of party the committee is, only if applicable
  , committeeDetailParty'Underscoretype'Underscorefull         :: Text -- ^ Description of the type of party the committee is, only if applicable
  , committeeDetailQualifying'Underscoredate                   :: Date -- ^ Date the committee became a qualified committee.
  , committeeDetailState                                       :: Text -- ^ State of the committee's address as filed on the Form 1
  , committeeDetailState'Underscorefull                        :: Text -- ^ State of committee as reported on the Form 1
  , committeeDetailStreet'Underscore1                          :: Text -- ^ Street address of committee as reported on the Form 1
  , committeeDetailStreet'Underscore2                          :: Text -- ^ Second line of street address of committee as reported on the Form 1
  , committeeDetailTreasurer'Underscorecity                    :: Text -- ^ City of committee treasurer as reported on the Form 1
  , committeeDetailTreasurer'Underscorename                    :: Text -- ^ Name of the Committee's treasurer. If multiple treasurers for the committee, the most recent treasurer will be shown.
  , committeeDetailTreasurer'Underscorename'Underscore1        :: Text -- ^
  , committeeDetailTreasurer'Underscorename'Underscore2        :: Text -- ^
  , committeeDetailTreasurer'Underscorename'Underscoremiddle   :: Text -- ^
  , committeeDetailTreasurer'Underscorename'Underscoreprefix   :: Text -- ^
  , committeeDetailTreasurer'Underscorename'Underscoresuffix   :: Text -- ^
  , committeeDetailTreasurer'Underscorename'Underscoretitle    :: Text -- ^
  , committeeDetailTreasurer'Underscorephone                   :: Text -- ^ Phone number of the committee treasurer as reported on the Form 1
  , committeeDetailTreasurer'Underscorestate                   :: Text -- ^ State of the committee treasurer as reported on the Form 1
  , committeeDetailTreasurer'Underscorestreet'Underscore1      :: Text -- ^ Street of the committee treasurer as reported on the Form 1
  , committeeDetailTreasurer'Underscorestreet'Underscore2      :: Text -- ^ Second line of the street address of the committee treasurer as reported on the Form 1
  , committeeDetailTreasurer'Underscorezip                     :: Text -- ^ Zip code of the committee treasurer as reported on the Form 1
  , committeeDetailWebsite                                     :: Text -- ^ Website url as reported on the Form 1
  , committeeDetailZip                                         :: Text -- ^ Zip code of committee as reported on the Form 1
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeDetail where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeDetail")
instance ToJSON CommitteeDetail where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeDetail")

-- |
data CommitteeDetailPage = CommitteeDetailPage
  { committeeDetailPagePagination :: OffsetInfo -- ^
  , committeeDetailPageResults    :: [CommitteeDetail] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeDetailPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeDetailPage")
instance ToJSON CommitteeDetailPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeDetailPage")

-- |
data CommitteeHistory = CommitteeHistory
  { committeeHistoryCandidate'Underscoreids                    :: [Text] -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , committeeHistoryCity                                       :: Text -- ^ City of committee as reported on the Form 1
  , committeeHistoryCommittee'Underscoreid                     :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeHistoryCommittee'Underscoretype                   :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeHistoryCommittee'Underscoretype'Underscorefull    :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeHistoryCycle                                      :: Int -- ^  A two year election cycle that the committee was active- (after original registration date but before expiration date in FEC Form 1s) The cycle begins with an odd year and is named for its ending, even year.
  , committeeHistoryCycles                                     :: [Int] -- ^  A two year election cycle that the committee was active- (after original registration date but before expiration date in FEC Form 1s) The cycle begins with an odd year and is named for its ending, even year.
  , committeeHistoryDesignation                                :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeHistoryDesignation'Underscorefull                 :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeHistoryFiling'Underscorefrequency                 :: Text -- ^ The one-letter      code of the filing frequency:          - A Administratively terminated          - D Debt          - M Monthly filer          - Q Quarterly filer          - T Terminated          - W Waived
  , committeeHistoryName                                       :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeHistoryOrganization'Underscoretype                :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , committeeHistoryOrganization'Underscoretype'Underscorefull :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , committeeHistoryParty                                      :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , committeeHistoryParty'Underscorefull                       :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , committeeHistoryState                                      :: Text -- ^ State of the committee's address as filed on the Form 1
  , committeeHistoryState'Underscorefull                       :: Text -- ^ State of committee as reported on the Form 1
  , committeeHistoryStreet'Underscore1                         :: Text -- ^ Street address of committee as reported on the Form 1
  , committeeHistoryStreet'Underscore2                         :: Text -- ^ Second line of street address of committee as reported on the Form 1
  , committeeHistoryTreasurer'Underscorename                   :: Text -- ^ Name of the Committee's treasurer. If multiple treasurers for the committee, the most recent treasurer will be shown.
  , committeeHistoryZip                                        :: Text -- ^ Zip code of committee as reported on the Form 1
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeHistory where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeHistory")
instance ToJSON CommitteeHistory where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeHistory")

-- |
data CommitteeHistoryPage = CommitteeHistoryPage
  { committeeHistoryPagePagination :: OffsetInfo -- ^
  , committeeHistoryPageResults    :: [CommitteeHistory] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeHistoryPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeHistoryPage")
instance ToJSON CommitteeHistoryPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeHistoryPage")

-- |
data CommitteePage = CommitteePage
  { committeePagePagination :: OffsetInfo -- ^
  , committeePageResults    :: [Committee] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeePage")
instance ToJSON CommitteePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeePage")

-- |
data CommitteeReports = CommitteeReports
  { committeeReportsAggregate'Underscoreamount'Underscorepersonal'Underscorecontributions'Underscoregeneral :: Double -- ^
  , committeeReportsAggregate'Underscorecontributions'Underscorepersonal'Underscorefunds'Underscoreprimary :: Double -- ^
  , committeeReportsAll'Underscoreloans'Underscorereceived'Underscoreperiod :: Double -- ^
  , committeeReportsAll'Underscoreloans'Underscorereceived'Underscoreytd :: Double -- ^
  , committeeReportsAll'Underscoreother'Underscoreloans'Underscoreperiod :: Double -- ^
  , committeeReportsAll'Underscoreother'Underscoreloans'Underscoreytd :: Double -- ^
  , committeeReportsAllocated'Underscorefederal'Underscoreelection'Underscorelevin'Underscoreshare'Underscoreperiod :: Double -- ^
  , committeeReportsAmendment'Underscorechain :: [Double] -- ^  The first value in the chain is the original filing.  The ordering in the chain reflects the order the amendments were filed up to the amendment being viewed.
  , committeeReportsAmendment'Underscoreindicator :: Text -- ^
  , committeeReportsAmendment'Underscoreindicator'Underscorefull :: Text -- ^
  , committeeReportsBeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeReportsCalendar'Underscoreytd :: Int -- ^
  , committeeReportsCandidate'Underscorecontribution'Underscoreperiod :: Double -- ^
  , committeeReportsCandidate'Underscorecontribution'Underscoreytd :: Double -- ^
  , committeeReportsCash'Underscoreon'Underscorehand'Underscorebeginning'Underscorecalendar'Underscoreytd :: Double -- ^
  , committeeReportsCash'Underscoreon'Underscorehand'Underscorebeginning'Underscoreperiod :: Double -- ^ Balance for the committee at the start of the two-year period
  , committeeReportsCash'Underscoreon'Underscorehand'Underscoreclose'Underscoreytd :: Double -- ^
  , committeeReportsCash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^ Ending cash balance on the most recent filing
  , committeeReportsCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeReportsCommittee'Underscorename :: Text -- ^
  , committeeReportsCommittee'Underscoretype :: Text -- ^
  , committeeReportsCoordinated'Underscoreexpenditures'Underscoreby'Underscoreparty'Underscorecommittee'Underscoreperiod :: Double -- ^
  , committeeReportsCoordinated'Underscoreexpenditures'Underscoreby'Underscoreparty'Underscorecommittee'Underscoreytd :: Double -- ^
  , committeeReportsCoverage'Underscoreend'Underscoredate :: Integer -- ^ Ending date of the reporting period
  , committeeReportsCoverage'Underscorestart'Underscoredate :: Integer -- ^ Beginning date of the reporting period
  , committeeReportsCsv'Underscoreurl :: Text -- ^
  , committeeReportsCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeReportsDebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^ Debts owed by the committee
  , committeeReportsDebts'Underscoreowed'Underscoreto'Underscorecommittee :: Double -- ^ Debts owed to the committee
  , committeeReportsDocument'Underscoredescription :: Text -- ^
  , committeeReportsEnd'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeReportsExempt'Underscorelegal'Underscoreaccounting'Underscoredisbursement'Underscoreperiod :: Double -- ^
  , committeeReportsExempt'Underscorelegal'Underscoreaccounting'Underscoredisbursement'Underscoreytd :: Double -- ^
  , committeeReportsExpenditure'Underscoresubject'Underscoreto'Underscorelimits :: Double -- ^
  , committeeReportsFec'Underscorefile'Underscoreid :: Text -- ^
  , committeeReportsFec'Underscoreurl :: Text -- ^
  , committeeReportsFed'Underscorecandidate'Underscorecommittee'Underscorecontribution'Underscorerefunds'Underscoreytd :: Double -- ^
  , committeeReportsFed'Underscorecandidate'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^
  , committeeReportsFed'Underscorecandidate'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^
  , committeeReportsFed'Underscorecandidate'Underscorecontribution'Underscorerefunds'Underscoreperiod :: Double -- ^
  , committeeReportsFederal'Underscorefunds'Underscoreperiod :: Double -- ^
  , committeeReportsFederal'Underscorefunds'Underscoreytd :: Double -- ^
  , committeeReportsFile'Underscorenumber :: Int -- ^
  , committeeReportsFundraising'Underscoredisbursements'Underscoreperiod :: Double -- ^
  , committeeReportsFundraising'Underscoredisbursements'Underscoreytd :: Double -- ^
  , committeeReportsGross'Underscorereceipt'Underscoreauthorized'Underscorecommittee'Underscoregeneral :: Double -- ^
  , committeeReportsGross'Underscorereceipt'Underscoreauthorized'Underscorecommittee'Underscoreprimary :: Double -- ^
  , committeeReportsGross'Underscorereceipt'Underscoreminus'Underscorepersonal'Underscorecontribution'Underscoregeneral :: Double -- ^
  , committeeReportsGross'Underscorereceipt'Underscoreminus'Underscorepersonal'Underscorecontributions'Underscoreprimary :: Double -- ^
  , committeeReportsHtml'Underscoreurl :: Text -- ^ HTML link to the filing.
  , committeeReportsIndependent'Underscorecontributions'Underscoreperiod :: Double -- ^
  , committeeReportsIndependent'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsIndependent'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsIndividual'Underscoreitemized'Underscorecontributions'Underscoreperiod :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the reporting period
  , committeeReportsIndividual'Underscoreitemized'Underscorecontributions'Underscoreytd :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the year to date
  , committeeReportsIndividual'Underscoreunitemized'Underscorecontributions'Underscoreperiod :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the reporting period
  , committeeReportsIndividual'Underscoreunitemized'Underscorecontributions'Underscoreytd :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the year to date
  , committeeReportsIs'Underscoreamended :: Bool -- ^ False indicates that a report is the most recent. True indicates that the report has been superseded by an amendment.
  , committeeReportsItems'Underscoreon'Underscorehand'Underscoreliquidated :: Double -- ^
  , committeeReportsLoan'Underscorerepayments'Underscorecandidate'Underscoreloans'Underscoreperiod :: Double -- ^
  , committeeReportsLoan'Underscorerepayments'Underscorecandidate'Underscoreloans'Underscoreytd :: Double -- ^
  , committeeReportsLoan'Underscorerepayments'Underscoremade'Underscoreperiod :: Double -- ^
  , committeeReportsLoan'Underscorerepayments'Underscoremade'Underscoreytd :: Double -- ^
  , committeeReportsLoan'Underscorerepayments'Underscoreother'Underscoreloans'Underscoreperiod :: Double -- ^
  , committeeReportsLoan'Underscorerepayments'Underscoreother'Underscoreloans'Underscoreytd :: Double -- ^
  , committeeReportsLoan'Underscorerepayments'Underscorereceived'Underscoreperiod :: Double -- ^
  , committeeReportsLoan'Underscorerepayments'Underscorereceived'Underscoreytd :: Double -- ^
  , committeeReportsLoans'Underscoremade'Underscoreby'Underscorecandidate'Underscoreperiod :: Double -- ^
  , committeeReportsLoans'Underscoremade'Underscoreby'Underscorecandidate'Underscoreytd :: Double -- ^
  , committeeReportsLoans'Underscoremade'Underscoreperiod :: Double -- ^
  , committeeReportsLoans'Underscoremade'Underscoreytd :: Double -- ^
  , committeeReportsLoans'Underscorereceived'Underscorefrom'Underscorecandidate'Underscoreperiod :: Double -- ^
  , committeeReportsLoans'Underscorereceived'Underscorefrom'Underscorecandidate'Underscoreytd :: Double -- ^
  , committeeReportsMeans'Underscorefiled :: Text -- ^ The method used to file with the FEC, either electronic or on paper.
  , committeeReportsMost'Underscorerecent :: Bool -- ^
  , committeeReportsMost'Underscorerecent'Underscorefile'Underscorenumber :: Double -- ^
  , committeeReportsNet'Underscorecontributions'Underscorecycle'Underscoreto'Underscoredate :: Double -- ^
  , committeeReportsNet'Underscorecontributions'Underscoreperiod :: Double -- ^
  , committeeReportsNet'Underscorecontributions'Underscoreytd :: Double -- ^
  , committeeReportsNet'Underscoreoperating'Underscoreexpenditures'Underscorecycle'Underscoreto'Underscoredate :: Double -- ^
  , committeeReportsNet'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsNet'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsNon'Underscoreallocated'Underscorefed'Underscoreelection'Underscoreactivity'Underscoreperiod :: Double -- ^
  , committeeReportsNon'Underscoreallocated'Underscorefed'Underscoreelection'Underscoreactivity'Underscoreytd :: Double -- ^
  , committeeReportsNonfed'Underscoreshare'Underscoreallocated'Underscoredisbursements'Underscoreperiod :: Double -- ^
  , committeeReportsOffsets'Underscoreto'Underscorefundraising'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsOffsets'Underscoreto'Underscorefundraising'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsOffsets'Underscoreto'Underscorelegal'Underscoreaccounting'Underscoreperiod :: Double -- ^
  , committeeReportsOffsets'Underscoreto'Underscorelegal'Underscoreaccounting'Underscoreytd :: Double -- ^
  , committeeReportsOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^ Offsets to operating expenditures total for the reporting period
  , committeeReportsOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^ Offsets to operating expenditures total for the year to date
  , committeeReportsOperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsOperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsOther'Underscoredisbursements'Underscoreperiod :: Double -- ^ Other disbursements total for the reporting period
  , committeeReportsOther'Underscoredisbursements'Underscoreytd :: Double -- ^ Other disbursements total for the year to date
  , committeeReportsOther'Underscorefed'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsOther'Underscorefed'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsOther'Underscorefed'Underscorereceipts'Underscoreperiod :: Double -- ^
  , committeeReportsOther'Underscorefed'Underscorereceipts'Underscoreytd :: Double -- ^
  , committeeReportsOther'Underscoreloans'Underscorereceived'Underscoreperiod :: Double -- ^
  , committeeReportsOther'Underscoreloans'Underscorereceived'Underscoreytd :: Double -- ^
  , committeeReportsOther'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Other committees contributions total for the reporting period
  , committeeReportsOther'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Other committees contributions total for the year to date
  , committeeReportsOther'Underscorereceipts'Underscoreperiod :: Double -- ^
  , committeeReportsOther'Underscorereceipts'Underscoreytd :: Double -- ^
  , committeeReportsPdf'Underscoreurl :: Text -- ^
  , committeeReportsPolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Party committees contributions total for the reporting period
  , committeeReportsPolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Party committees contributions total for the year to date
  , committeeReportsPrevious'Underscorefile'Underscorenumber :: Double -- ^
  , committeeReportsReceipt'Underscoredate :: Date -- ^ Date the FEC received the electronic or paper record
  , committeeReportsRefunded'Underscoreindividual'Underscorecontributions'Underscoreperiod :: Double -- ^ Individual refunds total for the reporting period
  , committeeReportsRefunded'Underscoreindividual'Underscorecontributions'Underscoreytd :: Double -- ^ Individual refunds total for the year to date
  , committeeReportsRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Other committee refunds total for the reporting period
  , committeeReportsRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Other committee refunds total for the year to date
  , committeeReportsRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Political party refunds total for the reporting period
  , committeeReportsRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Political party refunds total for the year to date
  , committeeReportsRefunds'Underscoretotal'Underscorecontributions'Underscorecol'Underscoretotal'Underscoreytd :: Double -- ^
  , committeeReportsRepayments'Underscoreloans'Underscoremade'Underscoreby'Underscorecandidate'Underscoreperiod :: Double -- ^
  , committeeReportsRepayments'Underscoreloans'Underscoremade'Underscorecandidate'Underscoreytd :: Double -- ^
  , committeeReportsRepayments'Underscoreother'Underscoreloans'Underscoreperiod :: Double -- ^
  , committeeReportsRepayments'Underscoreother'Underscoreloans'Underscoreytd :: Double -- ^
  , committeeReportsReport'Underscoreform :: Text -- ^
  , committeeReportsReport'Underscoretype :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsReport'Underscoretype'Underscorefull :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsReport'Underscoreyear :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , committeeReportsShared'Underscorefed'Underscoreactivity'Underscorenonfed'Underscoreytd :: Double -- ^
  , committeeReportsShared'Underscorefed'Underscoreactivity'Underscoreperiod :: Double -- ^
  , committeeReportsShared'Underscorefed'Underscoreactivity'Underscoreytd :: Double -- ^
  , committeeReportsShared'Underscorefed'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsShared'Underscorefed'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsShared'Underscorenonfed'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsShared'Underscorenonfed'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsSubtotal'Underscoreperiod :: Double -- ^
  , committeeReportsSubtotal'Underscoresummary'Underscorepage'Underscoreperiod :: Double -- ^
  , committeeReportsSubtotal'Underscoresummary'Underscoreperiod :: Double -- ^
  , committeeReportsSubtotal'Underscoresummary'Underscoreytd :: Double -- ^
  , committeeReportsTotal'Underscorecontribution'Underscorerefunds'Underscorecol'Underscoretotal'Underscoreperiod :: Double -- ^
  , committeeReportsTotal'Underscorecontribution'Underscorerefunds'Underscoreperiod :: Double -- ^ Total contribution refunds total for the reporting period
  , committeeReportsTotal'Underscorecontribution'Underscorerefunds'Underscoreytd :: Double -- ^ Total contribution refunds total for the year to date
  , committeeReportsTotal'Underscorecontributions'Underscorecolumn'Underscoretotal'Underscoreperiod :: Double -- ^
  , committeeReportsTotal'Underscorecontributions'Underscoreperiod :: Double -- ^ Contribution total for the reporting period
  , committeeReportsTotal'Underscorecontributions'Underscoreytd :: Double -- ^ Contribution total for the year to date
  , committeeReportsTotal'Underscoredisbursements'Underscoreperiod :: Double -- ^ Disbursements total for the reporting period
  , committeeReportsTotal'Underscoredisbursements'Underscoreytd :: Double -- ^ Disbursements total for the year to date
  , committeeReportsTotal'Underscorefed'Underscoredisbursements'Underscoreperiod :: Double -- ^
  , committeeReportsTotal'Underscorefed'Underscoredisbursements'Underscoreytd :: Double -- ^
  , committeeReportsTotal'Underscorefed'Underscoreelection'Underscoreactivity'Underscoreperiod :: Double -- ^
  , committeeReportsTotal'Underscorefed'Underscoreelection'Underscoreactivity'Underscoreytd :: Double -- ^
  , committeeReportsTotal'Underscorefed'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsTotal'Underscorefed'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsTotal'Underscorefed'Underscorereceipts'Underscoreperiod :: Double -- ^
  , committeeReportsTotal'Underscorefed'Underscorereceipts'Underscoreytd :: Double -- ^
  , committeeReportsTotal'Underscoreindividual'Underscorecontributions'Underscoreperiod :: Double -- ^ Individual contributions total for the reporting period
  , committeeReportsTotal'Underscoreindividual'Underscorecontributions'Underscoreytd :: Double -- ^ Individual contributions total for the year to date
  , committeeReportsTotal'Underscoreloan'Underscorerepayments'Underscoremade'Underscoreperiod :: Double -- ^
  , committeeReportsTotal'Underscoreloan'Underscorerepayments'Underscoremade'Underscoreytd :: Double -- ^
  , committeeReportsTotal'Underscoreloans'Underscorereceived'Underscoreperiod :: Double -- ^
  , committeeReportsTotal'Underscoreloans'Underscorereceived'Underscoreytd :: Double -- ^
  , committeeReportsTotal'Underscorenonfed'Underscoretransfers'Underscoreperiod :: Double -- ^
  , committeeReportsTotal'Underscorenonfed'Underscoretransfers'Underscoreytd :: Double -- ^
  , committeeReportsTotal'Underscoreoffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsTotal'Underscoreoffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsTotal'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsTotal'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsTotal'Underscoreperiod :: Double -- ^
  , committeeReportsTotal'Underscorereceipts'Underscoreperiod :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the reporting period
  , committeeReportsTotal'Underscorereceipts'Underscoreytd :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the year to date
  , committeeReportsTotal'Underscoreytd :: Double -- ^
  , committeeReportsTransfers'Underscorefrom'Underscoreaffiliated'Underscorecommittee'Underscoreperiod :: Double -- ^
  , committeeReportsTransfers'Underscorefrom'Underscoreaffiliated'Underscorecommittee'Underscoreytd :: Double -- ^
  , committeeReportsTransfers'Underscorefrom'Underscoreaffiliated'Underscoreparty'Underscoreperiod :: Double -- ^
  , committeeReportsTransfers'Underscorefrom'Underscoreaffiliated'Underscoreparty'Underscoreytd :: Double -- ^
  , committeeReportsTransfers'Underscorefrom'Underscorenonfed'Underscoreaccount'Underscoreperiod :: Double -- ^
  , committeeReportsTransfers'Underscorefrom'Underscorenonfed'Underscoreaccount'Underscoreytd :: Double -- ^
  , committeeReportsTransfers'Underscorefrom'Underscorenonfed'Underscorelevin'Underscoreperiod :: Double -- ^
  , committeeReportsTransfers'Underscorefrom'Underscorenonfed'Underscorelevin'Underscoreytd :: Double -- ^
  , committeeReportsTransfers'Underscorefrom'Underscoreother'Underscoreauthorized'Underscorecommittee'Underscoreperiod :: Double -- ^
  , committeeReportsTransfers'Underscorefrom'Underscoreother'Underscoreauthorized'Underscorecommittee'Underscoreytd :: Double -- ^
  , committeeReportsTransfers'Underscoreto'Underscoreaffiliated'Underscorecommittee'Underscoreperiod :: Double -- ^
  , committeeReportsTransfers'Underscoreto'Underscoreaffilitated'Underscorecommittees'Underscoreytd :: Double -- ^
  , committeeReportsTransfers'Underscoreto'Underscoreother'Underscoreauthorized'Underscorecommittee'Underscoreperiod :: Double -- ^
  , committeeReportsTransfers'Underscoreto'Underscoreother'Underscoreauthorized'Underscorecommittee'Underscoreytd :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeReports where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeReports")
instance ToJSON CommitteeReports where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeReports")

-- |
data CommitteeReportsHouseSenate = CommitteeReportsHouseSenate
  { committeeReportsHouseSenateAggregate'Underscoreamount'Underscorepersonal'Underscorecontributions'Underscoregeneral :: Double -- ^
  , committeeReportsHouseSenateAggregate'Underscorecontributions'Underscorepersonal'Underscorefunds'Underscoreprimary :: Double -- ^
  , committeeReportsHouseSenateAll'Underscoreother'Underscoreloans'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateAll'Underscoreother'Underscoreloans'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateAmendment'Underscorechain :: [Double] -- ^  The first value in the chain is the original filing.  The ordering in the chain reflects the order the amendments were filed up to the amendment being viewed.
  , committeeReportsHouseSenateAmendment'Underscoreindicator :: Text -- ^
  , committeeReportsHouseSenateAmendment'Underscoreindicator'Underscorefull :: Text -- ^
  , committeeReportsHouseSenateBeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeReportsHouseSenateCandidate'Underscorecontribution'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateCandidate'Underscorecontribution'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateCash'Underscoreon'Underscorehand'Underscorebeginning'Underscoreperiod :: Double -- ^ Balance for the committee at the start of the two-year period
  , committeeReportsHouseSenateCash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^ Ending cash balance on the most recent filing
  , committeeReportsHouseSenateCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeReportsHouseSenateCommittee'Underscorename :: Text -- ^
  , committeeReportsHouseSenateCommittee'Underscoretype :: Text -- ^
  , committeeReportsHouseSenateCoverage'Underscoreend'Underscoredate :: Integer -- ^ Ending date of the reporting period
  , committeeReportsHouseSenateCoverage'Underscorestart'Underscoredate :: Integer -- ^ Beginning date of the reporting period
  , committeeReportsHouseSenateCsv'Underscoreurl :: Text -- ^
  , committeeReportsHouseSenateCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeReportsHouseSenateDebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^ Debts owed by the committee
  , committeeReportsHouseSenateDebts'Underscoreowed'Underscoreto'Underscorecommittee :: Double -- ^ Debts owed to the committee
  , committeeReportsHouseSenateDocument'Underscoredescription :: Text -- ^
  , committeeReportsHouseSenateEnd'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeReportsHouseSenateFec'Underscorefile'Underscoreid :: Text -- ^
  , committeeReportsHouseSenateFec'Underscoreurl :: Text -- ^
  , committeeReportsHouseSenateFile'Underscorenumber :: Int -- ^
  , committeeReportsHouseSenateGross'Underscorereceipt'Underscoreauthorized'Underscorecommittee'Underscoregeneral :: Double -- ^
  , committeeReportsHouseSenateGross'Underscorereceipt'Underscoreauthorized'Underscorecommittee'Underscoreprimary :: Double -- ^
  , committeeReportsHouseSenateGross'Underscorereceipt'Underscoreminus'Underscorepersonal'Underscorecontribution'Underscoregeneral :: Double -- ^
  , committeeReportsHouseSenateGross'Underscorereceipt'Underscoreminus'Underscorepersonal'Underscorecontributions'Underscoreprimary :: Double -- ^
  , committeeReportsHouseSenateHtml'Underscoreurl :: Text -- ^ HTML link to the filing.
  , committeeReportsHouseSenateIndividual'Underscoreitemized'Underscorecontributions'Underscoreperiod :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the reporting period
  , committeeReportsHouseSenateIndividual'Underscoreitemized'Underscorecontributions'Underscoreytd :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the year to date
  , committeeReportsHouseSenateIndividual'Underscoreunitemized'Underscorecontributions'Underscoreperiod :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the reporting period
  , committeeReportsHouseSenateIndividual'Underscoreunitemized'Underscorecontributions'Underscoreytd :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the year to date
  , committeeReportsHouseSenateIs'Underscoreamended :: Bool -- ^ False indicates that a report is the most recent. True indicates that the report has been superseded by an amendment.
  , committeeReportsHouseSenateLoan'Underscorerepayments'Underscorecandidate'Underscoreloans'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateLoan'Underscorerepayments'Underscorecandidate'Underscoreloans'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateLoan'Underscorerepayments'Underscoreother'Underscoreloans'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateLoan'Underscorerepayments'Underscoreother'Underscoreloans'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateLoans'Underscoremade'Underscoreby'Underscorecandidate'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateLoans'Underscoremade'Underscoreby'Underscorecandidate'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateMeans'Underscorefiled :: Text -- ^ The method used to file with the FEC, either electronic or on paper.
  , committeeReportsHouseSenateMost'Underscorerecent :: Bool -- ^
  , committeeReportsHouseSenateMost'Underscorerecent'Underscorefile'Underscorenumber :: Double -- ^
  , committeeReportsHouseSenateNet'Underscorecontributions'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateNet'Underscorecontributions'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateNet'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateNet'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^ Offsets to operating expenditures total for the reporting period
  , committeeReportsHouseSenateOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^ Offsets to operating expenditures total for the year to date
  , committeeReportsHouseSenateOperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateOperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateOther'Underscoredisbursements'Underscoreperiod :: Double -- ^ Other disbursements total for the reporting period
  , committeeReportsHouseSenateOther'Underscoredisbursements'Underscoreytd :: Double -- ^ Other disbursements total for the year to date
  , committeeReportsHouseSenateOther'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Other committees contributions total for the reporting period
  , committeeReportsHouseSenateOther'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Other committees contributions total for the year to date
  , committeeReportsHouseSenateOther'Underscorereceipts'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateOther'Underscorereceipts'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenatePdf'Underscoreurl :: Text -- ^
  , committeeReportsHouseSenatePolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Party committees contributions total for the reporting period
  , committeeReportsHouseSenatePolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Party committees contributions total for the year to date
  , committeeReportsHouseSenatePrevious'Underscorefile'Underscorenumber :: Double -- ^
  , committeeReportsHouseSenateReceipt'Underscoredate :: Date -- ^ Date the FEC received the electronic or paper record
  , committeeReportsHouseSenateRefunded'Underscoreindividual'Underscorecontributions'Underscoreperiod :: Double -- ^ Individual refunds total for the reporting period
  , committeeReportsHouseSenateRefunded'Underscoreindividual'Underscorecontributions'Underscoreytd :: Double -- ^ Individual refunds total for the year to date
  , committeeReportsHouseSenateRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Other committee refunds total for the reporting period
  , committeeReportsHouseSenateRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Other committee refunds total for the year to date
  , committeeReportsHouseSenateRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Political party refunds total for the reporting period
  , committeeReportsHouseSenateRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Political party refunds total for the year to date
  , committeeReportsHouseSenateRefunds'Underscoretotal'Underscorecontributions'Underscorecol'Underscoretotal'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateReport'Underscoreform :: Text -- ^
  , committeeReportsHouseSenateReport'Underscoretype :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsHouseSenateReport'Underscoretype'Underscorefull :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsHouseSenateReport'Underscoreyear :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , committeeReportsHouseSenateSubtotal'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateTotal'Underscorecontribution'Underscorerefunds'Underscorecol'Underscoretotal'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateTotal'Underscorecontribution'Underscorerefunds'Underscoreperiod :: Double -- ^ Total contribution refunds total for the reporting period
  , committeeReportsHouseSenateTotal'Underscorecontribution'Underscorerefunds'Underscoreytd :: Double -- ^ Total contribution refunds total for the year to date
  , committeeReportsHouseSenateTotal'Underscorecontributions'Underscorecolumn'Underscoretotal'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateTotal'Underscorecontributions'Underscoreperiod :: Double -- ^ Contribution total for the reporting period
  , committeeReportsHouseSenateTotal'Underscorecontributions'Underscoreytd :: Double -- ^ Contribution total for the year to date
  , committeeReportsHouseSenateTotal'Underscoredisbursements'Underscoreperiod :: Double -- ^ Disbursements total for the reporting period
  , committeeReportsHouseSenateTotal'Underscoredisbursements'Underscoreytd :: Double -- ^ Disbursements total for the year to date
  , committeeReportsHouseSenateTotal'Underscoreindividual'Underscorecontributions'Underscoreperiod :: Double -- ^ Individual contributions total for the reporting period
  , committeeReportsHouseSenateTotal'Underscoreindividual'Underscorecontributions'Underscoreytd :: Double -- ^ Individual contributions total for the year to date
  , committeeReportsHouseSenateTotal'Underscoreloan'Underscorerepayments'Underscoremade'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateTotal'Underscoreloan'Underscorerepayments'Underscoremade'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateTotal'Underscoreloans'Underscorereceived'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateTotal'Underscoreloans'Underscorereceived'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateTotal'Underscoreoffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateTotal'Underscoreoffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateTotal'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateTotal'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateTotal'Underscorereceipts'Underscoreperiod :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the reporting period
  , committeeReportsHouseSenateTotal'Underscorereceipts'Underscoreytd :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the year to date
  , committeeReportsHouseSenateTransfers'Underscorefrom'Underscoreother'Underscoreauthorized'Underscorecommittee'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateTransfers'Underscorefrom'Underscoreother'Underscoreauthorized'Underscorecommittee'Underscoreytd :: Double -- ^
  , committeeReportsHouseSenateTransfers'Underscoreto'Underscoreother'Underscoreauthorized'Underscorecommittee'Underscoreperiod :: Double -- ^
  , committeeReportsHouseSenateTransfers'Underscoreto'Underscoreother'Underscoreauthorized'Underscorecommittee'Underscoreytd :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeReportsHouseSenate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeReportsHouseSenate")
instance ToJSON CommitteeReportsHouseSenate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeReportsHouseSenate")

-- |
data CommitteeReportsHouseSenatePage = CommitteeReportsHouseSenatePage
  { committeeReportsHouseSenatePagePagination :: OffsetInfo -- ^
  , committeeReportsHouseSenatePageResults    :: [CommitteeReportsHouseSenate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeReportsHouseSenatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeReportsHouseSenatePage")
instance ToJSON CommitteeReportsHouseSenatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeReportsHouseSenatePage")

-- |
data CommitteeReportsIEOnly = CommitteeReportsIEOnly
  { committeeReportsIEOnlyBeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeReportsIEOnlyCommittee'Underscoreid :: Text -- ^
  , committeeReportsIEOnlyCommittee'Underscorename :: Text -- ^
  , committeeReportsIEOnlyCommittee'Underscoretype :: Text -- ^
  , committeeReportsIEOnlyCoverage'Underscoreend'Underscoredate :: Integer -- ^
  , committeeReportsIEOnlyCoverage'Underscorestart'Underscoredate :: Integer -- ^
  , committeeReportsIEOnlyCsv'Underscoreurl :: Text -- ^
  , committeeReportsIEOnlyCycle :: Int -- ^
  , committeeReportsIEOnlyDocument'Underscoredescription :: Text -- ^
  , committeeReportsIEOnlyEnd'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeReportsIEOnlyFec'Underscorefile'Underscoreid :: Text -- ^
  , committeeReportsIEOnlyFec'Underscoreurl :: Text -- ^
  , committeeReportsIEOnlyIndependent'Underscorecontributions'Underscoreperiod :: Double -- ^
  , committeeReportsIEOnlyIndependent'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsIEOnlyIs'Underscoreamended :: Bool -- ^ False indicates that a report is the most recent. True indicates that the report has been superseded by an amendment.
  , committeeReportsIEOnlyMeans'Underscorefiled :: Text -- ^ The method used to file with the FEC, either electronic or on paper.
  , committeeReportsIEOnlyPdf'Underscoreurl :: Text -- ^
  , committeeReportsIEOnlyReceipt'Underscoredate :: Date -- ^ Date the FEC received the electronic or paper record
  , committeeReportsIEOnlyReport'Underscoreform :: Text -- ^
  , committeeReportsIEOnlyReport'Underscoretype :: Text -- ^
  , committeeReportsIEOnlyReport'Underscoretype'Underscorefull :: Text -- ^
  , committeeReportsIEOnlyReport'Underscoreyear :: Int -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeReportsIEOnly where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeReportsIEOnly")
instance ToJSON CommitteeReportsIEOnly where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeReportsIEOnly")

-- |
data CommitteeReportsIEOnlyPage = CommitteeReportsIEOnlyPage
  { committeeReportsIEOnlyPagePagination :: OffsetInfo -- ^
  , committeeReportsIEOnlyPageResults    :: [CommitteeReportsIEOnly] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeReportsIEOnlyPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeReportsIEOnlyPage")
instance ToJSON CommitteeReportsIEOnlyPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeReportsIEOnlyPage")

-- |
data CommitteeReportsPacParty = CommitteeReportsPacParty
  { committeeReportsPacPartyAll'Underscoreloans'Underscorereceived'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyAll'Underscoreloans'Underscorereceived'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyAllocated'Underscorefederal'Underscoreelection'Underscorelevin'Underscoreshare'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyAmendment'Underscorechain :: [Double] -- ^  The first value in the chain is the original filing.  The ordering in the chain reflects the order the amendments were filed up to the amendment being viewed.
  , committeeReportsPacPartyAmendment'Underscoreindicator :: Text -- ^
  , committeeReportsPacPartyAmendment'Underscoreindicator'Underscorefull :: Text -- ^
  , committeeReportsPacPartyBeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeReportsPacPartyCalendar'Underscoreytd :: Int -- ^
  , committeeReportsPacPartyCash'Underscoreon'Underscorehand'Underscorebeginning'Underscorecalendar'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyCash'Underscoreon'Underscorehand'Underscorebeginning'Underscoreperiod :: Double -- ^ Balance for the committee at the start of the two-year period
  , committeeReportsPacPartyCash'Underscoreon'Underscorehand'Underscoreclose'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyCash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^ Ending cash balance on the most recent filing
  , committeeReportsPacPartyCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeReportsPacPartyCommittee'Underscorename :: Text -- ^
  , committeeReportsPacPartyCommittee'Underscoretype :: Text -- ^
  , committeeReportsPacPartyCoordinated'Underscoreexpenditures'Underscoreby'Underscoreparty'Underscorecommittee'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyCoordinated'Underscoreexpenditures'Underscoreby'Underscoreparty'Underscorecommittee'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyCoverage'Underscoreend'Underscoredate :: Integer -- ^ Ending date of the reporting period
  , committeeReportsPacPartyCoverage'Underscorestart'Underscoredate :: Integer -- ^ Beginning date of the reporting period
  , committeeReportsPacPartyCsv'Underscoreurl :: Text -- ^
  , committeeReportsPacPartyCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeReportsPacPartyDebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^ Debts owed by the committee
  , committeeReportsPacPartyDebts'Underscoreowed'Underscoreto'Underscorecommittee :: Double -- ^ Debts owed to the committee
  , committeeReportsPacPartyDocument'Underscoredescription :: Text -- ^
  , committeeReportsPacPartyEnd'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeReportsPacPartyFec'Underscorefile'Underscoreid :: Text -- ^
  , committeeReportsPacPartyFec'Underscoreurl :: Text -- ^
  , committeeReportsPacPartyFed'Underscorecandidate'Underscorecommittee'Underscorecontribution'Underscorerefunds'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyFed'Underscorecandidate'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyFed'Underscorecandidate'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyFed'Underscorecandidate'Underscorecontribution'Underscorerefunds'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyFile'Underscorenumber :: Int -- ^
  , committeeReportsPacPartyHtml'Underscoreurl :: Text -- ^ HTML link to the filing.
  , committeeReportsPacPartyIndependent'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyIndependent'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyIndividual'Underscoreitemized'Underscorecontributions'Underscoreperiod :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the reporting period
  , committeeReportsPacPartyIndividual'Underscoreitemized'Underscorecontributions'Underscoreytd :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the year to date
  , committeeReportsPacPartyIndividual'Underscoreunitemized'Underscorecontributions'Underscoreperiod :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the reporting period
  , committeeReportsPacPartyIndividual'Underscoreunitemized'Underscorecontributions'Underscoreytd :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the year to date
  , committeeReportsPacPartyIs'Underscoreamended :: Bool -- ^ False indicates that a report is the most recent. True indicates that the report has been superseded by an amendment.
  , committeeReportsPacPartyLoan'Underscorerepayments'Underscoremade'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyLoan'Underscorerepayments'Underscoremade'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyLoan'Underscorerepayments'Underscorereceived'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyLoan'Underscorerepayments'Underscorereceived'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyLoans'Underscoremade'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyLoans'Underscoremade'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyMeans'Underscorefiled :: Text -- ^ The method used to file with the FEC, either electronic or on paper.
  , committeeReportsPacPartyMost'Underscorerecent :: Bool -- ^
  , committeeReportsPacPartyMost'Underscorerecent'Underscorefile'Underscorenumber :: Double -- ^
  , committeeReportsPacPartyNet'Underscorecontributions'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyNet'Underscorecontributions'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyNet'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyNet'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyNon'Underscoreallocated'Underscorefed'Underscoreelection'Underscoreactivity'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyNon'Underscoreallocated'Underscorefed'Underscoreelection'Underscoreactivity'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyNonfed'Underscoreshare'Underscoreallocated'Underscoredisbursements'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^ Offsets to operating expenditures total for the reporting period
  , committeeReportsPacPartyOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^ Offsets to operating expenditures total for the year to date
  , committeeReportsPacPartyOther'Underscoredisbursements'Underscoreperiod :: Double -- ^ Other disbursements total for the reporting period
  , committeeReportsPacPartyOther'Underscoredisbursements'Underscoreytd :: Double -- ^ Other disbursements total for the year to date
  , committeeReportsPacPartyOther'Underscorefed'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyOther'Underscorefed'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyOther'Underscorefed'Underscorereceipts'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyOther'Underscorefed'Underscorereceipts'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyOther'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Other committees contributions total for the reporting period
  , committeeReportsPacPartyOther'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Other committees contributions total for the year to date
  , committeeReportsPacPartyPdf'Underscoreurl :: Text -- ^
  , committeeReportsPacPartyPolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Party committees contributions total for the reporting period
  , committeeReportsPacPartyPolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Party committees contributions total for the year to date
  , committeeReportsPacPartyPrevious'Underscorefile'Underscorenumber :: Double -- ^
  , committeeReportsPacPartyReceipt'Underscoredate :: Date -- ^ Date the FEC received the electronic or paper record
  , committeeReportsPacPartyRefunded'Underscoreindividual'Underscorecontributions'Underscoreperiod :: Double -- ^ Individual refunds total for the reporting period
  , committeeReportsPacPartyRefunded'Underscoreindividual'Underscorecontributions'Underscoreytd :: Double -- ^ Individual refunds total for the year to date
  , committeeReportsPacPartyRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Other committee refunds total for the reporting period
  , committeeReportsPacPartyRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Other committee refunds total for the year to date
  , committeeReportsPacPartyRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Political party refunds total for the reporting period
  , committeeReportsPacPartyRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Political party refunds total for the year to date
  , committeeReportsPacPartyReport'Underscoreform :: Text -- ^
  , committeeReportsPacPartyReport'Underscoretype :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsPacPartyReport'Underscoretype'Underscorefull :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsPacPartyReport'Underscoreyear :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , committeeReportsPacPartyShared'Underscorefed'Underscoreactivity'Underscorenonfed'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyShared'Underscorefed'Underscoreactivity'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyShared'Underscorefed'Underscoreactivity'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyShared'Underscorefed'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyShared'Underscorefed'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyShared'Underscorenonfed'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyShared'Underscorenonfed'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsPacPartySubtotal'Underscoresummary'Underscorepage'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartySubtotal'Underscoresummary'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyTotal'Underscorecontribution'Underscorerefunds'Underscoreperiod :: Double -- ^ Total contribution refunds total for the reporting period
  , committeeReportsPacPartyTotal'Underscorecontribution'Underscorerefunds'Underscoreytd :: Double -- ^ Total contribution refunds total for the year to date
  , committeeReportsPacPartyTotal'Underscorecontributions'Underscoreperiod :: Double -- ^ Contribution total for the reporting period
  , committeeReportsPacPartyTotal'Underscorecontributions'Underscoreytd :: Double -- ^ Contribution total for the year to date
  , committeeReportsPacPartyTotal'Underscoredisbursements'Underscoreperiod :: Double -- ^ Disbursements total for the reporting period
  , committeeReportsPacPartyTotal'Underscoredisbursements'Underscoreytd :: Double -- ^ Disbursements total for the year to date
  , committeeReportsPacPartyTotal'Underscorefed'Underscoredisbursements'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyTotal'Underscorefed'Underscoredisbursements'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyTotal'Underscorefed'Underscoreelection'Underscoreactivity'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyTotal'Underscorefed'Underscoreelection'Underscoreactivity'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyTotal'Underscorefed'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyTotal'Underscorefed'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyTotal'Underscorefed'Underscorereceipts'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyTotal'Underscorefed'Underscorereceipts'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyTotal'Underscoreindividual'Underscorecontributions'Underscoreperiod :: Double -- ^ Individual contributions total for the reporting period
  , committeeReportsPacPartyTotal'Underscoreindividual'Underscorecontributions'Underscoreytd :: Double -- ^ Individual contributions total for the year to date
  , committeeReportsPacPartyTotal'Underscorenonfed'Underscoretransfers'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyTotal'Underscorenonfed'Underscoretransfers'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyTotal'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyTotal'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyTotal'Underscorereceipts'Underscoreperiod :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the reporting period
  , committeeReportsPacPartyTotal'Underscorereceipts'Underscoreytd :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the year to date
  , committeeReportsPacPartyTransfers'Underscorefrom'Underscoreaffiliated'Underscoreparty'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyTransfers'Underscorefrom'Underscoreaffiliated'Underscoreparty'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyTransfers'Underscorefrom'Underscorenonfed'Underscoreaccount'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyTransfers'Underscorefrom'Underscorenonfed'Underscoreaccount'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyTransfers'Underscorefrom'Underscorenonfed'Underscorelevin'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyTransfers'Underscorefrom'Underscorenonfed'Underscorelevin'Underscoreytd :: Double -- ^
  , committeeReportsPacPartyTransfers'Underscoreto'Underscoreaffiliated'Underscorecommittee'Underscoreperiod :: Double -- ^
  , committeeReportsPacPartyTransfers'Underscoreto'Underscoreaffilitated'Underscorecommittees'Underscoreytd :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeReportsPacParty where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeReportsPacParty")
instance ToJSON CommitteeReportsPacParty where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeReportsPacParty")

-- |
data CommitteeReportsPacPartyPage = CommitteeReportsPacPartyPage
  { committeeReportsPacPartyPagePagination :: OffsetInfo -- ^
  , committeeReportsPacPartyPageResults    :: [CommitteeReportsPacParty] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeReportsPacPartyPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeReportsPacPartyPage")
instance ToJSON CommitteeReportsPacPartyPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeReportsPacPartyPage")

-- |
data CommitteeReportsPage = CommitteeReportsPage
  { committeeReportsPagePagination :: OffsetInfo -- ^
  , committeeReportsPageResults    :: [CommitteeReports] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeReportsPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeReportsPage")
instance ToJSON CommitteeReportsPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeReportsPage")

-- |
data CommitteeReportsPresidential = CommitteeReportsPresidential
  { committeeReportsPresidentialAmendment'Underscorechain :: [Double] -- ^  The first value in the chain is the original filing.  The ordering in the chain reflects the order the amendments were filed up to the amendment being viewed.
  , committeeReportsPresidentialAmendment'Underscoreindicator :: Text -- ^
  , committeeReportsPresidentialAmendment'Underscoreindicator'Underscorefull :: Text -- ^
  , committeeReportsPresidentialBeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeReportsPresidentialCandidate'Underscorecontribution'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialCandidate'Underscorecontribution'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialCash'Underscoreon'Underscorehand'Underscorebeginning'Underscoreperiod :: Double -- ^ Balance for the committee at the start of the two-year period
  , committeeReportsPresidentialCash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^ Ending cash balance on the most recent filing
  , committeeReportsPresidentialCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeReportsPresidentialCommittee'Underscorename :: Text -- ^
  , committeeReportsPresidentialCommittee'Underscoretype :: Text -- ^
  , committeeReportsPresidentialCoverage'Underscoreend'Underscoredate :: Integer -- ^ Ending date of the reporting period
  , committeeReportsPresidentialCoverage'Underscorestart'Underscoredate :: Integer -- ^ Beginning date of the reporting period
  , committeeReportsPresidentialCsv'Underscoreurl :: Text -- ^
  , committeeReportsPresidentialCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeReportsPresidentialDebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^ Debts owed by the committee
  , committeeReportsPresidentialDebts'Underscoreowed'Underscoreto'Underscorecommittee :: Double -- ^ Debts owed to the committee
  , committeeReportsPresidentialDocument'Underscoredescription :: Text -- ^
  , committeeReportsPresidentialEnd'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeReportsPresidentialExempt'Underscorelegal'Underscoreaccounting'Underscoredisbursement'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialExempt'Underscorelegal'Underscoreaccounting'Underscoredisbursement'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialExpenditure'Underscoresubject'Underscoreto'Underscorelimits :: Double -- ^
  , committeeReportsPresidentialFec'Underscorefile'Underscoreid :: Text -- ^
  , committeeReportsPresidentialFec'Underscoreurl :: Text -- ^
  , committeeReportsPresidentialFederal'Underscorefunds'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialFederal'Underscorefunds'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialFile'Underscorenumber :: Int -- ^
  , committeeReportsPresidentialFundraising'Underscoredisbursements'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialFundraising'Underscoredisbursements'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialHtml'Underscoreurl :: Text -- ^ HTML link to the filing.
  , committeeReportsPresidentialIndividual'Underscoreitemized'Underscorecontributions'Underscoreperiod :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the reporting period
  , committeeReportsPresidentialIndividual'Underscoreitemized'Underscorecontributions'Underscoreytd :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the year to date
  , committeeReportsPresidentialIndividual'Underscoreunitemized'Underscorecontributions'Underscoreperiod :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the reporting period
  , committeeReportsPresidentialIndividual'Underscoreunitemized'Underscorecontributions'Underscoreytd :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the year to date
  , committeeReportsPresidentialIs'Underscoreamended :: Bool -- ^ False indicates that a report is the most recent. True indicates that the report has been superseded by an amendment.
  , committeeReportsPresidentialItems'Underscoreon'Underscorehand'Underscoreliquidated :: Double -- ^
  , committeeReportsPresidentialLoans'Underscorereceived'Underscorefrom'Underscorecandidate'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialLoans'Underscorereceived'Underscorefrom'Underscorecandidate'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialMeans'Underscorefiled :: Text -- ^ The method used to file with the FEC, either electronic or on paper.
  , committeeReportsPresidentialMost'Underscorerecent :: Bool -- ^
  , committeeReportsPresidentialMost'Underscorerecent'Underscorefile'Underscorenumber :: Double -- ^
  , committeeReportsPresidentialNet'Underscorecontributions'Underscorecycle'Underscoreto'Underscoredate :: Double -- ^
  , committeeReportsPresidentialNet'Underscoreoperating'Underscoreexpenditures'Underscorecycle'Underscoreto'Underscoredate :: Double -- ^
  , committeeReportsPresidentialOffsets'Underscoreto'Underscorefundraising'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialOffsets'Underscoreto'Underscorefundraising'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialOffsets'Underscoreto'Underscorelegal'Underscoreaccounting'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialOffsets'Underscoreto'Underscorelegal'Underscoreaccounting'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^ Offsets to operating expenditures total for the reporting period
  , committeeReportsPresidentialOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^ Offsets to operating expenditures total for the year to date
  , committeeReportsPresidentialOperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialOperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialOther'Underscoredisbursements'Underscoreperiod :: Double -- ^ Other disbursements total for the reporting period
  , committeeReportsPresidentialOther'Underscoredisbursements'Underscoreytd :: Double -- ^ Other disbursements total for the year to date
  , committeeReportsPresidentialOther'Underscoreloans'Underscorereceived'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialOther'Underscoreloans'Underscorereceived'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialOther'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Other committees contributions total for the reporting period
  , committeeReportsPresidentialOther'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Other committees contributions total for the year to date
  , committeeReportsPresidentialOther'Underscorereceipts'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialOther'Underscorereceipts'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialPdf'Underscoreurl :: Text -- ^
  , committeeReportsPresidentialPolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Party committees contributions total for the reporting period
  , committeeReportsPresidentialPolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Party committees contributions total for the year to date
  , committeeReportsPresidentialPrevious'Underscorefile'Underscorenumber :: Double -- ^
  , committeeReportsPresidentialReceipt'Underscoredate :: Date -- ^ Date the FEC received the electronic or paper record
  , committeeReportsPresidentialRefunded'Underscoreindividual'Underscorecontributions'Underscoreperiod :: Double -- ^ Individual refunds total for the reporting period
  , committeeReportsPresidentialRefunded'Underscoreindividual'Underscorecontributions'Underscoreytd :: Double -- ^ Individual refunds total for the year to date
  , committeeReportsPresidentialRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Other committee refunds total for the reporting period
  , committeeReportsPresidentialRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Other committee refunds total for the year to date
  , committeeReportsPresidentialRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreperiod :: Double -- ^ Political party refunds total for the reporting period
  , committeeReportsPresidentialRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions'Underscoreytd :: Double -- ^ Political party refunds total for the year to date
  , committeeReportsPresidentialRepayments'Underscoreloans'Underscoremade'Underscoreby'Underscorecandidate'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialRepayments'Underscoreloans'Underscoremade'Underscorecandidate'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialRepayments'Underscoreother'Underscoreloans'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialRepayments'Underscoreother'Underscoreloans'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialReport'Underscoreform :: Text -- ^
  , committeeReportsPresidentialReport'Underscoretype :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsPresidentialReport'Underscoretype'Underscorefull :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsPresidentialReport'Underscoreyear :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , committeeReportsPresidentialSubtotal'Underscoresummary'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialTotal'Underscorecontribution'Underscorerefunds'Underscoreperiod :: Double -- ^ Total contribution refunds total for the reporting period
  , committeeReportsPresidentialTotal'Underscorecontribution'Underscorerefunds'Underscoreytd :: Double -- ^ Total contribution refunds total for the year to date
  , committeeReportsPresidentialTotal'Underscorecontributions'Underscoreperiod :: Double -- ^ Contribution total for the reporting period
  , committeeReportsPresidentialTotal'Underscorecontributions'Underscoreytd :: Double -- ^ Contribution total for the year to date
  , committeeReportsPresidentialTotal'Underscoredisbursements'Underscoreperiod :: Double -- ^ Disbursements total for the reporting period
  , committeeReportsPresidentialTotal'Underscoredisbursements'Underscoreytd :: Double -- ^ Disbursements total for the year to date
  , committeeReportsPresidentialTotal'Underscoreindividual'Underscorecontributions'Underscoreperiod :: Double -- ^ Individual contributions total for the reporting period
  , committeeReportsPresidentialTotal'Underscoreindividual'Underscorecontributions'Underscoreytd :: Double -- ^ Individual contributions total for the year to date
  , committeeReportsPresidentialTotal'Underscoreloan'Underscorerepayments'Underscoremade'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialTotal'Underscoreloan'Underscorerepayments'Underscoremade'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialTotal'Underscoreloans'Underscorereceived'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialTotal'Underscoreloans'Underscorereceived'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialTotal'Underscoreoffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialTotal'Underscoreoffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialTotal'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialTotal'Underscorereceipts'Underscoreperiod :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the reporting period
  , committeeReportsPresidentialTotal'Underscorereceipts'Underscoreytd :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the year to date
  , committeeReportsPresidentialTotal'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialTransfers'Underscorefrom'Underscoreaffiliated'Underscorecommittee'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialTransfers'Underscorefrom'Underscoreaffiliated'Underscorecommittee'Underscoreytd :: Double -- ^
  , committeeReportsPresidentialTransfers'Underscoreto'Underscoreother'Underscoreauthorized'Underscorecommittee'Underscoreperiod :: Double -- ^
  , committeeReportsPresidentialTransfers'Underscoreto'Underscoreother'Underscoreauthorized'Underscorecommittee'Underscoreytd :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeReportsPresidential where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeReportsPresidential")
instance ToJSON CommitteeReportsPresidential where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeReportsPresidential")

-- |
data CommitteeReportsPresidentialPage = CommitteeReportsPresidentialPage
  { committeeReportsPresidentialPagePagination :: OffsetInfo -- ^
  , committeeReportsPresidentialPageResults    :: [CommitteeReportsPresidential] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeReportsPresidentialPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeReportsPresidentialPage")
instance ToJSON CommitteeReportsPresidentialPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeReportsPresidentialPage")

-- |
data CommitteeSearch = CommitteeSearch
  { committeeSearchId   :: Text -- ^
  , committeeSearchName :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeSearch where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeSearch")
instance ToJSON CommitteeSearch where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeSearch")

-- |
data CommitteeSearchList = CommitteeSearchList
  { committeeSearchListResults :: [CommitteeSearch] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeSearchList where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeSearchList")
instance ToJSON CommitteeSearchList where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeSearchList")

-- |
data CommitteeTotals = CommitteeTotals
  { committeeTotalsAll'Underscoreloans'Underscorereceived :: Double -- ^
  , committeeTotalsAll'Underscoreother'Underscoreloans :: Double -- ^
  , committeeTotalsAllocated'Underscorefederal'Underscoreelection'Underscorelevin'Underscoreshare :: Double -- ^
  , committeeTotalsCandidate'Underscorecontribution :: Double -- ^
  , committeeTotalsCash'Underscoreon'Underscorehand'Underscorebeginning'Underscoreperiod :: Double -- ^
  , committeeTotalsCommittee'Underscoredesignation :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsCommittee'Underscoredesignation'Underscorefull :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeTotalsCommittee'Underscorename :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeTotalsCommittee'Underscoretype :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsCommittee'Underscoretype'Underscorefull :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsContribution'Underscorerefunds :: Double -- ^
  , committeeTotalsContributions :: Double -- ^ Contribution
  , committeeTotalsConvention'Underscoreexp :: Double -- ^
  , committeeTotalsCoordinated'Underscoreexpenditures'Underscoreby'Underscoreparty'Underscorecommittee :: Double -- ^
  , committeeTotalsCoverage'Underscoreend'Underscoredate :: Integer -- ^
  , committeeTotalsCoverage'Underscorestart'Underscoredate :: Integer -- ^
  , committeeTotalsCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeTotalsDisbursements :: Double -- ^ Disbursements
  , committeeTotalsExempt'Underscorelegal'Underscoreaccounting'Underscoredisbursement :: Double -- ^
  , committeeTotalsExp'Underscoreprior'Underscoreyears'Underscoresubject'Underscorelimits :: Double -- ^
  , committeeTotalsExp'Underscoresubject'Underscorelimits :: Double -- ^
  , committeeTotalsFed'Underscorecandidate'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsFed'Underscorecandidate'Underscorecontribution'Underscorerefunds :: Double -- ^
  , committeeTotalsFed'Underscoredisbursements :: Double -- ^
  , committeeTotalsFed'Underscoreelection'Underscoreactivity :: Double -- ^
  , committeeTotalsFed'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsFed'Underscorereceipts :: Double -- ^
  , committeeTotalsFederal'Underscorefunds :: Double -- ^
  , committeeTotalsFundraising'Underscoredisbursements :: Double -- ^
  , committeeTotalsIndependent'Underscoreexpenditures :: Double -- ^
  , committeeTotalsIndividual'Underscorecontributions :: Double -- ^
  , committeeTotalsIndividual'Underscoreitemized'Underscorecontributions :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less.
  , committeeTotalsIndividual'Underscoreunitemized'Underscorecontributions :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total.
  , committeeTotalsItemized'Underscoreconvention'Underscoreexp :: Double -- ^
  , committeeTotalsItemized'Underscoreother'Underscoredisb :: Double -- ^
  , committeeTotalsItemized'Underscoreother'Underscoreincome :: Double -- ^
  , committeeTotalsItemized'Underscoreother'Underscorerefunds :: Double -- ^
  , committeeTotalsItemized'Underscorerefunds'Underscorerelating'Underscoreconvention'Underscoreexp :: Double -- ^
  , committeeTotalsLast'Underscorebeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeTotalsLast'Underscorecash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^
  , committeeTotalsLast'Underscoredebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^
  , committeeTotalsLast'Underscoredebts'Underscoreowed'Underscoreto'Underscorecommittee :: Double -- ^
  , committeeTotalsLast'Underscorereport'Underscoretype'Underscorefull :: Text -- ^
  , committeeTotalsLast'Underscorereport'Underscoreyear :: Int -- ^
  , committeeTotalsLoan'Underscorerepayments :: Double -- ^
  , committeeTotalsLoan'Underscorerepayments'Underscorecandidate'Underscoreloans :: Double -- ^
  , committeeTotalsLoan'Underscorerepayments'Underscoremade :: Double -- ^
  , committeeTotalsLoan'Underscorerepayments'Underscoreother'Underscoreloans :: Double -- ^
  , committeeTotalsLoan'Underscorerepayments'Underscorereceived :: Double -- ^
  , committeeTotalsLoans :: Double -- ^
  , committeeTotalsLoans'Underscoreand'Underscoreloan'Underscorerepayments'Underscoremade :: Double -- ^
  , committeeTotalsLoans'Underscoreand'Underscoreloan'Underscorerepayments'Underscorereceived :: Double -- ^
  , committeeTotalsLoans'Underscoremade :: Double -- ^
  , committeeTotalsLoans'Underscoremade'Underscoreby'Underscorecandidate :: Double -- ^
  , committeeTotalsLoans'Underscorereceived :: Double -- ^
  , committeeTotalsLoans'Underscorereceived'Underscorefrom'Underscorecandidate :: Double -- ^
  , committeeTotalsNet'Underscorecontributions :: Double -- ^
  , committeeTotalsNet'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsNon'Underscoreallocated'Underscorefed'Underscoreelection'Underscoreactivity :: Double -- ^
  , committeeTotalsOffsets'Underscoreto'Underscorefundraising'Underscoreexpenditures :: Double -- ^
  , committeeTotalsOffsets'Underscoreto'Underscorelegal'Underscoreaccounting :: Double -- ^
  , committeeTotalsOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsOperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsOther'Underscoredisbursements :: Double -- ^
  , committeeTotalsOther'Underscorefed'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsOther'Underscorefed'Underscorereceipts :: Double -- ^
  , committeeTotalsOther'Underscoreloans'Underscorereceived :: Double -- ^
  , committeeTotalsOther'Underscorepolitical'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsOther'Underscorereceipts :: Double -- ^
  , committeeTotalsOther'Underscorerefunds :: Double -- ^
  , committeeTotalsParty'Underscorefull :: Text -- ^ Party affiliated with a candidate or committee
  , committeeTotalsPdf'Underscoreurl :: Text -- ^
  , committeeTotalsPolitical'Underscoreparty'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsReceipts :: Double -- ^
  , committeeTotalsRefunded'Underscoreindividual'Underscorecontributions :: Double -- ^
  , committeeTotalsRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsRefunds'Underscorerelating'Underscoreconvention'Underscoreexp :: Double -- ^
  , committeeTotalsRepayments'Underscoreloans'Underscoremade'Underscoreby'Underscorecandidate :: Double -- ^
  , committeeTotalsRepayments'Underscoreother'Underscoreloans :: Double -- ^
  , committeeTotalsReport'Underscoreform :: Text -- ^
  , committeeTotalsShared'Underscorefed'Underscoreactivity :: Double -- ^
  , committeeTotalsShared'Underscorefed'Underscoreactivity'Underscorenonfed :: Double -- ^
  , committeeTotalsShared'Underscorefed'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsShared'Underscorenonfed'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsTotal'Underscoreexp'Underscoresubject'Underscorelimits :: Double -- ^
  , committeeTotalsTotal'Underscoreindependent'Underscorecontributions :: Double -- ^
  , committeeTotalsTotal'Underscoreindependent'Underscoreexpenditures :: Double -- ^
  , committeeTotalsTotal'Underscoreoffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsTotal'Underscoretransfers :: Double -- ^
  , committeeTotalsTransaction'Underscorecoverage'Underscoredate :: Date -- ^
  , committeeTotalsTransfers'Underscorefrom'Underscoreaffiliated'Underscorecommittee :: Double -- ^
  , committeeTotalsTransfers'Underscorefrom'Underscoreaffiliated'Underscoreparty :: Double -- ^
  , committeeTotalsTransfers'Underscorefrom'Underscorenonfed'Underscoreaccount :: Double -- ^
  , committeeTotalsTransfers'Underscorefrom'Underscorenonfed'Underscorelevin :: Double -- ^
  , committeeTotalsTransfers'Underscorefrom'Underscoreother'Underscoreauthorized'Underscorecommittee :: Double -- ^
  , committeeTotalsTransfers'Underscoreto'Underscoreaffiliated'Underscorecommittee :: Double -- ^
  , committeeTotalsTransfers'Underscoreto'Underscoreother'Underscoreauthorized'Underscorecommittee :: Double -- ^
  , committeeTotalsUnitemized'Underscoreconvention'Underscoreexp :: Double -- ^
  , committeeTotalsUnitemized'Underscoreother'Underscoredisb :: Double -- ^
  , committeeTotalsUnitemized'Underscoreother'Underscoreincome :: Double -- ^
  , committeeTotalsUnitemized'Underscoreother'Underscorerefunds :: Double -- ^
  , committeeTotalsUnitemized'Underscorerefunds'Underscorerelating'Underscoreconvention'Underscoreexp :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeTotals where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeTotals")
instance ToJSON CommitteeTotals where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeTotals")

-- |
data CommitteeTotalsHouseSenate = CommitteeTotalsHouseSenate
  { committeeTotalsHouseSenateAll'Underscoreother'Underscoreloans :: Double -- ^
  , committeeTotalsHouseSenateCandidate'Underscorecontribution :: Double -- ^
  , committeeTotalsHouseSenateCash'Underscoreon'Underscorehand'Underscorebeginning'Underscoreperiod :: Double -- ^
  , committeeTotalsHouseSenateCommittee'Underscoredesignation :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsHouseSenateCommittee'Underscoredesignation'Underscorefull :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsHouseSenateCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeTotalsHouseSenateCommittee'Underscorename :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeTotalsHouseSenateCommittee'Underscoretype :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsHouseSenateCommittee'Underscoretype'Underscorefull :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsHouseSenateContribution'Underscorerefunds :: Double -- ^
  , committeeTotalsHouseSenateContributions :: Double -- ^ Contribution
  , committeeTotalsHouseSenateCoverage'Underscoreend'Underscoredate :: Integer -- ^
  , committeeTotalsHouseSenateCoverage'Underscorestart'Underscoredate :: Integer -- ^
  , committeeTotalsHouseSenateCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeTotalsHouseSenateDisbursements :: Double -- ^ Disbursements
  , committeeTotalsHouseSenateIndividual'Underscorecontributions :: Double -- ^
  , committeeTotalsHouseSenateIndividual'Underscoreitemized'Underscorecontributions :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less.
  , committeeTotalsHouseSenateIndividual'Underscoreunitemized'Underscorecontributions :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total.
  , committeeTotalsHouseSenateLast'Underscorebeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeTotalsHouseSenateLast'Underscorecash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^
  , committeeTotalsHouseSenateLast'Underscoredebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^
  , committeeTotalsHouseSenateLast'Underscoredebts'Underscoreowed'Underscoreto'Underscorecommittee :: Double -- ^
  , committeeTotalsHouseSenateLast'Underscorereport'Underscoretype'Underscorefull :: Text -- ^
  , committeeTotalsHouseSenateLast'Underscorereport'Underscoreyear :: Int -- ^
  , committeeTotalsHouseSenateLoan'Underscorerepayments :: Double -- ^
  , committeeTotalsHouseSenateLoan'Underscorerepayments'Underscorecandidate'Underscoreloans :: Double -- ^
  , committeeTotalsHouseSenateLoan'Underscorerepayments'Underscoreother'Underscoreloans :: Double -- ^
  , committeeTotalsHouseSenateLoans :: Double -- ^
  , committeeTotalsHouseSenateLoans'Underscoremade'Underscoreby'Underscorecandidate :: Double -- ^
  , committeeTotalsHouseSenateNet'Underscorecontributions :: Double -- ^
  , committeeTotalsHouseSenateNet'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsHouseSenateOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsHouseSenateOperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsHouseSenateOther'Underscoredisbursements :: Double -- ^
  , committeeTotalsHouseSenateOther'Underscorepolitical'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsHouseSenateOther'Underscorereceipts :: Double -- ^
  , committeeTotalsHouseSenateParty'Underscorefull :: Text -- ^ Party affiliated with a candidate or committee
  , committeeTotalsHouseSenatePdf'Underscoreurl :: Text -- ^
  , committeeTotalsHouseSenatePolitical'Underscoreparty'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsHouseSenateReceipts :: Double -- ^
  , committeeTotalsHouseSenateRefunded'Underscoreindividual'Underscorecontributions :: Double -- ^
  , committeeTotalsHouseSenateRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsHouseSenateRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsHouseSenateReport'Underscoreform :: Text -- ^
  , committeeTotalsHouseSenateTransaction'Underscorecoverage'Underscoredate :: Date -- ^
  , committeeTotalsHouseSenateTransfers'Underscorefrom'Underscoreother'Underscoreauthorized'Underscorecommittee :: Double -- ^
  , committeeTotalsHouseSenateTransfers'Underscoreto'Underscoreother'Underscoreauthorized'Underscorecommittee :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeTotalsHouseSenate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeTotalsHouseSenate")
instance ToJSON CommitteeTotalsHouseSenate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeTotalsHouseSenate")

-- |
data CommitteeTotalsHouseSenatePage = CommitteeTotalsHouseSenatePage
  { committeeTotalsHouseSenatePagePagination :: OffsetInfo -- ^
  , committeeTotalsHouseSenatePageResults    :: [CommitteeTotalsHouseSenate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeTotalsHouseSenatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeTotalsHouseSenatePage")
instance ToJSON CommitteeTotalsHouseSenatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeTotalsHouseSenatePage")

-- |
data CommitteeTotalsIEOnly = CommitteeTotalsIEOnly
  { committeeTotalsIEOnlyCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeTotalsIEOnlyCoverage'Underscoreend'Underscoredate :: Integer -- ^ Ending date of the reporting period
  , committeeTotalsIEOnlyCoverage'Underscorestart'Underscoredate :: Integer -- ^ Beginning date of the reporting period
  , committeeTotalsIEOnlyCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeTotalsIEOnlyLast'Underscorebeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeTotalsIEOnlyLast'Underscorecash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^
  , committeeTotalsIEOnlyPdf'Underscoreurl :: Text -- ^
  , committeeTotalsIEOnlyReport'Underscoreform :: Text -- ^
  , committeeTotalsIEOnlyTotal'Underscoreindependent'Underscorecontributions :: Double -- ^
  , committeeTotalsIEOnlyTotal'Underscoreindependent'Underscoreexpenditures :: Double -- ^
  , committeeTotalsIEOnlyTransaction'Underscorecoverage'Underscoredate :: Date -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeTotalsIEOnly where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeTotalsIEOnly")
instance ToJSON CommitteeTotalsIEOnly where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeTotalsIEOnly")

-- |
data CommitteeTotalsIEOnlyPage = CommitteeTotalsIEOnlyPage
  { committeeTotalsIEOnlyPagePagination :: OffsetInfo -- ^
  , committeeTotalsIEOnlyPageResults    :: [CommitteeTotalsIEOnly] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeTotalsIEOnlyPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeTotalsIEOnlyPage")
instance ToJSON CommitteeTotalsIEOnlyPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeTotalsIEOnlyPage")

-- |
data CommitteeTotalsPacParty = CommitteeTotalsPacParty
  { committeeTotalsPacPartyAll'Underscoreloans'Underscorereceived :: Double -- ^
  , committeeTotalsPacPartyAllocated'Underscorefederal'Underscoreelection'Underscorelevin'Underscoreshare :: Double -- ^
  , committeeTotalsPacPartyCash'Underscoreon'Underscorehand'Underscorebeginning'Underscoreperiod :: Double -- ^
  , committeeTotalsPacPartyCommittee'Underscoredesignation :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsPacPartyCommittee'Underscoredesignation'Underscorefull :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsPacPartyCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeTotalsPacPartyCommittee'Underscorename :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeTotalsPacPartyCommittee'Underscoretype :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsPacPartyCommittee'Underscoretype'Underscorefull :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsPacPartyContribution'Underscorerefunds :: Double -- ^
  , committeeTotalsPacPartyContributions :: Double -- ^ Contribution
  , committeeTotalsPacPartyConvention'Underscoreexp :: Double -- ^
  , committeeTotalsPacPartyCoordinated'Underscoreexpenditures'Underscoreby'Underscoreparty'Underscorecommittee :: Double -- ^
  , committeeTotalsPacPartyCoverage'Underscoreend'Underscoredate :: Integer -- ^
  , committeeTotalsPacPartyCoverage'Underscorestart'Underscoredate :: Integer -- ^
  , committeeTotalsPacPartyCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeTotalsPacPartyDisbursements :: Double -- ^ Disbursements
  , committeeTotalsPacPartyExp'Underscoreprior'Underscoreyears'Underscoresubject'Underscorelimits :: Double -- ^
  , committeeTotalsPacPartyExp'Underscoresubject'Underscorelimits :: Double -- ^
  , committeeTotalsPacPartyFed'Underscorecandidate'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsPacPartyFed'Underscorecandidate'Underscorecontribution'Underscorerefunds :: Double -- ^
  , committeeTotalsPacPartyFed'Underscoredisbursements :: Double -- ^
  , committeeTotalsPacPartyFed'Underscoreelection'Underscoreactivity :: Double -- ^
  , committeeTotalsPacPartyFed'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPacPartyFed'Underscorereceipts :: Double -- ^
  , committeeTotalsPacPartyFederal'Underscorefunds :: Double -- ^
  , committeeTotalsPacPartyIndependent'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPacPartyIndividual'Underscorecontributions :: Double -- ^
  , committeeTotalsPacPartyIndividual'Underscoreitemized'Underscorecontributions :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less.
  , committeeTotalsPacPartyIndividual'Underscoreunitemized'Underscorecontributions :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total.
  , committeeTotalsPacPartyItemized'Underscoreconvention'Underscoreexp :: Double -- ^
  , committeeTotalsPacPartyItemized'Underscoreother'Underscoredisb :: Double -- ^
  , committeeTotalsPacPartyItemized'Underscoreother'Underscoreincome :: Double -- ^
  , committeeTotalsPacPartyItemized'Underscoreother'Underscorerefunds :: Double -- ^
  , committeeTotalsPacPartyItemized'Underscorerefunds'Underscorerelating'Underscoreconvention'Underscoreexp :: Double -- ^
  , committeeTotalsPacPartyLast'Underscorebeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeTotalsPacPartyLast'Underscorecash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^
  , committeeTotalsPacPartyLast'Underscoredebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^
  , committeeTotalsPacPartyLast'Underscoredebts'Underscoreowed'Underscoreto'Underscorecommittee :: Double -- ^
  , committeeTotalsPacPartyLast'Underscorereport'Underscoretype'Underscorefull :: Text -- ^
  , committeeTotalsPacPartyLast'Underscorereport'Underscoreyear :: Int -- ^
  , committeeTotalsPacPartyLoan'Underscorerepayments'Underscoremade :: Double -- ^
  , committeeTotalsPacPartyLoan'Underscorerepayments'Underscorereceived :: Double -- ^
  , committeeTotalsPacPartyLoans'Underscoreand'Underscoreloan'Underscorerepayments'Underscoremade :: Double -- ^
  , committeeTotalsPacPartyLoans'Underscoreand'Underscoreloan'Underscorerepayments'Underscorereceived :: Double -- ^
  , committeeTotalsPacPartyLoans'Underscoremade :: Double -- ^
  , committeeTotalsPacPartyNet'Underscorecontributions :: Double -- ^
  , committeeTotalsPacPartyNet'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPacPartyNon'Underscoreallocated'Underscorefed'Underscoreelection'Underscoreactivity :: Double -- ^
  , committeeTotalsPacPartyOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPacPartyOperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPacPartyOther'Underscoredisbursements :: Double -- ^
  , committeeTotalsPacPartyOther'Underscorefed'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPacPartyOther'Underscorefed'Underscorereceipts :: Double -- ^
  , committeeTotalsPacPartyOther'Underscorepolitical'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsPacPartyOther'Underscorerefunds :: Double -- ^
  , committeeTotalsPacPartyParty'Underscorefull :: Text -- ^ Party affiliated with a candidate or committee
  , committeeTotalsPacPartyPdf'Underscoreurl :: Text -- ^
  , committeeTotalsPacPartyPolitical'Underscoreparty'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsPacPartyReceipts :: Double -- ^
  , committeeTotalsPacPartyRefunded'Underscoreindividual'Underscorecontributions :: Double -- ^
  , committeeTotalsPacPartyRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsPacPartyRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsPacPartyRefunds'Underscorerelating'Underscoreconvention'Underscoreexp :: Double -- ^
  , committeeTotalsPacPartyReport'Underscoreform :: Text -- ^
  , committeeTotalsPacPartyShared'Underscorefed'Underscoreactivity :: Double -- ^
  , committeeTotalsPacPartyShared'Underscorefed'Underscoreactivity'Underscorenonfed :: Double -- ^
  , committeeTotalsPacPartyShared'Underscorefed'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPacPartyShared'Underscorenonfed'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPacPartyTotal'Underscoreexp'Underscoresubject'Underscorelimits :: Double -- ^
  , committeeTotalsPacPartyTotal'Underscoretransfers :: Double -- ^
  , committeeTotalsPacPartyTransaction'Underscorecoverage'Underscoredate :: Date -- ^
  , committeeTotalsPacPartyTransfers'Underscorefrom'Underscoreaffiliated'Underscoreparty :: Double -- ^
  , committeeTotalsPacPartyTransfers'Underscorefrom'Underscorenonfed'Underscoreaccount :: Double -- ^
  , committeeTotalsPacPartyTransfers'Underscorefrom'Underscorenonfed'Underscorelevin :: Double -- ^
  , committeeTotalsPacPartyTransfers'Underscoreto'Underscoreaffiliated'Underscorecommittee :: Double -- ^
  , committeeTotalsPacPartyUnitemized'Underscoreconvention'Underscoreexp :: Double -- ^
  , committeeTotalsPacPartyUnitemized'Underscoreother'Underscoredisb :: Double -- ^
  , committeeTotalsPacPartyUnitemized'Underscoreother'Underscoreincome :: Double -- ^
  , committeeTotalsPacPartyUnitemized'Underscoreother'Underscorerefunds :: Double -- ^
  , committeeTotalsPacPartyUnitemized'Underscorerefunds'Underscorerelating'Underscoreconvention'Underscoreexp :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeTotalsPacParty where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeTotalsPacParty")
instance ToJSON CommitteeTotalsPacParty where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeTotalsPacParty")

-- |
data CommitteeTotalsPacPartyPage = CommitteeTotalsPacPartyPage
  { committeeTotalsPacPartyPagePagination :: OffsetInfo -- ^
  , committeeTotalsPacPartyPageResults    :: [CommitteeTotalsPacParty] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeTotalsPacPartyPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeTotalsPacPartyPage")
instance ToJSON CommitteeTotalsPacPartyPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeTotalsPacPartyPage")

-- |
data CommitteeTotalsPage = CommitteeTotalsPage
  { committeeTotalsPagePagination :: OffsetInfo -- ^
  , committeeTotalsPageResults    :: [CommitteeTotals] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeTotalsPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeTotalsPage")
instance ToJSON CommitteeTotalsPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeTotalsPage")

-- |
data CommitteeTotalsPresidential = CommitteeTotalsPresidential
  { committeeTotalsPresidentialCandidate'Underscorecontribution :: Double -- ^
  , committeeTotalsPresidentialCash'Underscoreon'Underscorehand'Underscorebeginning'Underscoreperiod :: Double -- ^
  , committeeTotalsPresidentialCommittee'Underscoredesignation :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsPresidentialCommittee'Underscoredesignation'Underscorefull :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsPresidentialCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeTotalsPresidentialCommittee'Underscorename :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeTotalsPresidentialCommittee'Underscoretype :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsPresidentialCommittee'Underscoretype'Underscorefull :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsPresidentialContribution'Underscorerefunds :: Double -- ^
  , committeeTotalsPresidentialContributions :: Double -- ^ Contribution
  , committeeTotalsPresidentialCoverage'Underscoreend'Underscoredate :: Integer -- ^
  , committeeTotalsPresidentialCoverage'Underscorestart'Underscoredate :: Integer -- ^
  , committeeTotalsPresidentialCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeTotalsPresidentialDisbursements :: Double -- ^ Disbursements
  , committeeTotalsPresidentialExempt'Underscorelegal'Underscoreaccounting'Underscoredisbursement :: Double -- ^
  , committeeTotalsPresidentialFederal'Underscorefunds :: Double -- ^
  , committeeTotalsPresidentialFundraising'Underscoredisbursements :: Double -- ^
  , committeeTotalsPresidentialIndividual'Underscorecontributions :: Double -- ^
  , committeeTotalsPresidentialIndividual'Underscoreitemized'Underscorecontributions :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less.
  , committeeTotalsPresidentialIndividual'Underscoreunitemized'Underscorecontributions :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total.
  , committeeTotalsPresidentialLast'Underscorebeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , committeeTotalsPresidentialLast'Underscorecash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^
  , committeeTotalsPresidentialLast'Underscoredebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^
  , committeeTotalsPresidentialLast'Underscoredebts'Underscoreowed'Underscoreto'Underscorecommittee :: Double -- ^
  , committeeTotalsPresidentialLast'Underscorereport'Underscoretype'Underscorefull :: Text -- ^
  , committeeTotalsPresidentialLast'Underscorereport'Underscoreyear :: Int -- ^
  , committeeTotalsPresidentialLoan'Underscorerepayments'Underscoremade :: Double -- ^
  , committeeTotalsPresidentialLoans'Underscorereceived :: Double -- ^
  , committeeTotalsPresidentialLoans'Underscorereceived'Underscorefrom'Underscorecandidate :: Double -- ^
  , committeeTotalsPresidentialNet'Underscorecontributions :: Double -- ^
  , committeeTotalsPresidentialNet'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPresidentialOffsets'Underscoreto'Underscorefundraising'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPresidentialOffsets'Underscoreto'Underscorelegal'Underscoreaccounting :: Double -- ^
  , committeeTotalsPresidentialOffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPresidentialOperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPresidentialOther'Underscoredisbursements :: Double -- ^
  , committeeTotalsPresidentialOther'Underscoreloans'Underscorereceived :: Double -- ^
  , committeeTotalsPresidentialOther'Underscorepolitical'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsPresidentialOther'Underscorereceipts :: Double -- ^
  , committeeTotalsPresidentialParty'Underscorefull :: Text -- ^ Party affiliated with a candidate or committee
  , committeeTotalsPresidentialPdf'Underscoreurl :: Text -- ^
  , committeeTotalsPresidentialPolitical'Underscoreparty'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsPresidentialReceipts :: Double -- ^
  , committeeTotalsPresidentialRefunded'Underscoreindividual'Underscorecontributions :: Double -- ^
  , committeeTotalsPresidentialRefunded'Underscoreother'Underscorepolitical'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsPresidentialRefunded'Underscorepolitical'Underscoreparty'Underscorecommittee'Underscorecontributions :: Double -- ^
  , committeeTotalsPresidentialRepayments'Underscoreloans'Underscoremade'Underscoreby'Underscorecandidate :: Double -- ^
  , committeeTotalsPresidentialRepayments'Underscoreother'Underscoreloans :: Double -- ^
  , committeeTotalsPresidentialReport'Underscoreform :: Text -- ^
  , committeeTotalsPresidentialTotal'Underscoreoffsets'Underscoreto'Underscoreoperating'Underscoreexpenditures :: Double -- ^
  , committeeTotalsPresidentialTransaction'Underscorecoverage'Underscoredate :: Date -- ^
  , committeeTotalsPresidentialTransfers'Underscorefrom'Underscoreaffiliated'Underscorecommittee :: Double -- ^
  , committeeTotalsPresidentialTransfers'Underscoreto'Underscoreother'Underscoreauthorized'Underscorecommittee :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeTotalsPresidential where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeTotalsPresidential")
instance ToJSON CommitteeTotalsPresidential where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeTotalsPresidential")

-- |
data CommitteeTotalsPresidentialPage = CommitteeTotalsPresidentialPage
  { committeeTotalsPresidentialPagePagination :: OffsetInfo -- ^
  , committeeTotalsPresidentialPageResults    :: [CommitteeTotalsPresidential] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeTotalsPresidentialPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeTotalsPresidentialPage")
instance ToJSON CommitteeTotalsPresidentialPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeTotalsPresidentialPage")

-- |
data CommunicationCost = CommunicationCost
  { communicationCostAction'Underscorecode :: Text -- ^
  , communicationCostAction'Underscorecode'Underscorefull :: Text -- ^
  , communicationCostCandidate'Underscorefirst'Underscorename :: Text -- ^
  , communicationCostCandidate'Underscoreid :: Text -- ^
  , communicationCostCandidate'Underscorelast'Underscorename :: Text -- ^
  , communicationCostCandidate'Underscoremiddle'Underscorename :: Text -- ^
  , communicationCostCandidate'Underscorename :: Text -- ^
  , communicationCostCandidate'Underscoreoffice :: Text -- ^
  , communicationCostCandidate'Underscoreoffice'Underscoredistrict :: Text -- ^
  , communicationCostCandidate'Underscoreoffice'Underscorefull :: Text -- ^
  , communicationCostCandidate'Underscoreoffice'Underscorestate :: Text -- ^
  , communicationCostCommittee'Underscoreid :: Text -- ^
  , communicationCostCommittee'Underscorename :: Text -- ^
  , communicationCostCommunication'Underscoreclass :: Text -- ^
  , communicationCostCommunication'Underscoretype :: Text -- ^
  , communicationCostCommunication'Underscoretype'Underscorefull :: Text -- ^
  , communicationCostCycle :: Int -- ^
  , communicationCostFile'Underscorenumber :: Int -- ^
  , communicationCostForm'Underscoretype'Underscorecode :: Text -- ^
  , communicationCostImage'Underscorenumber :: Text -- ^
  , communicationCostOriginal'Underscoresub'Underscoreid :: Int -- ^
  , communicationCostPdf'Underscoreurl :: Text -- ^
  , communicationCostPrimary'Underscoregeneral'Underscoreindicator :: Text -- ^
  , communicationCostPrimary'Underscoregeneral'Underscoreindicator'Underscoredescription :: Text -- ^
  , communicationCostPurpose :: Text -- ^
  , communicationCostReport'Underscoretype :: Text -- ^
  , communicationCostReport'Underscoreyear :: Int -- ^
  , communicationCostSchedule'Underscoretype :: Text -- ^
  , communicationCostSchedule'Underscoretype'Underscorefull :: Text -- ^
  , communicationCostState'Underscorefull :: Text -- ^
  , communicationCostSub'Underscoreid :: Int -- ^
  , communicationCostSupport'Underscoreoppose'Underscoreindicator :: Text -- ^
  , communicationCostTran'Underscoreid :: Text -- ^
  , communicationCostTransaction'Underscoreamount :: Double -- ^
  , communicationCostTransaction'Underscoredate :: Date -- ^
  , communicationCostTransaction'Underscoretype :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommunicationCost where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "communicationCost")
instance ToJSON CommunicationCost where
  toJSON = genericToJSON (removeFieldLabelPrefix False "communicationCost")

-- |
data CommunicationCostByCandidate = CommunicationCostByCandidate
  { communicationCostByCandidateCandidate'Underscoreid :: Text -- ^
  , communicationCostByCandidateCandidate'Underscorename :: Text -- ^
  , communicationCostByCandidateCommittee'Underscoreid :: Text -- ^
  , communicationCostByCandidateCommittee'Underscorename :: Text -- ^
  , communicationCostByCandidateCount :: Int -- ^ Number of records making up the total
  , communicationCostByCandidateCycle :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , communicationCostByCandidateSupport'Underscoreoppose'Underscoreindicator :: Text -- ^ Explains if the money was spent in order to support or oppose a candidate or candidates. (Coded S or O for support or oppose.) This indicator applies to independent expenditures and communication costs.
  , communicationCostByCandidateTotal :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommunicationCostByCandidate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "communicationCostByCandidate")
instance ToJSON CommunicationCostByCandidate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "communicationCostByCandidate")

-- |
data CommunicationCostByCandidatePage = CommunicationCostByCandidatePage
  { communicationCostByCandidatePagePagination :: OffsetInfo -- ^
  , communicationCostByCandidatePageResults    :: [CommunicationCostByCandidate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommunicationCostByCandidatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "communicationCostByCandidatePage")
instance ToJSON CommunicationCostByCandidatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "communicationCostByCandidatePage")

-- |
data CommunicationCostPage = CommunicationCostPage
  { communicationCostPagePagination :: SeekInfo -- ^
  , communicationCostPageResults    :: [CommunicationCost] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommunicationCostPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "communicationCostPage")
instance ToJSON CommunicationCostPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "communicationCostPage")

-- |
data EFilings = EFilings
  { eFilingsAmended'Underscoreby                       :: Int -- ^
  , eFilingsAmendment'Underscorechain                  :: [Int] -- ^
  , eFilingsAmendment'Underscorenumber                 :: Int -- ^  Number of times the report has been amended.
  , eFilingsAmends'Underscorefile                      :: Int -- ^  For amendments, this file_number is the file_number of the previous report that is being amended. See amended_by for the most recent version of the report.
  , eFilingsBeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , eFilingsCommittee'Underscoreid                     :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , eFilingsCommittee'Underscorename                   :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , eFilingsCoverage'Underscoreend'Underscoredate      :: Date -- ^ Ending date of the reporting period
  , eFilingsCoverage'Underscorestart'Underscoredate    :: Date -- ^ Beginning date of the reporting period
  , eFilingsCsv'Underscoreurl                          :: Text -- ^
  , eFilingsDocument'Underscoredescription             :: Text -- ^
  , eFilingsEnding'Underscoreimage'Underscorenumber    :: Text -- ^
  , eFilingsFec'Underscorefile'Underscoreid            :: Text -- ^
  , eFilingsFec'Underscoreurl                          :: Text -- ^
  , eFilingsFile'Underscorenumber                      :: Int -- ^ Filing ID number
  , eFilingsForm'Underscoretype                        :: Text -- ^  Indicates the type of form that was filed. ex: F1, F2, F3P, F3X etc...
  , eFilingsHtml'Underscoreurl                         :: Text -- ^
  , eFilingsIs'Underscoreamended                       :: Bool -- ^
  , eFilingsLoad'Underscoretimestamp                   :: Integer -- ^ Date the information was loaded into the FEC systems. This can be affected by reseting systems and other factors, refer to receipt_date for the day that the FEC received the paper or electronic document. Keep in mind that paper filings take more time to process and there can be a lag between load_date and receipt_date. This field can be helpful to identify paper records that have been processed recently.
  , eFilingsMost'Underscorerecent                      :: Bool -- ^
  , eFilingsMost'Underscorerecent'Underscorefiling     :: Int -- ^
  , eFilingsPdf'Underscoreurl                          :: Text -- ^
  , eFilingsReceipt'Underscoredate                     :: Date -- ^ Date the FEC received the electronic or paper record
  } deriving (Show, Eq, Generic)

instance FromJSON EFilings where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "eFilings")
instance ToJSON EFilings where
  toJSON = genericToJSON (removeFieldLabelPrefix False "eFilings")

-- |
data EFilingsPage = EFilingsPage
  { eFilingsPagePagination :: OffsetInfo -- ^
  , eFilingsPageResults    :: [EFilings] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON EFilingsPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "eFilingsPage")
instance ToJSON EFilingsPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "eFilingsPage")

-- |
data EfilingsAmendments = EfilingsAmendments
  { efilingsAmendmentsAmendment'Underscorechain                :: [Double] -- ^
  , efilingsAmendmentsDepth                                    :: Double -- ^
  , efilingsAmendmentsFile'Underscorenumber                    :: Int -- ^ Filing ID number
  , efilingsAmendmentsLast                                     :: Double -- ^
  , efilingsAmendmentsLongest'Underscorechain                  :: [Double] -- ^
  , efilingsAmendmentsMost'Underscorerecent'Underscorefiling   :: Double -- ^
  , efilingsAmendmentsPrevious'Underscorefile'Underscorenumber :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON EfilingsAmendments where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "efilingsAmendments")
instance ToJSON EfilingsAmendments where
  toJSON = genericToJSON (removeFieldLabelPrefix False "efilingsAmendments")

-- |
data EfilingsAmendmentsPage = EfilingsAmendmentsPage
  { efilingsAmendmentsPagePagination :: OffsetInfo -- ^
  , efilingsAmendmentsPageResults    :: [EfilingsAmendments] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON EfilingsAmendmentsPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "efilingsAmendmentsPage")
instance ToJSON EfilingsAmendmentsPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "efilingsAmendmentsPage")

-- |
data Election = Election
  { electionCandidate'Underscoreelection'Underscoreyear :: Int -- ^
  , electionCandidate'Underscoreid :: Text -- ^
  , electionCandidate'Underscorename :: Text -- ^
  , electionCandidate'Underscorepcc'Underscoreid :: Text -- ^
  , electionCandidate'Underscorepcc'Underscorename :: Text -- ^
  , electionCash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^
  , electionCommittee'Underscoreids :: [Text] -- ^
  , electionCoverage'Underscoreend'Underscoredate :: Date -- ^
  , electionIncumbent'Underscorechallenge'Underscorefull :: Text -- ^
  , electionParty'Underscorefull :: Text -- ^
  , electionTotal'Underscoredisbursements :: Double -- ^
  , electionTotal'Underscorereceipts :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Election where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "election")
instance ToJSON Election where
  toJSON = genericToJSON (removeFieldLabelPrefix False "election")

-- |
data ElectionDate = ElectionDate
  { electionDateActive'Underscoreelection                :: Bool -- ^
  , electionDateCreate'Underscoredate                    :: Integer -- ^ Date the record was created
  , electionDateElection'Underscoredate                  :: Date -- ^ Date of election
  , electionDateElection'Underscoredistrict              :: Int -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , electionDateElection'Underscorenotes                 :: Text -- ^
  , electionDateElection'Underscoreparty                 :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , electionDateElection'Underscorestate                 :: Text -- ^ US state or territory where a candidate runs for office
  , electionDateElection'Underscoretype'Underscorefull   :: Text -- ^
  , electionDateElection'Underscoretype'Underscoreid     :: Text -- ^ Election type  Convention, Primary, General, Special, Runoff etc.
  , electionDateElection'Underscoreyear                  :: Int -- ^ Year of election
  , electionDateOffice'Underscoresought                  :: Text -- ^ Federal office candidate runs for: H, S or P
  , electionDatePrimary'Underscoregeneral'Underscoredate :: Date -- ^
  , electionDateUpdate'Underscoredate                    :: Integer -- ^ Date the record was updated
  } deriving (Show, Eq, Generic)

instance FromJSON ElectionDate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electionDate")
instance ToJSON ElectionDate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electionDate")

-- |
data ElectionDatePage = ElectionDatePage
  { electionDatePagePagination :: OffsetInfo -- ^
  , electionDatePageResults    :: [ElectionDate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ElectionDatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electionDatePage")
instance ToJSON ElectionDatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electionDatePage")

-- |
data ElectionPage = ElectionPage
  { electionPagePagination :: OffsetInfo -- ^
  , electionPageResults    :: [Election] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ElectionPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electionPage")
instance ToJSON ElectionPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electionPage")

-- |
data ElectionSearch = ElectionSearch
  { electionSearchCandidate'Underscorestatus :: Text -- ^
  , electionSearchCycle                      :: Int -- ^
  , electionSearchDistrict                   :: Text -- ^
  , electionSearchIncumbent'Underscoreid     :: Text -- ^
  , electionSearchIncumbent'Underscorename   :: Text -- ^
  , electionSearchOffice                     :: Text -- ^
  , electionSearchState                      :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ElectionSearch where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electionSearch")
instance ToJSON ElectionSearch where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electionSearch")

-- |
data ElectionSearchPage = ElectionSearchPage
  { electionSearchPagePagination :: OffsetInfo -- ^
  , electionSearchPageResults    :: [ElectionSearch] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ElectionSearchPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electionSearchPage")
instance ToJSON ElectionSearchPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electionSearchPage")

-- |
data ElectionSummary = ElectionSummary
  { electionSummaryCount                              :: Int -- ^
  , electionSummaryDisbursements                      :: Double -- ^
  , electionSummaryIndependent'Underscoreexpenditures :: Double -- ^
  , electionSummaryReceipts                           :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ElectionSummary where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electionSummary")
instance ToJSON ElectionSummary where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electionSummary")

-- |
data Electioneering = Electioneering
  { electioneeringAmendment'Underscoreindicator                  :: Text -- ^
  , electioneeringBeginning'Underscoreimage'Underscorenumber     :: Text -- ^
  , electioneeringCalculated'Underscorecandidate'Underscoreshare :: Double -- ^ If an electioneering cost targets several candidates, the total cost is divided by the number of candidates. If it only mentions one candidate the full cost of the communication is listed.
  , electioneeringCandidate'Underscoredistrict                   :: Text -- ^
  , electioneeringCandidate'Underscoreid                         :: Text -- ^
  , electioneeringCandidate'Underscorename                       :: Text -- ^
  , electioneeringCandidate'Underscoreoffice                     :: Text -- ^
  , electioneeringCandidate'Underscorestate                      :: Text -- ^
  , electioneeringCommittee'Underscoreid                         :: Text -- ^
  , electioneeringCommittee'Underscorename                       :: Text -- ^
  , electioneeringCommunication'Underscoredate                   :: Date -- ^ It is the airing, broadcast, cablecast or other dissemination of the communication
  , electioneeringDisbursement'Underscoreamount                  :: Double -- ^
  , electioneeringDisbursement'Underscoredate                    :: Date -- ^ Disbursement date includes actual disbursements and execution of contracts creating an obligation to make disbursements (SB date of disbursement)
  , electioneeringElection'Underscoretype                        :: Text -- ^
  , electioneeringFile'Underscorenumber                          :: Int -- ^
  , electioneeringLink'Underscoreid                              :: Int -- ^
  , electioneeringNumber'Underscoreof'Underscorecandidates       :: Double -- ^
  , electioneeringPdf'Underscoreurl                              :: Text -- ^
  , electioneeringPublic'Underscoredistribution'Underscoredate   :: Date -- ^ The pubic distribution date is the date that triggers disclosure of the electioneering communication (date reported on page 1 of Form 9)
  , electioneeringPurpose'Underscoredescription                  :: Text -- ^
  , electioneeringReceipt'Underscoredate                         :: Date -- ^
  , electioneeringReport'Underscoreyear                          :: Int -- ^
  , electioneeringSb'Underscoreimage'Underscorenum               :: Text -- ^
  , electioneeringSb'Underscorelink'Underscoreid                 :: Text -- ^
  , electioneeringSub'Underscoreid                               :: Int -- ^ The identifier for each electioneering record
  } deriving (Show, Eq, Generic)

instance FromJSON Electioneering where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electioneering")
instance ToJSON Electioneering where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electioneering")

-- |
data ElectioneeringByCandidate = ElectioneeringByCandidate
  { electioneeringByCandidateCandidate'Underscoreid   :: Text -- ^
  , electioneeringByCandidateCandidate'Underscorename :: Text -- ^
  , electioneeringByCandidateCommittee'Underscoreid   :: Text -- ^
  , electioneeringByCandidateCommittee'Underscorename :: Text -- ^
  , electioneeringByCandidateCount                    :: Int -- ^ Number of records making up the total
  , electioneeringByCandidateCycle                    :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , electioneeringByCandidateTotal                    :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ElectioneeringByCandidate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electioneeringByCandidate")
instance ToJSON ElectioneeringByCandidate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electioneeringByCandidate")

-- |
data ElectioneeringByCandidatePage = ElectioneeringByCandidatePage
  { electioneeringByCandidatePagePagination :: OffsetInfo -- ^
  , electioneeringByCandidatePageResults    :: [ElectioneeringByCandidate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ElectioneeringByCandidatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electioneeringByCandidatePage")
instance ToJSON ElectioneeringByCandidatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electioneeringByCandidatePage")

-- |
data ElectioneeringPage = ElectioneeringPage
  { electioneeringPagePagination :: SeekInfo -- ^
  , electioneeringPageResults    :: [Electioneering] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ElectioneeringPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electioneeringPage")
instance ToJSON ElectioneeringPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electioneeringPage")

-- |
data ElectionsList = ElectionsList
  { electionsListCycle    :: Int -- ^
  , electionsListDistrict :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , electionsListOffice   :: Text -- ^ Federal office candidate runs for: H, S or P
  , electionsListState    :: Text -- ^ US state or territory
  } deriving (Show, Eq, Generic)

instance FromJSON ElectionsList where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electionsList")
instance ToJSON ElectionsList where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electionsList")

-- |
data ElectionsListPage = ElectionsListPage
  { electionsListPagePagination :: OffsetInfo -- ^
  , electionsListPageResults    :: [ElectionsList] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ElectionsListPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electionsListPage")
instance ToJSON ElectionsListPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electionsListPage")

-- |
data EntityReceiptDisbursementTotals = EntityReceiptDisbursementTotals
  { entityReceiptDisbursementTotalsCumulative'Underscorecandidate'Underscoredisbursements :: Float -- ^ Cumulative candidate disbursements in a two year period, adjusted to avoid double counting.
  , entityReceiptDisbursementTotalsCumulative'Underscorecandidate'Underscorereceipts :: Float -- ^ Cumulative candidate receipts in a two year period, adjusted to avoid double counting.
  , entityReceiptDisbursementTotalsCumulative'Underscorepac'Underscoredisbursements :: Float -- ^ Cumulative PAC disbursements in a two year period, adjusted to avoid double counting.
  , entityReceiptDisbursementTotalsCumulative'Underscorepac'Underscorereceipts :: Float -- ^ Cumulative PAC recipts in a two year period, adjusted to avoid double counting.
  , entityReceiptDisbursementTotalsCumulative'Underscoreparty'Underscoredisbursements :: Float -- ^ Cumulative party disbursements in a two year period, adjusted to avoid double counting.
  , entityReceiptDisbursementTotalsCumulative'Underscoreparty'Underscorereceipts :: Float -- ^ Cumulative party receipts in a two year period, adjusted to avoid double counting.
  , entityReceiptDisbursementTotalsCycle :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , entityReceiptDisbursementTotalsEnd'Underscoredate :: Date -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON EntityReceiptDisbursementTotals where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entityReceiptDisbursementTotals")
instance ToJSON EntityReceiptDisbursementTotals where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entityReceiptDisbursementTotals")

-- |
data EntityReceiptDisbursementTotalsPage = EntityReceiptDisbursementTotalsPage
  { entityReceiptDisbursementTotalsPagePagination :: OffsetInfo -- ^
  , entityReceiptDisbursementTotalsPageResults :: [EntityReceiptDisbursementTotals] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON EntityReceiptDisbursementTotalsPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "entityReceiptDisbursementTotalsPage")
instance ToJSON EntityReceiptDisbursementTotalsPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "entityReceiptDisbursementTotalsPage")

-- |
data Filings = Filings
  { filingsAmendment'Underscorechain :: [Double] -- ^
  , filingsAmendment'Underscoreindicator :: Text -- ^  The first value in the chain is the original filing.  The ordering in the chain reflects the order the amendments were filed up to the amendment being viewed.
  , filingsAmendment'Underscoreversion :: Int -- ^
  , filingsBeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , filingsCandidate'Underscoreid :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , filingsCandidate'Underscorename :: Text -- ^ Name of candidate running for office
  , filingsCash'Underscoreon'Underscorehand'Underscorebeginning'Underscoreperiod :: Double -- ^ Balance for the committee at the start of the two-year period
  , filingsCash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^ Ending cash balance on the most recent filing
  , filingsCmte'Underscoretp :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , filingsCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , filingsCommittee'Underscorename :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , filingsCoverage'Underscoreend'Underscoredate :: Date -- ^ Ending date of the reporting period
  , filingsCoverage'Underscorestart'Underscoredate :: Date -- ^ Beginning date of the reporting period
  , filingsCsv'Underscoreurl :: Text -- ^
  , filingsCycle :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , filingsDebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^ Debts owed by the committee
  , filingsDebts'Underscoreowed'Underscoreto'Underscorecommittee :: Double -- ^ Debts owed to the committee
  , filingsDocument'Underscoredescription :: Text -- ^
  , filingsDocument'Underscoretype :: Text -- ^  The type of document for documents other than reports:     - 2 24 Hour Contribution Notice     - 4 48 Hour Contribution Notice     - A Debt Settlement Statement     - B Acknowledgment of Receipt of Debt Settlement Statement     - C RFAI: Debt Settlement First Notice     - D Commission Debt Settlement Review     - E Commission Response TO Debt Settlement Request     - F Administrative Termination     - G Debt Settlement Plan Amendment     - H Disavowal Notice     - I Disavowal Response     - J Conduit Report     - K Termination Approval     - L Repeat Non-Filer Notice     - M Filing Frequency Change Notice     - N Paper Amendment to Electronic Report     - O Acknowledgment of Filing Frequency Change     - S RFAI: Debt Settlement Second     - T Miscellaneous Report TO FEC     - V Repeat Violation Notice (441A OR 441B)     - P Notice of Paper Filing     - R F3L Filing Frequency Change Notice     - Q Acknowledgment of F3L Filing Frequency Change     - U Unregistered Committee Notice
  , filingsDocument'Underscoretype'Underscorefull :: Text -- ^  The type of document for documents other than reports:     - 2 24 Hour Contribution Notice     - 4 48 Hour Contribution Notice     - A Debt Settlement Statement     - B Acknowledgment of Receipt of Debt Settlement Statement     - C RFAI: Debt Settlement First Notice     - D Commission Debt Settlement Review     - E Commission Response TO Debt Settlement Request     - F Administrative Termination     - G Debt Settlement Plan Amendment     - H Disavowal Notice     - I Disavowal Response     - J Conduit Report     - K Termination Approval     - L Repeat Non-Filer Notice     - M Filing Frequency Change Notice     - N Paper Amendment to Electronic Report     - O Acknowledgment of Filing Frequency Change     - S RFAI: Debt Settlement Second     - T Miscellaneous Report TO FEC     - V Repeat Violation Notice (441A OR 441B)     - P Notice of Paper Filing     - R F3L Filing Frequency Change Notice     - Q Acknowledgment of F3L Filing Frequency Change     - U Unregistered Committee Notice
  , filingsElection'Underscoreyear :: Int -- ^ Year of election
  , filingsEnding'Underscoreimage'Underscorenumber :: Text -- ^
  , filingsFec'Underscorefile'Underscoreid :: Text -- ^
  , filingsFec'Underscoreurl :: Text -- ^
  , filingsFile'Underscorenumber :: Int -- ^
  , filingsForm'Underscoretype :: Text -- ^  Indicates the type of form that was filed. ex: F1, F2, F3P, F3X etc...
  , filingsHouse'Underscorepersonal'Underscorefunds :: Double -- ^
  , filingsHtml'Underscoreurl :: Text -- ^ HTML link to the filing.
  , filingsIs'Underscoreamended :: Bool -- ^
  , filingsMeans'Underscorefiled :: Text -- ^ The method used to file with the FEC, either electronic or on paper.
  , filingsMost'Underscorerecent :: Bool -- ^
  , filingsMost'Underscorerecent'Underscorefile'Underscorenumber :: Int -- ^
  , filingsNet'Underscoredonations :: Double -- ^
  , filingsOffice :: Text -- ^ Federal office candidate runs for: H, S or P
  , filingsOpposition'Underscorepersonal'Underscorefunds :: Double -- ^
  , filingsPages :: Int -- ^ Number of pages in the document
  , filingsParty :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , filingsPdf'Underscoreurl :: Text -- ^
  , filingsPrevious'Underscorefile'Underscorenumber :: Int -- ^
  , filingsPrimary'Underscoregeneral'Underscoreindicator :: Text -- ^
  , filingsReceipt'Underscoredate :: Date -- ^ Date the FEC received the electronic or paper record
  , filingsReport'Underscoretype :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , filingsReport'Underscoretype'Underscorefull :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , filingsReport'Underscoreyear :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , filingsRequest'Underscoretype :: Text -- ^
  , filingsSenate'Underscorepersonal'Underscorefunds :: Double -- ^
  , filingsState :: Text -- ^ US state or territory where a candidate runs for office
  , filingsSub'Underscoreid :: Text -- ^
  , filingsTotal'Underscorecommunication'Underscorecost :: Double -- ^
  , filingsTotal'Underscoredisbursements :: Double -- ^
  , filingsTotal'Underscoreindependent'Underscoreexpenditures :: Double -- ^
  , filingsTotal'Underscoreindividual'Underscorecontributions :: Double -- ^
  , filingsTotal'Underscorereceipts :: Double -- ^
  , filingsTreasurer'Underscorename :: Text -- ^ Name of the Committee's treasurer. If multiple treasurers for the committee, the most recent treasurer will be shown.
  , filingsUpdate'Underscoredate :: Date -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Filings where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "filings")
instance ToJSON Filings where
  toJSON = genericToJSON (removeFieldLabelPrefix False "filings")

-- |
data FilingsPage = FilingsPage
  { filingsPagePagination :: OffsetInfo -- ^
  , filingsPageResults    :: [Filings] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON FilingsPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "filingsPage")
instance ToJSON FilingsPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "filingsPage")

-- |
data Inline_response_default = Inline_response_default
  { inlineResponseDefaultPagination :: OffsetInfo -- ^
  , inlineResponseDefaultResults    :: [ElectionDate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault")
instance ToJSON Inline_response_default where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault")

-- |
data Inline_response_default_1 = Inline_response_default_1
  { inlineResponseDefault1Admin'Underscorefines :: [Inline_response_default_1_admin_fines] -- ^
  , inlineResponseDefault1Adrs :: [Inline_response_default_1_adrs] -- ^
  , inlineResponseDefault1Advisory'Underscoreopinions :: [Inline_response_default_1_advisory_opinions] -- ^
  , inlineResponseDefault1Murs :: [Inline_response_default_1_murs] -- ^
  , inlineResponseDefault1Regulations :: [Inline_response_default_1_regulations] -- ^
  , inlineResponseDefault1Statutes :: [Inline_response_default_1_statutes] -- ^
  , inlineResponseDefault1Total'Underscoreadmin'Underscorefines :: Int -- ^ Total number of Admin Fines matching the search criteria
  , inlineResponseDefault1Total'Underscoreadrs :: Int -- ^ Total number of ADRs matching the search criteria
  , inlineResponseDefault1Total'Underscoreadvisory'Underscoreopinions :: Int -- ^ Total number of Advisory Opinions matching the search criteria
  , inlineResponseDefault1Total'Underscoreall :: Int -- ^ Total number of legal documents matching the search criteria
  , inlineResponseDefault1Total'Underscoremurs :: Int -- ^ Total number of MURs matching the search criteria
  , inlineResponseDefault1Total'Underscoreregulations :: Int -- ^ Total number of Regulations matching the search criteria
  , inlineResponseDefault1Total'Underscorestatutes :: Int -- ^ Total number of Statutes matching the search criteria
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1")
instance ToJSON Inline_response_default_1 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1")

-- |
data Inline_response_default_1_admin_fines = Inline_response_default_1_admin_fines
  { inlineResponseDefault1AdminFinesChallenge'Underscoreoutcome :: Text -- ^
  , inlineResponseDefault1AdminFinesChallenge'Underscorereceipt'Underscoredate :: Date -- ^
  , inlineResponseDefault1AdminFinesCheck'Underscoreamount :: Double -- ^
  , inlineResponseDefault1AdminFinesCommission'Underscorevotes :: [Inline_response_default_1_commission_votes] -- ^
  , inlineResponseDefault1AdminFinesCommittee'Underscoreid :: Text -- ^
  , inlineResponseDefault1AdminFinesDoc'Underscoreid :: Text -- ^
  , inlineResponseDefault1AdminFinesDocument'Underscorehighlights :: Value -- ^
  , inlineResponseDefault1AdminFinesDocuments :: [Inline_response_default_1_documents] -- ^
  , inlineResponseDefault1AdminFinesFinal'Underscoredetermination'Underscoreamount :: Double -- ^
  , inlineResponseDefault1AdminFinesFinal'Underscoredetermination'Underscoredate :: Date -- ^
  , inlineResponseDefault1AdminFinesHighlights :: [Text] -- ^
  , inlineResponseDefault1AdminFinesName :: Text -- ^
  , inlineResponseDefault1AdminFinesNo :: Text -- ^
  , inlineResponseDefault1AdminFinesPetition'Underscorecourt'Underscoredecision'Underscoredate :: Date -- ^
  , inlineResponseDefault1AdminFinesPetition'Underscorecourt'Underscorefiling'Underscoredate :: Date -- ^
  , inlineResponseDefault1AdminFinesReason'Underscoreto'Underscorebelieve'Underscoreaction'Underscoredate :: Date -- ^
  , inlineResponseDefault1AdminFinesReason'Underscoreto'Underscorebelieve'Underscorefine'Underscoreamount :: Double -- ^
  , inlineResponseDefault1AdminFinesReport'Underscoretype :: Text -- ^
  , inlineResponseDefault1AdminFinesReport'Underscoreyear :: Text -- ^
  , inlineResponseDefault1AdminFinesTreasury'Underscorereferral'Underscoreamount :: Double -- ^
  , inlineResponseDefault1AdminFinesTreasury'Underscorereferral'Underscoredate :: Date -- ^
  , inlineResponseDefault1AdminFinesUrl :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_admin_fines where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1AdminFines")
instance ToJSON Inline_response_default_1_admin_fines where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1AdminFines")

-- |
data Inline_response_default_1_adrs = Inline_response_default_1_adrs
  { inlineResponseDefault1AdrsClose'Underscoredate :: Date -- ^
  , inlineResponseDefault1AdrsCommission'Underscorevotes :: [Inline_response_default_1_commission_votes] -- ^
  , inlineResponseDefault1AdrsDispositions :: [Inline_response_default_1_dispositions] -- ^
  , inlineResponseDefault1AdrsDoc'Underscoreid :: Text -- ^
  , inlineResponseDefault1AdrsDocument'Underscorehighlights :: Value -- ^
  , inlineResponseDefault1AdrsDocuments :: [Inline_response_default_1_documents] -- ^
  , inlineResponseDefault1AdrsElection'Underscorecycles :: Int -- ^
  , inlineResponseDefault1AdrsHighlights :: [Text] -- ^
  , inlineResponseDefault1AdrsName :: Text -- ^
  , inlineResponseDefault1AdrsNo :: Text -- ^
  , inlineResponseDefault1AdrsOpen'Underscoredate :: Date -- ^
  , inlineResponseDefault1AdrsParticipants :: [Inline_response_default_1_participants] -- ^
  , inlineResponseDefault1AdrsRespondents :: [Text] -- ^
  , inlineResponseDefault1AdrsSubjects :: [Text] -- ^
  , inlineResponseDefault1AdrsUrl :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_adrs where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1Adrs")
instance ToJSON Inline_response_default_1_adrs where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1Adrs")

-- |
data Inline_response_default_1_advisory_opinions = Inline_response_default_1_advisory_opinions
  { inlineResponseDefault1AdvisoryOpinionsAo'Underscorecitations :: [Inline_response_default_1_ao_citations] -- ^
  , inlineResponseDefault1AdvisoryOpinionsAos'Underscorecited'Underscoreby :: [Inline_response_default_1_ao_citations] -- ^
  , inlineResponseDefault1AdvisoryOpinionsCommenter'Underscorenames :: [Text] -- ^
  , inlineResponseDefault1AdvisoryOpinionsDocument'Underscorehighlights :: Value -- ^
  , inlineResponseDefault1AdvisoryOpinionsDocuments :: [Inline_response_default_1_documents_1] -- ^
  , inlineResponseDefault1AdvisoryOpinionsEntities :: [Inline_response_default_1_entities] -- ^
  , inlineResponseDefault1AdvisoryOpinionsHighlights :: [Text] -- ^
  , inlineResponseDefault1AdvisoryOpinionsIs'Underscorepending :: Bool -- ^
  , inlineResponseDefault1AdvisoryOpinionsIssue'Underscoredate :: Date -- ^
  , inlineResponseDefault1AdvisoryOpinionsName :: Text -- ^
  , inlineResponseDefault1AdvisoryOpinionsNo :: Text -- ^
  , inlineResponseDefault1AdvisoryOpinionsRegulatory'Underscorecitations :: [Inline_response_default_1_regulatory_citations] -- ^
  , inlineResponseDefault1AdvisoryOpinionsRepresentative'Underscorenames :: [Text] -- ^
  , inlineResponseDefault1AdvisoryOpinionsRequest'Underscoredate :: Date -- ^
  , inlineResponseDefault1AdvisoryOpinionsRequestor'Underscorenames :: [Text] -- ^
  , inlineResponseDefault1AdvisoryOpinionsRequestor'Underscoretypes :: [Text] -- ^
  , inlineResponseDefault1AdvisoryOpinionsStatus :: Text -- ^
  , inlineResponseDefault1AdvisoryOpinionsStatutory'Underscorecitations :: [Inline_response_default_1_statutory_citations] -- ^
  , inlineResponseDefault1AdvisoryOpinionsSummary :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_advisory_opinions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1AdvisoryOpinions")
instance ToJSON Inline_response_default_1_advisory_opinions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1AdvisoryOpinions")

-- |
data Inline_response_default_1_ao_citations = Inline_response_default_1_ao_citations
  { inlineResponseDefault1AoCitationsName :: Text -- ^
  , inlineResponseDefault1AoCitationsNo   :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_ao_citations where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1AoCitations")
instance ToJSON Inline_response_default_1_ao_citations where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1AoCitations")

-- |
data Inline_response_default_1_citations = Inline_response_default_1_citations
  { inlineResponseDefault1CitationsText  :: Text -- ^
  , inlineResponseDefault1CitationsTitle :: Text -- ^
  , inlineResponseDefault1CitationsType  :: Text -- ^
  , inlineResponseDefault1CitationsUrl   :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_citations where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1Citations")
instance ToJSON Inline_response_default_1_citations where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1Citations")

-- |
data Inline_response_default_1_commission_votes = Inline_response_default_1_commission_votes
  { inlineResponseDefault1CommissionVotesAction              :: Text -- ^
  , inlineResponseDefault1CommissionVotesVote'Underscoredate :: Date -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_commission_votes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1CommissionVotes")
instance ToJSON Inline_response_default_1_commission_votes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1CommissionVotes")

-- |
data Inline_response_default_1_dispositions = Inline_response_default_1_dispositions
  { inlineResponseDefault1DispositionsCitations :: [Inline_response_default_1_citations] -- ^
  , inlineResponseDefault1DispositionsDisposition :: Text -- ^
  , inlineResponseDefault1DispositionsPenalty :: Double -- ^
  , inlineResponseDefault1DispositionsRespondent :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_dispositions where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1Dispositions")
instance ToJSON Inline_response_default_1_dispositions where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1Dispositions")

-- |
data Inline_response_default_1_documents = Inline_response_default_1_documents
  { inlineResponseDefault1DocumentsCategory                :: Text -- ^
  , inlineResponseDefault1DocumentsDescription             :: Text -- ^
  , inlineResponseDefault1DocumentsDocument'Underscoredate :: Date -- ^
  , inlineResponseDefault1DocumentsDocument'Underscoreid   :: Int -- ^
  , inlineResponseDefault1DocumentsLength                  :: Int -- ^
  , inlineResponseDefault1DocumentsUrl                     :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_documents where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1Documents")
instance ToJSON Inline_response_default_1_documents where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1Documents")

-- |
data Inline_response_default_1_documents_1 = Inline_response_default_1_documents_1
  { inlineResponseDefault1Documents1Category              :: Text -- ^
  , inlineResponseDefault1Documents1Date                  :: Date -- ^
  , inlineResponseDefault1Documents1Description           :: Text -- ^
  , inlineResponseDefault1Documents1Document'Underscoreid :: Int -- ^
  , inlineResponseDefault1Documents1Url                   :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_documents_1 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1Documents1")
instance ToJSON Inline_response_default_1_documents_1 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1Documents1")

-- |
data Inline_response_default_1_entities = Inline_response_default_1_entities
  { inlineResponseDefault1EntitiesName :: Text -- ^
  , inlineResponseDefault1EntitiesRole :: Text -- ^
  , inlineResponseDefault1EntitiesType :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_entities where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1Entities")
instance ToJSON Inline_response_default_1_entities where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1Entities")

-- |
data Inline_response_default_1_murs = Inline_response_default_1_murs
  { inlineResponseDefault1MursClose'Underscoredate :: Date -- ^
  , inlineResponseDefault1MursCommission'Underscorevotes :: [Inline_response_default_1_commission_votes] -- ^
  , inlineResponseDefault1MursDispositions :: [Inline_response_default_1_dispositions] -- ^
  , inlineResponseDefault1MursDoc'Underscoreid :: Text -- ^
  , inlineResponseDefault1MursDocument'Underscorehighlights :: Value -- ^
  , inlineResponseDefault1MursDocuments :: [Inline_response_default_1_documents] -- ^
  , inlineResponseDefault1MursElection'Underscorecycles :: Int -- ^
  , inlineResponseDefault1MursHighlights :: [Text] -- ^
  , inlineResponseDefault1MursMur'Underscoretype :: Text -- ^
  , inlineResponseDefault1MursName :: Text -- ^
  , inlineResponseDefault1MursNo :: Text -- ^
  , inlineResponseDefault1MursOpen'Underscoredate :: Date -- ^
  , inlineResponseDefault1MursParticipants :: [Inline_response_default_1_participants] -- ^
  , inlineResponseDefault1MursRespondents :: [Text] -- ^
  , inlineResponseDefault1MursSubjects :: [Text] -- ^
  , inlineResponseDefault1MursUrl :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_murs where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1Murs")
instance ToJSON Inline_response_default_1_murs where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1Murs")

-- |
data Inline_response_default_1_participants = Inline_response_default_1_participants
  { inlineResponseDefault1ParticipantsCitations :: Value -- ^
  , inlineResponseDefault1ParticipantsName      :: Text -- ^
  , inlineResponseDefault1ParticipantsRole      :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_participants where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1Participants")
instance ToJSON Inline_response_default_1_participants where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1Participants")

-- |
data Inline_response_default_1_regulations = Inline_response_default_1_regulations
  { inlineResponseDefault1RegulationsDoc'Underscoreid              :: Text -- ^
  , inlineResponseDefault1RegulationsDocument'Underscorehighlights :: Value -- ^
  , inlineResponseDefault1RegulationsHighlights                    :: [Text] -- ^
  , inlineResponseDefault1RegulationsName                          :: Text -- ^
  , inlineResponseDefault1RegulationsNo                            :: Text -- ^
  , inlineResponseDefault1RegulationsUrl                           :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_regulations where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1Regulations")
instance ToJSON Inline_response_default_1_regulations where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1Regulations")

-- |
data Inline_response_default_1_regulatory_citations = Inline_response_default_1_regulatory_citations
  { inlineResponseDefault1RegulatoryCitationsPart    :: Int -- ^
  , inlineResponseDefault1RegulatoryCitationsSection :: Int -- ^
  , inlineResponseDefault1RegulatoryCitationsTitle   :: Int -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_regulatory_citations where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1RegulatoryCitations")
instance ToJSON Inline_response_default_1_regulatory_citations where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1RegulatoryCitations")

-- |
data Inline_response_default_1_statutes = Inline_response_default_1_statutes
  { inlineResponseDefault1StatutesChapter                       :: Text -- ^
  , inlineResponseDefault1StatutesDoc'Underscoreid              :: Text -- ^
  , inlineResponseDefault1StatutesDocument'Underscorehighlights :: Value -- ^
  , inlineResponseDefault1StatutesHighlights                    :: [Text] -- ^
  , inlineResponseDefault1StatutesName                          :: Text -- ^
  , inlineResponseDefault1StatutesNo                            :: Text -- ^
  , inlineResponseDefault1StatutesTitle                         :: Text -- ^
  , inlineResponseDefault1StatutesUrl                           :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_statutes where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1Statutes")
instance ToJSON Inline_response_default_1_statutes where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1Statutes")

-- |
data Inline_response_default_1_statutory_citations = Inline_response_default_1_statutory_citations
  { inlineResponseDefault1StatutoryCitationsSection :: Text -- ^
  , inlineResponseDefault1StatutoryCitationsTitle   :: Int -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_statutory_citations where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1StatutoryCitations")
instance ToJSON Inline_response_default_1_statutory_citations where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1StatutoryCitations")

-- |
data Inline_response_default_2 = Inline_response_default_2
  { inlineResponseDefault2Pagination :: OffsetInfo -- ^
  , inlineResponseDefault2Results    :: [ReportDate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_2 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault2")
instance ToJSON Inline_response_default_2 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault2")

-- |
data Inline_response_default_3 = Inline_response_default_3
  { inlineResponseDefault3Pagination :: OffsetInfo -- ^
  , inlineResponseDefault3Results    :: [Inline_response_default_3_results] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_3 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault3")
instance ToJSON Inline_response_default_3 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault3")

-- |
data Inline_response_default_3_results = Inline_response_default_3_results
  { inlineResponseDefault3ResultsAction'Underscorecode :: Text -- ^
  , inlineResponseDefault3ResultsAction'Underscorecode'Underscorefull :: Text -- ^
  , inlineResponseDefault3ResultsCandidate'Underscorefirst'Underscorename :: Text -- ^
  , inlineResponseDefault3ResultsCandidate'Underscoreid :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , inlineResponseDefault3ResultsCandidate'Underscorelast'Underscorename :: Text -- ^
  , inlineResponseDefault3ResultsCandidate'Underscoremiddle'Underscorename :: Text -- ^
  , inlineResponseDefault3ResultsCandidate'Underscorename :: Text -- ^ Name of candidate running for office
  , inlineResponseDefault3ResultsCandidate'Underscoreoffice :: Text -- ^
  , inlineResponseDefault3ResultsCandidate'Underscoreoffice'Underscoredistrict :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , inlineResponseDefault3ResultsCandidate'Underscoreoffice'Underscorefull :: Text -- ^
  , inlineResponseDefault3ResultsCandidate'Underscoreoffice'Underscorestate :: Text -- ^
  , inlineResponseDefault3ResultsCandidate'Underscoreoffice'Underscorestate'Underscorefull :: Text -- ^
  , inlineResponseDefault3ResultsCandidate'Underscoreprefix :: Text -- ^
  , inlineResponseDefault3ResultsCandidate'Underscoresuffix :: Text -- ^
  , inlineResponseDefault3ResultsCommittee :: CommitteeHistory -- ^
  , inlineResponseDefault3ResultsCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , inlineResponseDefault3ResultsCycle :: Int -- ^
  , inlineResponseDefault3ResultsDue'Underscoredate'Underscoreterms :: Text -- ^
  , inlineResponseDefault3ResultsElection'Underscoretype :: Text -- ^
  , inlineResponseDefault3ResultsElection'Underscoretype'Underscorefull :: Text -- ^
  , inlineResponseDefault3ResultsEntity'Underscoretype :: Text -- ^
  , inlineResponseDefault3ResultsEntity'Underscoretype'Underscorefull :: Text -- ^
  , inlineResponseDefault3ResultsFec'Underscorecommittee'Underscoreid :: Text -- ^
  , inlineResponseDefault3ResultsFec'Underscoreelection'Underscoretype'Underscorefull :: Text -- ^
  , inlineResponseDefault3ResultsFec'Underscoreelection'Underscoretype'Underscoreyear :: Text -- ^
  , inlineResponseDefault3ResultsFile'Underscorenumber :: Int -- ^
  , inlineResponseDefault3ResultsFiling'Underscoreform :: Text -- ^
  , inlineResponseDefault3ResultsImage'Underscorenumber :: Text -- ^ An unique identifier for each page the electronic or paper report.
  , inlineResponseDefault3ResultsIncurred'Underscoredate :: Integer -- ^
  , inlineResponseDefault3ResultsInterest'Underscorerate'Underscoreterms :: Text -- ^
  , inlineResponseDefault3ResultsLine'Underscorenumber :: Text -- ^
  , inlineResponseDefault3ResultsLink'Underscoreid :: Int -- ^
  , inlineResponseDefault3ResultsLoad'Underscoredate :: Integer -- ^
  , inlineResponseDefault3ResultsLoan'Underscorebalance :: Float -- ^
  , inlineResponseDefault3ResultsLoan'Underscoresource'Underscorecity :: Text -- ^
  , inlineResponseDefault3ResultsLoan'Underscoresource'Underscorefirst'Underscorename :: Text -- ^
  , inlineResponseDefault3ResultsLoan'Underscoresource'Underscorelast'Underscorename :: Text -- ^
  , inlineResponseDefault3ResultsLoan'Underscoresource'Underscoremiddle'Underscorename :: Text -- ^
  , inlineResponseDefault3ResultsLoan'Underscoresource'Underscorename :: Text -- ^ Source of the loan (i.e., bank loan, brokerage account, credit card, home equity line of credit,other line of credit, or personal funds of the candidate
  , inlineResponseDefault3ResultsLoan'Underscoresource'Underscoreprefix :: Text -- ^
  , inlineResponseDefault3ResultsLoan'Underscoresource'Underscorestate :: Text -- ^
  , inlineResponseDefault3ResultsLoan'Underscoresource'Underscorestreet'Underscore1 :: Text -- ^
  , inlineResponseDefault3ResultsLoan'Underscoresource'Underscorestreet'Underscore2 :: Text -- ^
  , inlineResponseDefault3ResultsLoan'Underscoresource'Underscoresuffix :: Text -- ^
  , inlineResponseDefault3ResultsLoan'Underscoresource'Underscorezip :: Int -- ^
  , inlineResponseDefault3ResultsMemo'Underscorecode :: Text -- ^
  , inlineResponseDefault3ResultsMemo'Underscoretext :: Text -- ^
  , inlineResponseDefault3ResultsOriginal'Underscoreloan'Underscoreamount :: Float -- ^
  , inlineResponseDefault3ResultsOriginal'Underscoresub'Underscoreid :: Int -- ^
  , inlineResponseDefault3ResultsPayment'Underscoreto'Underscoredate :: Float -- ^
  , inlineResponseDefault3ResultsPdf'Underscoreurl :: Text -- ^
  , inlineResponseDefault3ResultsPersonally'Underscorefunded :: Text -- ^
  , inlineResponseDefault3ResultsReport'Underscoretype :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , inlineResponseDefault3ResultsReport'Underscoreyear :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , inlineResponseDefault3ResultsSchedule'Underscorea'Underscoreline'Underscorenumber :: Int -- ^
  , inlineResponseDefault3ResultsSchedule'Underscoretype :: Text -- ^
  , inlineResponseDefault3ResultsSchedule'Underscoretype'Underscorefull :: Text -- ^
  , inlineResponseDefault3ResultsSecured'Underscoreind :: Text -- ^
  , inlineResponseDefault3ResultsSub'Underscoreid :: Text -- ^
  , inlineResponseDefault3ResultsTransaction'Underscoreid :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_3_results where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault3Results")
instance ToJSON Inline_response_default_3_results where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault3Results")

-- |
data Inline_response_default_4 = Inline_response_default_4
  { inlineResponseDefault4Pagination :: OffsetInfo -- ^
  , inlineResponseDefault4Results    :: [Inline_response_default_4_results] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_4 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault4")
instance ToJSON Inline_response_default_4 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault4")

-- |
data Inline_response_default_4_results = Inline_response_default_4_results
  { inlineResponseDefault4ResultsAction'Underscorecode :: Text -- ^
  , inlineResponseDefault4ResultsAction'Underscorecode'Underscorefull :: Text -- ^
  , inlineResponseDefault4ResultsAmount'Underscoreincurred'Underscoreperiod :: Float -- ^
  , inlineResponseDefault4ResultsCandidate'Underscorefirst'Underscorename :: Text -- ^
  , inlineResponseDefault4ResultsCandidate'Underscoreid :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , inlineResponseDefault4ResultsCandidate'Underscorelast'Underscorename :: Text -- ^
  , inlineResponseDefault4ResultsCandidate'Underscoreoffice :: Text -- ^
  , inlineResponseDefault4ResultsCandidate'Underscoreoffice'Underscoredistrict :: Text -- ^
  , inlineResponseDefault4ResultsCandidate'Underscoreoffice'Underscorestate :: Text -- ^
  , inlineResponseDefault4ResultsCandidate'Underscoreoffice'Underscorestate'Underscorefull :: Text -- ^
  , inlineResponseDefault4ResultsCanidate'Underscorename :: Text -- ^ Name of candidate running for office
  , inlineResponseDefault4ResultsCommittee :: CommitteeHistory -- ^
  , inlineResponseDefault4ResultsCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , inlineResponseDefault4ResultsCommittee'Underscorename :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , inlineResponseDefault4ResultsConduit'Underscorecommittee'Underscorecity :: Text -- ^
  , inlineResponseDefault4ResultsConduit'Underscorecommittee'Underscoreid :: Text -- ^
  , inlineResponseDefault4ResultsConduit'Underscorecommittee'Underscorename :: Text -- ^
  , inlineResponseDefault4ResultsConduit'Underscorecommittee'Underscorestate :: Text -- ^
  , inlineResponseDefault4ResultsConduit'Underscorecommittee'Underscorestreet1 :: Text -- ^
  , inlineResponseDefault4ResultsConduit'Underscorecommittee'Underscorestreet2 :: Text -- ^
  , inlineResponseDefault4ResultsConduit'Underscorecommittee'Underscorezip :: Int -- ^
  , inlineResponseDefault4ResultsCreditor'Underscoredebtor'Underscorecity :: Text -- ^
  , inlineResponseDefault4ResultsCreditor'Underscoredebtor'Underscorefirst'Underscorename :: Text -- ^
  , inlineResponseDefault4ResultsCreditor'Underscoredebtor'Underscoreid :: Text -- ^
  , inlineResponseDefault4ResultsCreditor'Underscoredebtor'Underscorelast'Underscorename :: Text -- ^
  , inlineResponseDefault4ResultsCreditor'Underscoredebtor'Underscoremiddle'Underscorename :: Text -- ^
  , inlineResponseDefault4ResultsCreditor'Underscoredebtor'Underscorename :: Text -- ^
  , inlineResponseDefault4ResultsCreditor'Underscoredebtor'Underscoreprefix :: Text -- ^
  , inlineResponseDefault4ResultsCreditor'Underscoredebtor'Underscorestate :: Text -- ^
  , inlineResponseDefault4ResultsCreditor'Underscoredebtor'Underscorestreet1 :: Text -- ^
  , inlineResponseDefault4ResultsCreditor'Underscoredebtor'Underscorestreet2 :: Text -- ^
  , inlineResponseDefault4ResultsCreditor'Underscoredebtor'Underscoresuffix :: Text -- ^
  , inlineResponseDefault4ResultsElection'Underscorecycle :: Int -- ^
  , inlineResponseDefault4ResultsEntity'Underscoretype :: Text -- ^
  , inlineResponseDefault4ResultsFile'Underscorenumber :: Int -- ^
  , inlineResponseDefault4ResultsFiling'Underscoreform :: Text -- ^
  , inlineResponseDefault4ResultsImage'Underscorenumber :: Text -- ^ An unique identifier for each page the electronic or paper report.
  , inlineResponseDefault4ResultsLine'Underscorenumber :: Text -- ^
  , inlineResponseDefault4ResultsLink'Underscoreid :: Int -- ^
  , inlineResponseDefault4ResultsLoad'Underscoredate :: Integer -- ^
  , inlineResponseDefault4ResultsNature'Underscoreof'Underscoredebt :: Text -- ^
  , inlineResponseDefault4ResultsOriginal'Underscoresub'Underscoreid :: Int -- ^
  , inlineResponseDefault4ResultsOutstanding'Underscorebalance'Underscorebeginning'Underscoreof'Underscoreperiod :: Float -- ^
  , inlineResponseDefault4ResultsOutstanding'Underscorebalance'Underscoreclose'Underscoreof'Underscoreperiod :: Float -- ^
  , inlineResponseDefault4ResultsPayment'Underscoreperiod :: Float -- ^
  , inlineResponseDefault4ResultsPdf'Underscoreurl :: Text -- ^
  , inlineResponseDefault4ResultsReport'Underscoretype :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , inlineResponseDefault4ResultsReport'Underscoreyear :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , inlineResponseDefault4ResultsSchedule'Underscoretype :: Text -- ^
  , inlineResponseDefault4ResultsSchedule'Underscoretype'Underscorefull :: Text -- ^
  , inlineResponseDefault4ResultsSub'Underscoreid :: Text -- ^
  , inlineResponseDefault4ResultsTransaction'Underscoreid :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_4_results where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault4Results")
instance ToJSON Inline_response_default_4_results where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault4Results")

-- |
data Inline_response_default_5 = Inline_response_default_5
  { inlineResponseDefault5Pagination :: OffsetInfo -- ^
  , inlineResponseDefault5Results    :: [Inline_response_default_5_results] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_5 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault5")
instance ToJSON Inline_response_default_5 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault5")

-- |
data Inline_response_default_5_results = Inline_response_default_5_results
  { inlineResponseDefault5ResultsAction'Underscorecode :: Text -- ^
  , inlineResponseDefault5ResultsAction'Underscorecode'Underscorefull :: Text -- ^
  , inlineResponseDefault5ResultsAggregate'Underscoregeneral'Underscoreelection'Underscoreexpenditure :: Text -- ^
  , inlineResponseDefault5ResultsBack'Underscorereference'Underscoreschedule'Underscorename :: Text -- ^
  , inlineResponseDefault5ResultsBack'Underscorereference'Underscoretransaction'Underscoreid :: Int -- ^
  , inlineResponseDefault5ResultsCandidate'Underscorefirst'Underscorename :: Text -- ^
  , inlineResponseDefault5ResultsCandidate'Underscoreid :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , inlineResponseDefault5ResultsCandidate'Underscorelast'Underscorename :: Text -- ^
  , inlineResponseDefault5ResultsCandidate'Underscoremiddle'Underscorename :: Text -- ^
  , inlineResponseDefault5ResultsCandidate'Underscorename :: Text -- ^ Name of candidate running for office
  , inlineResponseDefault5ResultsCandidate'Underscoreoffice :: Text -- ^
  , inlineResponseDefault5ResultsCandidate'Underscoreoffice'Underscoredistrict :: Text -- ^
  , inlineResponseDefault5ResultsCandidate'Underscoreoffice'Underscorefull :: Text -- ^
  , inlineResponseDefault5ResultsCandidate'Underscoreoffice'Underscorestate :: Text -- ^
  , inlineResponseDefault5ResultsCandidate'Underscoreoffice'Underscorestate'Underscorefull :: Text -- ^
  , inlineResponseDefault5ResultsCandidate'Underscoreprefix :: Text -- ^
  , inlineResponseDefault5ResultsCandidate'Underscoresuffix :: Text -- ^
  , inlineResponseDefault5ResultsCatolog'Underscorecode :: Text -- ^
  , inlineResponseDefault5ResultsCatolog'Underscorecode'Underscorefull :: Text -- ^
  , inlineResponseDefault5ResultsCommittee :: CommitteeHistory -- ^
  , inlineResponseDefault5ResultsCommittee'Underscoredesignated'Underscorecoordinated'Underscoreexpenditure'Underscoreindicator :: Text -- ^
  , inlineResponseDefault5ResultsCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , inlineResponseDefault5ResultsCommittee'Underscorename :: Text -- ^
  , inlineResponseDefault5ResultsConduit'Underscorecommittee'Underscorecity :: Text -- ^
  , inlineResponseDefault5ResultsConduit'Underscorecommittee'Underscoreid :: Text -- ^
  , inlineResponseDefault5ResultsConduit'Underscorecommittee'Underscorename :: Text -- ^
  , inlineResponseDefault5ResultsConduit'Underscorecommittee'Underscorestate :: Text -- ^
  , inlineResponseDefault5ResultsConduit'Underscorecommittee'Underscorestreet1 :: Text -- ^
  , inlineResponseDefault5ResultsConduit'Underscorecommittee'Underscorestreet2 :: Text -- ^
  , inlineResponseDefault5ResultsConduit'Underscorecommittee'Underscorezip :: Int -- ^
  , inlineResponseDefault5ResultsDesignated'Underscorecommittee'Underscoreid :: Text -- ^
  , inlineResponseDefault5ResultsDesignated'Underscorecommittee'Underscorename :: Text -- ^
  , inlineResponseDefault5ResultsElection'Underscorecycle :: Int -- ^
  , inlineResponseDefault5ResultsEntity'Underscoretype :: Text -- ^
  , inlineResponseDefault5ResultsEntity'Underscoretype'Underscoredesc :: Text -- ^
  , inlineResponseDefault5ResultsExpenditure'Underscoreamount :: Int -- ^
  , inlineResponseDefault5ResultsExpenditure'Underscoredate :: Integer -- ^
  , inlineResponseDefault5ResultsExpenditure'Underscorepurpose'Underscorefull :: Text -- ^
  , inlineResponseDefault5ResultsExpenditure'Underscoretype :: Text -- ^
  , inlineResponseDefault5ResultsExpenditure'Underscoretype'Underscorefull :: Text -- ^
  , inlineResponseDefault5ResultsFile'Underscorenumber :: Int -- ^
  , inlineResponseDefault5ResultsFiling'Underscoreform :: Text -- ^
  , inlineResponseDefault5ResultsImage'Underscorenumber :: Text -- ^ An unique identifier for each page the electronic or paper report.
  , inlineResponseDefault5ResultsLine'Underscorenumber :: Text -- ^
  , inlineResponseDefault5ResultsLink'Underscoreid :: Int -- ^
  , inlineResponseDefault5ResultsLoad'Underscoredate :: Integer -- ^
  , inlineResponseDefault5ResultsMemo'Underscorecode :: Text -- ^
  , inlineResponseDefault5ResultsMemo'Underscorecode'Underscorefull :: Text -- ^
  , inlineResponseDefault5ResultsMemo'Underscoretext :: Text -- ^
  , inlineResponseDefault5ResultsOriginal'Underscoresub'Underscoreid :: Int -- ^
  , inlineResponseDefault5ResultsPayee'Underscorefirst'Underscorename :: Text -- ^
  , inlineResponseDefault5ResultsPayee'Underscorelast'Underscorename :: Text -- ^
  , inlineResponseDefault5ResultsPayee'Underscoremiddle'Underscorename :: Text -- ^
  , inlineResponseDefault5ResultsPayee'Underscorename :: Text -- ^
  , inlineResponseDefault5ResultsPdf'Underscoreurl :: Text -- ^
  , inlineResponseDefault5ResultsReport'Underscoretype :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , inlineResponseDefault5ResultsReport'Underscoreyear :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , inlineResponseDefault5ResultsSchedule'Underscoretype :: Text -- ^
  , inlineResponseDefault5ResultsSchedule'Underscoretype'Underscorefull :: Text -- ^
  , inlineResponseDefault5ResultsSub'Underscoreid :: Text -- ^
  , inlineResponseDefault5ResultsSubordinate'Underscorecommittee :: CommitteeHistory -- ^
  , inlineResponseDefault5ResultsSubordinate'Underscorecommittee'Underscoreid :: Text -- ^
  , inlineResponseDefault5ResultsTransaction'Underscoreid :: Text -- ^
  , inlineResponseDefault5ResultsUnlimited'Underscorespending'Underscoreflag :: Text -- ^
  , inlineResponseDefault5ResultsUnlimited'Underscorespending'Underscoreflag'Underscorefull :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_5_results where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault5Results")
instance ToJSON Inline_response_default_5_results where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault5Results")

-- |
data OffsetInfo = OffsetInfo
  { offsetInfoCount              :: Int -- ^
  , offsetInfoPage               :: Int -- ^
  , offsetInfoPages              :: Int -- ^
  , offsetInfoPer'Underscorepage :: Int -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON OffsetInfo where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "offsetInfo")
instance ToJSON OffsetInfo where
  toJSON = genericToJSON (removeFieldLabelPrefix False "offsetInfo")

-- |
data OperationsLog = OperationsLog
  { operationsLogAmendment'Underscoreindicator :: Text -- ^ Type of the report.N(new), A(amended) or T(cancel)
  , operationsLogBeginning'Underscoreimage'Underscorenumber :: Text -- ^  Unique identifier for the electronic or paper report. This number is used to construct PDF URLs to the original document.
  , operationsLogCandidate'Underscorecommittee'Underscoreid :: Text -- ^  A unique identifier of the registered filer.
  , operationsLogCoverage'Underscoreend'Underscoredate :: Integer -- ^ Ending date of the reporting period
  , operationsLogCoverage'Underscorestart'Underscoredate :: Integer -- ^ Beginning date of the reporting period
  , operationsLogEnding'Underscoreimage'Underscorenumber :: Text -- ^ Image number is an unique identifier for each page the electronic or paper report. The last image number corresponds to the image number for the last page of the document.
  , operationsLogForm'Underscoretype :: Text -- ^  Indicates the type of form that was filed. ex: F1, F2, F3P, F3X etc...
  , operationsLogReceipt'Underscoredate :: Integer -- ^ Date the FEC received the electronic or paper record
  , operationsLogReport'Underscoretype :: Text -- ^ Monthly, quarterly or other period covered reports
  , operationsLogReport'Underscoreyear :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , operationsLogStatus'Underscorenum :: Int -- ^  Status of the transactional report.     -0- Transaction is entered            into the system.           But not verified.     -1- Transaction is verified.
  , operationsLogSub'Underscoreid :: Int -- ^  A unique identifier of the transactional report.
  , operationsLogSummary'Underscoredata'Underscorecomplete'Underscoredate :: Integer -- ^ Date when the report is entered into the database
  , operationsLogSummary'Underscoredata'Underscoreverification'Underscoredate :: Integer -- ^ Same day or a day after the report is loaded in the database
  , operationsLogTransaction'Underscoredata'Underscorecomplete'Underscoredate :: Date -- ^ Date when the report is processed completely
  } deriving (Show, Eq, Generic)

instance FromJSON OperationsLog where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "operationsLog")
instance ToJSON OperationsLog where
  toJSON = genericToJSON (removeFieldLabelPrefix False "operationsLog")

-- |
data OperationsLogPage = OperationsLogPage
  { operationsLogPagePagination :: OffsetInfo -- ^
  , operationsLogPageResults    :: [OperationsLog] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON OperationsLogPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "operationsLogPage")
instance ToJSON OperationsLogPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "operationsLogPage")

-- |
data RadAnalyst = RadAnalyst
  { radAnalystAnalyst'Underscoreemail                    :: Text -- ^ Email of RAD analyst
  , radAnalystAnalyst'Underscoreid                       :: Double -- ^ ID of RAD analyst.
  , radAnalystAnalyst'Underscoreshort'Underscoreid       :: Double -- ^ Short ID of RAD analyst.
  , radAnalystAnalyst'Underscoretitle                    :: Text -- ^ Title of RAD analyst
  , radAnalystAssignment'Underscoreupdate'Underscoredate :: Date -- ^ Date of most recent RAD analyst assignment change
  , radAnalystCommittee'Underscoreid                     :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , radAnalystCommittee'Underscorename                   :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , radAnalystFirst'Underscorename                       :: Text -- ^ Fist name of RAD analyst
  , radAnalystLast'Underscorename                        :: Text -- ^ Last name of RAD analyst
  , radAnalystRad'Underscorebranch                       :: Text -- ^ Branch of RAD analyst
  , radAnalystTelephone'Underscoreext                    :: Double -- ^ Telephone extension of RAD analyst
  } deriving (Show, Eq, Generic)

instance FromJSON RadAnalyst where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "radAnalyst")
instance ToJSON RadAnalyst where
  toJSON = genericToJSON (removeFieldLabelPrefix False "radAnalyst")

-- |
data RadAnalystPage = RadAnalystPage
  { radAnalystPagePagination :: OffsetInfo -- ^
  , radAnalystPageResults    :: [RadAnalyst] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON RadAnalystPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "radAnalystPage")
instance ToJSON RadAnalystPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "radAnalystPage")

-- |
data ReportDate = ReportDate
  { reportDateCreate'Underscoredate                :: Date -- ^ Date the record was created
  , reportDateDue'Underscoredate                   :: Date -- ^ Date the report is due
  , reportDateReport'Underscoretype                :: Text -- ^
  , reportDateReport'Underscoretype'Underscorefull :: Text -- ^
  , reportDateReport'Underscoreyear                :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , reportDateUpdate'Underscoredate                :: Date -- ^ Date the record was updated
  } deriving (Show, Eq, Generic)

instance FromJSON ReportDate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reportDate")
instance ToJSON ReportDate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reportDate")

-- |
data ReportDatePage = ReportDatePage
  { reportDatePagePagination :: OffsetInfo -- ^
  , reportDatePageResults    :: [ReportDate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ReportDatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reportDatePage")
instance ToJSON ReportDatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reportDatePage")

-- |
data ReportType = ReportType
  { reportTypeReport'Underscoretype                :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , reportTypeReport'Underscoretype'Underscorefull :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  } deriving (Show, Eq, Generic)

instance FromJSON ReportType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reportType")
instance ToJSON ReportType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reportType")

-- |
data ScheduleA = ScheduleA
  { scheduleAAmendment'Underscoreindicator :: Text -- ^
  , scheduleAAmendment'Underscoreindicator'Underscoredesc :: Text -- ^
  , scheduleABack'Underscorereference'Underscoreschedule'Underscorename :: Text -- ^
  , scheduleABack'Underscorereference'Underscoretransaction'Underscoreid :: Text -- ^
  , scheduleACandidate'Underscorefirst'Underscorename :: Text -- ^
  , scheduleACandidate'Underscoreid :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , scheduleACandidate'Underscorelast'Underscorename :: Text -- ^
  , scheduleACandidate'Underscoremiddle'Underscorename :: Text -- ^
  , scheduleACandidate'Underscorename :: Text -- ^ Name of candidate running for office
  , scheduleACandidate'Underscoreoffice :: Text -- ^
  , scheduleACandidate'Underscoreoffice'Underscoredistrict :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , scheduleACandidate'Underscoreoffice'Underscorefull :: Text -- ^
  , scheduleACandidate'Underscoreoffice'Underscorestate :: Text -- ^
  , scheduleACandidate'Underscoreoffice'Underscorestate'Underscorefull :: Text -- ^
  , scheduleACandidate'Underscoreprefix :: Text -- ^
  , scheduleACandidate'Underscoresuffix :: Text -- ^
  , scheduleACommittee :: CommitteeHistory -- ^
  , scheduleACommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleACommittee'Underscorename :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , scheduleAConduit'Underscorecommittee'Underscorecity :: Text -- ^
  , scheduleAConduit'Underscorecommittee'Underscoreid :: Text -- ^
  , scheduleAConduit'Underscorecommittee'Underscorename :: Text -- ^
  , scheduleAConduit'Underscorecommittee'Underscorestate :: Text -- ^
  , scheduleAConduit'Underscorecommittee'Underscorestreet1 :: Text -- ^
  , scheduleAConduit'Underscorecommittee'Underscorestreet2 :: Text -- ^
  , scheduleAConduit'Underscorecommittee'Underscorezip :: Int -- ^
  , scheduleAContribution'Underscorereceipt'Underscoreamount :: Double -- ^
  , scheduleAContribution'Underscorereceipt'Underscoredate :: Date -- ^
  , scheduleAContributor :: CommitteeHistory -- ^
  , scheduleAContributor'Underscoreaggregate'Underscoreytd :: Double -- ^
  , scheduleAContributor'Underscorecity :: Text -- ^ City of contributor
  , scheduleAContributor'Underscoreemployer :: Text -- ^ Employer of contributor, filers need to make an effort to gather this information
  , scheduleAContributor'Underscorefirst'Underscorename :: Text -- ^
  , scheduleAContributor'Underscoreid :: Text -- ^ The FEC identifier should be represented here if the contributor is registered with the FEC.
  , scheduleAContributor'Underscorelast'Underscorename :: Text -- ^
  , scheduleAContributor'Underscoremiddle'Underscorename :: Text -- ^
  , scheduleAContributor'Underscorename :: Text -- ^ Name of contributor
  , scheduleAContributor'Underscoreoccupation :: Text -- ^ Occupation of contributor, filers need to make an effort to gather this information
  , scheduleAContributor'Underscoreprefix :: Text -- ^
  , scheduleAContributor'Underscorestate :: Text -- ^ State of contributor
  , scheduleAContributor'Underscorestreet'Underscore1 :: Text -- ^
  , scheduleAContributor'Underscorestreet'Underscore2 :: Text -- ^
  , scheduleAContributor'Underscoresuffix :: Text -- ^
  , scheduleAContributor'Underscorezip :: Text -- ^ Zip code of contributor
  , scheduleADonor'Underscorecommittee'Underscorename :: Text -- ^
  , scheduleAElection'Underscoretype :: Text -- ^
  , scheduleAElection'Underscoretype'Underscorefull :: Text -- ^
  , scheduleAEntity'Underscoretype :: Text -- ^
  , scheduleAEntity'Underscoretype'Underscoredesc :: Text -- ^
  , scheduleAFec'Underscoreelection'Underscoretype'Underscoredesc :: Text -- ^
  , scheduleAFec'Underscoreelection'Underscoreyear :: Text -- ^
  , scheduleAFile'Underscorenumber :: Int -- ^
  , scheduleAFiling'Underscoreform :: Text -- ^
  , scheduleAImage'Underscorenumber :: Text -- ^
  , scheduleAIncreased'Underscorelimit :: Text -- ^
  , scheduleAIs'Underscoreindividual :: Bool -- ^
  , scheduleALine'Underscorenumber :: Text -- ^
  , scheduleALine'Underscorenumber'Underscorelabel :: Text -- ^
  , scheduleALink'Underscoreid :: Int -- ^
  , scheduleALoad'Underscoredate :: Integer -- ^
  , scheduleAMemo'Underscorecode :: Text -- ^
  , scheduleAMemo'Underscorecode'Underscorefull :: Text -- ^
  , scheduleAMemo'Underscoretext :: Text -- ^
  , scheduleAMemoed'Underscoresubtotal :: Bool -- ^
  , scheduleANational'Underscorecommittee'Underscorenonfederal'Underscoreaccount :: Text -- ^
  , scheduleAOriginal'Underscoresub'Underscoreid :: Text -- ^
  , scheduleAPdf'Underscoreurl :: Text -- ^
  , scheduleAReceipt'Underscoretype :: Text -- ^
  , scheduleAReceipt'Underscoretype'Underscoredesc :: Text -- ^
  , scheduleAReceipt'Underscoretype'Underscorefull :: Text -- ^
  , scheduleAReport'Underscoretype :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , scheduleAReport'Underscoreyear :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , scheduleASchedule'Underscoretype :: Text -- ^
  , scheduleASchedule'Underscoretype'Underscorefull :: Text -- ^
  , scheduleASub'Underscoreid :: Text -- ^
  , scheduleATransaction'Underscoreid :: Text -- ^
  , scheduleATwo'Underscoreyear'Underscoretransaction'Underscoreperiod :: Int -- ^  This is a two-year period that is derived from the year a transaction took place in the Itemized Schedule A and Schedule B tables. In cases where we have the date of the transaction (contribution_receipt_date in schedules/schedule_a, disbursement_date in schedules/schedule_b) the two_year_transaction_period is named after the ending, even-numbered year. If we do not have the date  of the transaction, we fall back to using the report year (report_year in both tables) instead,  making the same cycle adjustment as necessary. If no transaction year is specified, the results default to the most current cycle.
  , scheduleAUnused'Underscorecontbr'Underscoreid :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleA where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleA")
instance ToJSON ScheduleA where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleA")

-- |
data ScheduleAByEmployer = ScheduleAByEmployer
  { scheduleAByEmployerCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleAByEmployerCount                  :: Int -- ^ Number of records making up the total
  , scheduleAByEmployerCycle                  :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleAByEmployerEmployer               :: Text -- ^ Employer of contributor as reported on the committee's filing
  , scheduleAByEmployerTotal                  :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByEmployer where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByEmployer")
instance ToJSON ScheduleAByEmployer where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByEmployer")

-- |
data ScheduleAByEmployerPage = ScheduleAByEmployerPage
  { scheduleAByEmployerPagePagination :: OffsetInfo -- ^
  , scheduleAByEmployerPageResults    :: [ScheduleAByEmployer] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByEmployerPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByEmployerPage")
instance ToJSON ScheduleAByEmployerPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByEmployerPage")

-- |
data ScheduleAByOccupation = ScheduleAByOccupation
  { scheduleAByOccupationCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleAByOccupationCount                  :: Int -- ^ Number of records making up the total
  , scheduleAByOccupationCycle                  :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleAByOccupationOccupation             :: Text -- ^ Occupation of contributor as reported on the committee's filing
  , scheduleAByOccupationTotal                  :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByOccupation where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByOccupation")
instance ToJSON ScheduleAByOccupation where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByOccupation")

-- |
data ScheduleAByOccupationPage = ScheduleAByOccupationPage
  { scheduleAByOccupationPagePagination :: OffsetInfo -- ^
  , scheduleAByOccupationPageResults    :: [ScheduleAByOccupation] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByOccupationPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByOccupationPage")
instance ToJSON ScheduleAByOccupationPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByOccupationPage")

-- |
data ScheduleABySize = ScheduleABySize
  { scheduleABySizeCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleABySizeCount                  :: Int -- ^ Number of records making up the total
  , scheduleABySizeCycle                  :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleABySizeSize                   :: Int -- ^
  , scheduleABySizeTotal                  :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleABySize where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleABySize")
instance ToJSON ScheduleABySize where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleABySize")

-- |
data ScheduleABySizeCandidate = ScheduleABySizeCandidate
  { scheduleABySizeCandidateCandidate'Underscoreid :: Text -- ^
  , scheduleABySizeCandidateCycle                  :: Int -- ^
  , scheduleABySizeCandidateSize                   :: Int -- ^
  , scheduleABySizeCandidateTotal                  :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleABySizeCandidate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleABySizeCandidate")
instance ToJSON ScheduleABySizeCandidate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleABySizeCandidate")

-- |
data ScheduleABySizeCandidatePage = ScheduleABySizeCandidatePage
  { scheduleABySizeCandidatePagePagination :: OffsetInfo -- ^
  , scheduleABySizeCandidatePageResults    :: [ScheduleABySizeCandidate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleABySizeCandidatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleABySizeCandidatePage")
instance ToJSON ScheduleABySizeCandidatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleABySizeCandidatePage")

-- |
data ScheduleABySizePage = ScheduleABySizePage
  { scheduleABySizePagePagination :: OffsetInfo -- ^
  , scheduleABySizePageResults    :: [ScheduleABySize] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleABySizePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleABySizePage")
instance ToJSON ScheduleABySizePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleABySizePage")

-- |
data ScheduleAByState = ScheduleAByState
  { scheduleAByStateCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleAByStateCount                  :: Int -- ^ Number of records making up the total
  , scheduleAByStateCycle                  :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleAByStateState                  :: Text -- ^ US state or territory
  , scheduleAByStateState'Underscorefull   :: Text -- ^ US state or territory
  , scheduleAByStateTotal                  :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByState where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByState")
instance ToJSON ScheduleAByState where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByState")

-- |
data ScheduleAByStateCandidate = ScheduleAByStateCandidate
  { scheduleAByStateCandidateCandidate'Underscoreid :: Text -- ^
  , scheduleAByStateCandidateCycle                  :: Int -- ^
  , scheduleAByStateCandidateState                  :: Text -- ^
  , scheduleAByStateCandidateState'Underscorefull   :: Text -- ^
  , scheduleAByStateCandidateTotal                  :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByStateCandidate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByStateCandidate")
instance ToJSON ScheduleAByStateCandidate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByStateCandidate")

-- |
data ScheduleAByStateCandidatePage = ScheduleAByStateCandidatePage
  { scheduleAByStateCandidatePagePagination :: OffsetInfo -- ^
  , scheduleAByStateCandidatePageResults    :: [ScheduleAByStateCandidate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByStateCandidatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByStateCandidatePage")
instance ToJSON ScheduleAByStateCandidatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByStateCandidatePage")

-- |
data ScheduleAByStatePage = ScheduleAByStatePage
  { scheduleAByStatePagePagination :: OffsetInfo -- ^
  , scheduleAByStatePageResults    :: [ScheduleAByState] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByStatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByStatePage")
instance ToJSON ScheduleAByStatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByStatePage")

-- |
data ScheduleAByStateRecipientTotals = ScheduleAByStateRecipientTotals
  { scheduleAByStateRecipientTotalsCommittee'Underscoretype :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , scheduleAByStateRecipientTotalsCommittee'Underscoretype'Underscorefull :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , scheduleAByStateRecipientTotalsCount :: Int -- ^ Number of records making up the total.
  , scheduleAByStateRecipientTotalsCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , scheduleAByStateRecipientTotalsState :: Text -- ^ US state or territory
  , scheduleAByStateRecipientTotalsState'Underscorefull :: Text -- ^ US state or territory
  , scheduleAByStateRecipientTotalsTotal :: Double -- ^ The calculated total.
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByStateRecipientTotals where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByStateRecipientTotals")
instance ToJSON ScheduleAByStateRecipientTotals where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByStateRecipientTotals")

-- |
data ScheduleAByStateRecipientTotalsPage = ScheduleAByStateRecipientTotalsPage
  { scheduleAByStateRecipientTotalsPagePagination :: OffsetInfo -- ^
  , scheduleAByStateRecipientTotalsPageResults :: [ScheduleAByStateRecipientTotals] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByStateRecipientTotalsPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByStateRecipientTotalsPage")
instance ToJSON ScheduleAByStateRecipientTotalsPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByStateRecipientTotalsPage")

-- |
data ScheduleAByZip = ScheduleAByZip
  { scheduleAByZipCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleAByZipCount                  :: Int -- ^ Number of records making up the total
  , scheduleAByZipCycle                  :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleAByZipState                  :: Text -- ^ US state or territory
  , scheduleAByZipState'Underscorefull   :: Text -- ^ US state or territory
  , scheduleAByZipTotal                  :: Double -- ^
  , scheduleAByZipZip                    :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByZip where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByZip")
instance ToJSON ScheduleAByZip where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByZip")

-- |
data ScheduleAByZipPage = ScheduleAByZipPage
  { scheduleAByZipPagePagination :: OffsetInfo -- ^
  , scheduleAByZipPageResults    :: [ScheduleAByZip] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByZipPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByZipPage")
instance ToJSON ScheduleAByZipPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByZipPage")

-- |
data ScheduleAEfile = ScheduleAEfile
  { scheduleAEfileAmendment'Underscoreindicator :: Text -- ^
  , scheduleAEfileBack'Underscorereference'Underscoreschedule'Underscorename :: Text -- ^
  , scheduleAEfileBack'Underscorereference'Underscoretransaction'Underscoreid :: Text -- ^
  , scheduleAEfileBeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , scheduleAEfileCommittee :: CommitteeHistory -- ^
  , scheduleAEfileCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleAEfileConduit'Underscorecommittee'Underscorecity :: Text -- ^
  , scheduleAEfileConduit'Underscorecommittee'Underscoreid :: Text -- ^
  , scheduleAEfileConduit'Underscorecommittee'Underscorename :: Text -- ^
  , scheduleAEfileConduit'Underscorecommittee'Underscorestate :: Text -- ^
  , scheduleAEfileConduit'Underscorecommittee'Underscorestreet1 :: Text -- ^
  , scheduleAEfileConduit'Underscorecommittee'Underscorestreet2 :: Text -- ^
  , scheduleAEfileConduit'Underscorecommittee'Underscorezip :: Int -- ^
  , scheduleAEfileContribution'Underscorereceipt'Underscoreamount :: Double -- ^
  , scheduleAEfileContribution'Underscorereceipt'Underscoredate :: Date -- ^
  , scheduleAEfileContributor'Underscoreaggregate'Underscoreytd :: Double -- ^
  , scheduleAEfileContributor'Underscorecity :: Text -- ^ City of contributor
  , scheduleAEfileContributor'Underscoreemployer :: Text -- ^ Employer of contributor, filers need to make an effort to gather this information
  , scheduleAEfileContributor'Underscorefirst'Underscorename :: Text -- ^
  , scheduleAEfileContributor'Underscorelast'Underscorename :: Text -- ^
  , scheduleAEfileContributor'Underscoremiddle'Underscorename :: Text -- ^
  , scheduleAEfileContributor'Underscorename :: Text -- ^
  , scheduleAEfileContributor'Underscoreoccupation :: Text -- ^ Occupation of contributor, filers need to make an effort to gather this information
  , scheduleAEfileContributor'Underscoreprefix :: Text -- ^
  , scheduleAEfileContributor'Underscorestate :: Text -- ^ State of contributor
  , scheduleAEfileContributor'Underscoresuffix :: Text -- ^
  , scheduleAEfileContributor'Underscorezip :: Text -- ^ Zip code of contributor
  , scheduleAEfileCsv'Underscoreurl :: Text -- ^
  , scheduleAEfileCycle :: Int -- ^
  , scheduleAEfileEntity'Underscoretype :: Text -- ^
  , scheduleAEfileFec'Underscoreelection'Underscoretype'Underscoredesc :: Text -- ^
  , scheduleAEfileFec'Underscoreurl :: Text -- ^
  , scheduleAEfileFile'Underscorenumber :: Int -- ^
  , scheduleAEfileFiling :: EFilings -- ^
  , scheduleAEfileImage'Underscorenumber :: Text -- ^ An unique identifier for each page the electronic or paper report.
  , scheduleAEfileLine'Underscorenumber :: Text -- ^
  , scheduleAEfileLoad'Underscoretimestamp :: Integer -- ^
  , scheduleAEfileMemo'Underscorecode :: Text -- ^
  , scheduleAEfileMemo'Underscoretext :: Text -- ^
  , scheduleAEfilePdf'Underscoreurl :: Text -- ^
  , scheduleAEfilePgo :: Text -- ^
  , scheduleAEfileRelated'Underscoreline'Underscorenumber :: Int -- ^
  , scheduleAEfileReport'Underscoretype :: Text -- ^
  , scheduleAEfileTransaction'Underscoreid :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAEfile where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAEfile")
instance ToJSON ScheduleAEfile where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAEfile")

-- |
data ScheduleAEfilePage = ScheduleAEfilePage
  { scheduleAEfilePagePagination :: OffsetInfo -- ^
  , scheduleAEfilePageResults    :: [ScheduleAEfile] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAEfilePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAEfilePage")
instance ToJSON ScheduleAEfilePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAEfilePage")

-- |
data ScheduleAPage = ScheduleAPage
  { scheduleAPagePagination :: SeekInfo -- ^
  , scheduleAPageResults    :: [ScheduleA] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAPage")
instance ToJSON ScheduleAPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAPage")

-- |
data ScheduleB = ScheduleB
  { scheduleBAmendment'Underscoreindicator :: Text -- ^
  , scheduleBAmendment'Underscoreindicator'Underscoredesc :: Text -- ^
  , scheduleBBack'Underscorereference'Underscoreschedule'Underscoreid :: Text -- ^
  , scheduleBBack'Underscorereference'Underscoretransaction'Underscoreid :: Text -- ^
  , scheduleBBeneficiary'Underscorecommittee'Underscorename :: Text -- ^
  , scheduleBCandidate'Underscorefirst'Underscorename :: Text -- ^
  , scheduleBCandidate'Underscoreid :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , scheduleBCandidate'Underscorelast'Underscorename :: Text -- ^
  , scheduleBCandidate'Underscoremiddle'Underscorename :: Text -- ^
  , scheduleBCandidate'Underscorename :: Text -- ^ Name of candidate running for office
  , scheduleBCandidate'Underscoreoffice :: Text -- ^
  , scheduleBCandidate'Underscoreoffice'Underscoredescription :: Text -- ^
  , scheduleBCandidate'Underscoreoffice'Underscoredistrict :: Text -- ^
  , scheduleBCandidate'Underscoreoffice'Underscorestate :: Text -- ^
  , scheduleBCandidate'Underscoreoffice'Underscorestate'Underscorefull :: Text -- ^
  , scheduleBCandidate'Underscoreprefix :: Text -- ^
  , scheduleBCandidate'Underscoresuffix :: Text -- ^
  , scheduleBCategory'Underscorecode :: Text -- ^
  , scheduleBCategory'Underscorecode'Underscorefull :: Text -- ^
  , scheduleBComm'Underscoredt :: Date -- ^
  , scheduleBCommittee :: CommitteeHistory -- ^
  , scheduleBCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleBConduit'Underscorecommittee'Underscorecity :: Text -- ^
  , scheduleBConduit'Underscorecommittee'Underscorename :: Text -- ^
  , scheduleBConduit'Underscorecommittee'Underscorestate :: Text -- ^
  , scheduleBConduit'Underscorecommittee'Underscorestreet1 :: Text -- ^
  , scheduleBConduit'Underscorecommittee'Underscorestreet2 :: Text -- ^
  , scheduleBConduit'Underscorecommittee'Underscorezip :: Int -- ^
  , scheduleBDisbursement'Underscoreamount :: Double -- ^
  , scheduleBDisbursement'Underscoredate :: Date -- ^
  , scheduleBDisbursement'Underscoredescription :: Text -- ^
  , scheduleBDisbursement'Underscorepurpose'Underscorecategory :: Text -- ^
  , scheduleBDisbursement'Underscoretype :: Text -- ^
  , scheduleBDisbursement'Underscoretype'Underscoredescription :: Text -- ^
  , scheduleBElection'Underscoretype :: Text -- ^
  , scheduleBElection'Underscoretype'Underscorefull :: Text -- ^
  , scheduleBEntity'Underscoretype :: Text -- ^
  , scheduleBEntity'Underscoretype'Underscoredesc :: Text -- ^
  , scheduleBFec'Underscoreelection'Underscoretype'Underscoredesc :: Text -- ^
  , scheduleBFec'Underscoreelection'Underscoreyear :: Text -- ^
  , scheduleBFile'Underscorenumber :: Int -- ^
  , scheduleBFiling'Underscoreform :: Text -- ^
  , scheduleBImage'Underscorenumber :: Text -- ^
  , scheduleBLine'Underscorenumber :: Text -- ^
  , scheduleBLine'Underscorenumber'Underscorelabel :: Text -- ^
  , scheduleBLink'Underscoreid :: Int -- ^
  , scheduleBLoad'Underscoredate :: Integer -- ^
  , scheduleBMemo'Underscorecode :: Text -- ^
  , scheduleBMemo'Underscorecode'Underscorefull :: Text -- ^
  , scheduleBMemo'Underscoretext :: Text -- ^
  , scheduleBMemoed'Underscoresubtotal :: Bool -- ^
  , scheduleBNational'Underscorecommittee'Underscorenonfederal'Underscoreaccount :: Text -- ^
  , scheduleBOriginal'Underscoresub'Underscoreid :: Text -- ^
  , scheduleBPayee'Underscoreemployer :: Text -- ^
  , scheduleBPayee'Underscorefirst'Underscorename :: Text -- ^
  , scheduleBPayee'Underscorelast'Underscorename :: Text -- ^
  , scheduleBPayee'Underscoremiddle'Underscorename :: Text -- ^
  , scheduleBPayee'Underscoreoccupation :: Text -- ^
  , scheduleBPayee'Underscoreprefix :: Text -- ^
  , scheduleBPayee'Underscoresuffix :: Text -- ^
  , scheduleBPdf'Underscoreurl :: Text -- ^
  , scheduleBRecipient'Underscorecity :: Text -- ^
  , scheduleBRecipient'Underscorecommittee :: CommitteeHistory -- ^
  , scheduleBRecipient'Underscorecommittee'Underscoreid :: Text -- ^
  , scheduleBRecipient'Underscorename :: Text -- ^
  , scheduleBRecipient'Underscorestate :: Text -- ^
  , scheduleBRecipient'Underscorezip :: Text -- ^
  , scheduleBRef'Underscoredisp'Underscoreexcess'Underscoreflg :: Text -- ^
  , scheduleBReport'Underscoretype :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , scheduleBReport'Underscoreyear :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , scheduleBSchedule'Underscoretype :: Text -- ^
  , scheduleBSchedule'Underscoretype'Underscorefull :: Text -- ^
  , scheduleBSemi'Underscoreannual'Underscorebundled'Underscorerefund :: Double -- ^
  , scheduleBSpender'Underscorecommittee'Underscoretype :: Text -- ^
  , scheduleBSub'Underscoreid :: Text -- ^
  , scheduleBTransaction'Underscoreid :: Text -- ^
  , scheduleBTwo'Underscoreyear'Underscoretransaction'Underscoreperiod :: Int -- ^  This is a two-year period that is derived from the year a transaction took place in the Itemized Schedule A and Schedule B tables. In cases where we have the date of the transaction (contribution_receipt_date in schedules/schedule_a, disbursement_date in schedules/schedule_b) the two_year_transaction_period is named after the ending, even-numbered year. If we do not have the date  of the transaction, we fall back to using the report year (report_year in both tables) instead,  making the same cycle adjustment as necessary. If no transaction year is specified, the results default to the most current cycle.
  , scheduleBUnused'Underscorerecipient'Underscorecommittee'Underscoreid :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleB where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleB")
instance ToJSON ScheduleB where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleB")

-- |
data ScheduleBByPurpose = ScheduleBByPurpose
  { scheduleBByPurposeCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleBByPurposeCount                  :: Int -- ^ Number of records making up the total
  , scheduleBByPurposeCycle                  :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleBByPurposePurpose                :: Text -- ^ Purpose of the expenditure
  , scheduleBByPurposeTotal                  :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleBByPurpose where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleBByPurpose")
instance ToJSON ScheduleBByPurpose where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleBByPurpose")

-- |
data ScheduleBByPurposePage = ScheduleBByPurposePage
  { scheduleBByPurposePagePagination :: OffsetInfo -- ^
  , scheduleBByPurposePageResults    :: [ScheduleBByPurpose] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleBByPurposePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleBByPurposePage")
instance ToJSON ScheduleBByPurposePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleBByPurposePage")

-- |
data ScheduleBByRecipient = ScheduleBByRecipient
  { scheduleBByRecipientCommittee'Underscoreid   :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleBByRecipientCount                    :: Int -- ^ Number of records making up the total
  , scheduleBByRecipientCycle                    :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleBByRecipientRecipient'Underscorename :: Text -- ^ Name of the entity receiving the disbursement
  , scheduleBByRecipientTotal                    :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleBByRecipient where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleBByRecipient")
instance ToJSON ScheduleBByRecipient where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleBByRecipient")

-- |
data ScheduleBByRecipientID = ScheduleBByRecipientID
  { scheduleBByRecipientIDCommittee'Underscoreid   :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleBByRecipientIDCommittee'Underscorename :: Text -- ^
  , scheduleBByRecipientIDCount                    :: Int -- ^ Number of records making up the total
  , scheduleBByRecipientIDCycle                    :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleBByRecipientIDIdx                      :: Int -- ^
  , scheduleBByRecipientIDRecipient'Underscoreid   :: Text -- ^ The FEC identifier should be represented here if the entity receiving the disbursement is registered with the FEC.
  , scheduleBByRecipientIDRecipient'Underscorename :: Text -- ^
  , scheduleBByRecipientIDTotal                    :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleBByRecipientID where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleBByRecipientID")
instance ToJSON ScheduleBByRecipientID where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleBByRecipientID")

-- |
data ScheduleBByRecipientIDPage = ScheduleBByRecipientIDPage
  { scheduleBByRecipientIDPagePagination :: OffsetInfo -- ^
  , scheduleBByRecipientIDPageResults    :: [ScheduleBByRecipientID] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleBByRecipientIDPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleBByRecipientIDPage")
instance ToJSON ScheduleBByRecipientIDPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleBByRecipientIDPage")

-- |
data ScheduleBByRecipientPage = ScheduleBByRecipientPage
  { scheduleBByRecipientPagePagination :: OffsetInfo -- ^
  , scheduleBByRecipientPageResults    :: [ScheduleBByRecipient] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleBByRecipientPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleBByRecipientPage")
instance ToJSON ScheduleBByRecipientPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleBByRecipientPage")

-- |
data ScheduleBEfile = ScheduleBEfile
  { scheduleBEfileAmendment'Underscoreindicator :: Text -- ^
  , scheduleBEfileBack'Underscorereference'Underscoreschedule'Underscorename :: Text -- ^
  , scheduleBEfileBack'Underscorereference'Underscoretransaction'Underscoreid :: Text -- ^
  , scheduleBEfileBeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , scheduleBEfileBeneficiary'Underscorecommittee'Underscorename :: Text -- ^
  , scheduleBEfileCandidate'Underscoreoffice :: Text -- ^
  , scheduleBEfileCandidate'Underscoreoffice'Underscoredistrict :: Text -- ^
  , scheduleBEfileCommittee :: CommitteeHistory -- ^
  , scheduleBEfileCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleBEfileCsv'Underscoreurl :: Text -- ^
  , scheduleBEfileDisbursement'Underscoreamount :: Double -- ^
  , scheduleBEfileDisbursement'Underscoredate :: Date -- ^
  , scheduleBEfileDisbursement'Underscoredescription :: Text -- ^
  , scheduleBEfileDisbursement'Underscoretype :: Text -- ^
  , scheduleBEfileEntity'Underscoretype :: Text -- ^
  , scheduleBEfileFec'Underscoreurl :: Text -- ^
  , scheduleBEfileFile'Underscorenumber :: Int -- ^
  , scheduleBEfileFiling :: EFilings -- ^
  , scheduleBEfileImage'Underscorenumber :: Text -- ^ An unique identifier for each page the electronic or paper report.
  , scheduleBEfileIs'Underscorenotice :: Bool -- ^
  , scheduleBEfileLine'Underscorenumber :: Text -- ^
  , scheduleBEfileLoad'Underscoretimestamp :: Integer -- ^
  , scheduleBEfileMemo'Underscorecode :: Text -- ^
  , scheduleBEfileMemo'Underscoretext :: Text -- ^
  , scheduleBEfilePayee'Underscorename :: Text -- ^
  , scheduleBEfilePdf'Underscoreurl :: Text -- ^
  , scheduleBEfileRecipient'Underscorecity :: Text -- ^
  , scheduleBEfileRecipient'Underscorename :: Text -- ^
  , scheduleBEfileRecipient'Underscoreprefix :: Text -- ^
  , scheduleBEfileRecipient'Underscorestate :: Text -- ^
  , scheduleBEfileRecipient'Underscoresuffix :: Text -- ^
  , scheduleBEfileRecipient'Underscorezip :: Text -- ^
  , scheduleBEfileRelated'Underscoreline'Underscorenumber :: Int -- ^
  , scheduleBEfileReport'Underscoretype :: Text -- ^
  , scheduleBEfileSemi'Underscoreannual'Underscorebundled'Underscorerefund :: Int -- ^
  , scheduleBEfileTransaction'Underscoreid :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleBEfile where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleBEfile")
instance ToJSON ScheduleBEfile where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleBEfile")

-- |
data ScheduleBEfilePage = ScheduleBEfilePage
  { scheduleBEfilePagePagination :: OffsetInfo -- ^
  , scheduleBEfilePageResults    :: [ScheduleBEfile] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleBEfilePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleBEfilePage")
instance ToJSON ScheduleBEfilePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleBEfilePage")

-- |
data ScheduleBPage = ScheduleBPage
  { scheduleBPagePagination :: SeekInfo -- ^
  , scheduleBPageResults    :: [ScheduleB] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleBPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleBPage")
instance ToJSON ScheduleBPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleBPage")

-- |
data ScheduleE = ScheduleE
  { scheduleEAction'Underscorecode :: Text -- ^
  , scheduleEAction'Underscorecode'Underscorefull :: Text -- ^
  , scheduleEAmendment'Underscoreindicator :: Text -- ^      -N   new     -A   amendment     -T   terminated     -C   consolidated     -M   multi-candidate     -S   secondary      Null might be new or amendment. If amendment indicator is null and the filings is the first or first in a chain treat it as if it was a new. If it is not the first or first in a chain then treat the filing as an amendment.
  , scheduleEAmendment'Underscorenumber :: Int -- ^  Number of times the report has been amended.
  , scheduleEBack'Underscorereference'Underscoreschedule'Underscorename :: Text -- ^
  , scheduleEBack'Underscorereference'Underscoretransaction'Underscoreid :: Text -- ^
  , scheduleECandidate :: Text -- ^
  , scheduleECandidate'Underscorefirst'Underscorename :: Text -- ^
  , scheduleECandidate'Underscoreid :: Text -- ^
  , scheduleECandidate'Underscorelast'Underscorename :: Text -- ^
  , scheduleECandidate'Underscoremiddle'Underscorename :: Text -- ^
  , scheduleECandidate'Underscorename :: Text -- ^ Name of candidate running for office
  , scheduleECandidate'Underscoreoffice :: Text -- ^ Federal office candidate runs for: H, S or P
  , scheduleECandidate'Underscoreoffice'Underscoredistrict :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , scheduleECandidate'Underscoreoffice'Underscorestate :: Text -- ^ US state or territory
  , scheduleECandidate'Underscoreparty :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , scheduleECandidate'Underscoreprefix :: Text -- ^
  , scheduleECandidate'Underscoresuffix :: Text -- ^
  , scheduleECategory'Underscorecode :: Text -- ^
  , scheduleECategory'Underscorecode'Underscorefull :: Text -- ^
  , scheduleECommittee :: CommitteeHistory -- ^
  , scheduleECommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleEConduit'Underscorecommittee'Underscorecity :: Text -- ^
  , scheduleEConduit'Underscorecommittee'Underscoreid :: Text -- ^
  , scheduleEConduit'Underscorecommittee'Underscorename :: Text -- ^
  , scheduleEConduit'Underscorecommittee'Underscorestate :: Text -- ^
  , scheduleEConduit'Underscorecommittee'Underscorestreet1 :: Text -- ^
  , scheduleEConduit'Underscorecommittee'Underscorestreet2 :: Text -- ^
  , scheduleEConduit'Underscorecommittee'Underscorezip :: Int -- ^
  , scheduleEDissemination'Underscoredate :: Date -- ^
  , scheduleEElection'Underscoretype :: Text -- ^ Election type  Convention, Primary, General, Special, Runoff etc.
  , scheduleEElection'Underscoretype'Underscorefull :: Text -- ^ Election type  Convention, Primary, General, Special, Runoff etc.
  , scheduleEExpenditure'Underscoreamount :: Double -- ^
  , scheduleEExpenditure'Underscoredate :: Date -- ^
  , scheduleEExpenditure'Underscoredescription :: Text -- ^
  , scheduleEFile'Underscorenumber :: Int -- ^
  , scheduleEFiler'Underscorefirst'Underscorename :: Text -- ^
  , scheduleEFiler'Underscorelast'Underscorename :: Text -- ^
  , scheduleEFiler'Underscoremiddle'Underscorename :: Text -- ^
  , scheduleEFiler'Underscoreprefix :: Text -- ^
  , scheduleEFiler'Underscoresuffix :: Text -- ^
  , scheduleEFiling'Underscoreform :: Text -- ^
  , scheduleEImage'Underscorenumber :: Text -- ^
  , scheduleEIndependent'Underscoresign'Underscoredate :: Date -- ^
  , scheduleEIndependent'Underscoresign'Underscorename :: Text -- ^
  , scheduleEIs'Underscorenotice :: Bool -- ^
  , scheduleELine'Underscorenumber :: Text -- ^
  , scheduleELink'Underscoreid :: Int -- ^
  , scheduleEMemo'Underscorecode :: Text -- ^
  , scheduleEMemo'Underscorecode'Underscorefull :: Text -- ^
  , scheduleEMemo'Underscoretext :: Text -- ^
  , scheduleEMemoed'Underscoresubtotal :: Bool -- ^
  , scheduleENotary'Underscorecommission'Underscoreexpiration'Underscoredate :: Date -- ^
  , scheduleENotary'Underscoresign'Underscoredate :: Date -- ^
  , scheduleENotary'Underscoresign'Underscorename :: Text -- ^
  , scheduleEOffice'Underscoretotal'Underscoreytd :: Double -- ^
  , scheduleEOriginal'Underscoresub'Underscoreid :: Text -- ^
  , scheduleEPayee'Underscorecity :: Text -- ^
  , scheduleEPayee'Underscorefirst'Underscorename :: Text -- ^
  , scheduleEPayee'Underscorelast'Underscorename :: Text -- ^
  , scheduleEPayee'Underscoremiddle'Underscorename :: Text -- ^
  , scheduleEPayee'Underscorename :: Text -- ^
  , scheduleEPayee'Underscoreprefix :: Text -- ^
  , scheduleEPayee'Underscorestate :: Text -- ^
  , scheduleEPayee'Underscorestreet'Underscore1 :: Text -- ^
  , scheduleEPayee'Underscorestreet'Underscore2 :: Text -- ^
  , scheduleEPayee'Underscoresuffix :: Text -- ^
  , scheduleEPayee'Underscorezip :: Text -- ^
  , scheduleEPdf'Underscoreurl :: Text -- ^
  , scheduleEPrevious'Underscorefile'Underscorenumber :: Int -- ^
  , scheduleEReport'Underscoretype :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , scheduleEReport'Underscoreyear :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , scheduleESchedule'Underscoretype :: Text -- ^
  , scheduleESchedule'Underscoretype'Underscorefull :: Text -- ^
  , scheduleESub'Underscoreid :: Text -- ^
  , scheduleESupport'Underscoreoppose'Underscoreindicator :: Text -- ^
  , scheduleETransaction'Underscoreid :: Int -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleE where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleE")
instance ToJSON ScheduleE where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleE")

-- |
data ScheduleEByCandidate = ScheduleEByCandidate
  { scheduleEByCandidateCandidate'Underscoreid                       :: Text -- ^
  , scheduleEByCandidateCandidate'Underscorename                     :: Text -- ^
  , scheduleEByCandidateCommittee'Underscoreid                       :: Text -- ^
  , scheduleEByCandidateCommittee'Underscorename                     :: Text -- ^
  , scheduleEByCandidateCount                                        :: Int -- ^ Number of records making up the total
  , scheduleEByCandidateCycle                                        :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleEByCandidateSupport'Underscoreoppose'Underscoreindicator :: Text -- ^ Explains if the money was spent in order to support or oppose a candidate or candidates. (Coded S or O for support or oppose.) This indicator applies to independent expenditures and communication costs.
  , scheduleEByCandidateTotal                                        :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleEByCandidate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleEByCandidate")
instance ToJSON ScheduleEByCandidate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleEByCandidate")

-- |
data ScheduleEByCandidatePage = ScheduleEByCandidatePage
  { scheduleEByCandidatePagePagination :: OffsetInfo -- ^
  , scheduleEByCandidatePageResults    :: [ScheduleEByCandidate] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleEByCandidatePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleEByCandidatePage")
instance ToJSON ScheduleEByCandidatePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleEByCandidatePage")

-- |
data ScheduleEEfile = ScheduleEEfile
  { scheduleEEfileAmendment'Underscoreindicator :: Text -- ^
  , scheduleEEfileBack'Underscorereference'Underscoreschedule'Underscorename :: Text -- ^
  , scheduleEEfileBack'Underscorereference'Underscoretransaction'Underscoreid :: Text -- ^
  , scheduleEEfileBeginning'Underscoreimage'Underscorenumber :: Text -- ^
  , scheduleEEfileCand'Underscoreoffice'Underscoredistrict :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , scheduleEEfileCand'Underscoreoffice'Underscorestate :: Text -- ^ US state or territory
  , scheduleEEfileCandidate'Underscorefirst'Underscorename :: Text -- ^
  , scheduleEEfileCandidate'Underscoreid :: Text -- ^
  , scheduleEEfileCandidate'Underscoremiddle'Underscorename :: Text -- ^
  , scheduleEEfileCandidate'Underscorename :: Text -- ^ Name of candidate running for office
  , scheduleEEfileCandidate'Underscoreoffice :: Text -- ^ Federal office candidate runs for: H, S or P
  , scheduleEEfileCandidate'Underscoreprefix :: Text -- ^
  , scheduleEEfileCandidate'Underscoresuffix :: Text -- ^
  , scheduleEEfileCategory'Underscorecode :: Text -- ^
  , scheduleEEfileCommittee :: CommitteeHistory -- ^
  , scheduleEEfileCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleEEfileCsv'Underscoreurl :: Text -- ^
  , scheduleEEfileDissemination'Underscoredate :: Date -- ^
  , scheduleEEfileEntity'Underscoretype :: Text -- ^
  , scheduleEEfileExpenditure'Underscoreamount :: Int -- ^
  , scheduleEEfileExpenditure'Underscoredate :: Date -- ^
  , scheduleEEfileExpenditure'Underscoredescription :: Text -- ^
  , scheduleEEfileFec'Underscoreurl :: Text -- ^
  , scheduleEEfileFile'Underscorenumber :: Int -- ^
  , scheduleEEfileFiler'Underscorefirst'Underscorename :: Text -- ^
  , scheduleEEfileFiler'Underscorelast'Underscorename :: Text -- ^
  , scheduleEEfileFiler'Underscoremiddle'Underscorename :: Text -- ^
  , scheduleEEfileFiler'Underscoreprefix :: Text -- ^
  , scheduleEEfileFiler'Underscoresuffix :: Text -- ^
  , scheduleEEfileFiling :: EFilings -- ^
  , scheduleEEfileImage'Underscorenumber :: Text -- ^ An unique identifier for each page the electronic or paper report.
  , scheduleEEfileIs'Underscorenotice :: Bool -- ^
  , scheduleEEfileLine'Underscorenumber :: Text -- ^
  , scheduleEEfileLoad'Underscoretimestamp :: Integer -- ^
  , scheduleEEfileMemo'Underscorecode :: Text -- ^
  , scheduleEEfileMemo'Underscoretext :: Text -- ^
  , scheduleEEfileNotary'Underscoresign'Underscoredate :: Date -- ^
  , scheduleEEfileOffice'Underscoretotal'Underscoreytd :: Float -- ^
  , scheduleEEfilePayee'Underscorecity :: Text -- ^
  , scheduleEEfilePayee'Underscorefirst'Underscorename :: Text -- ^
  , scheduleEEfilePayee'Underscorelast'Underscorename :: Text -- ^
  , scheduleEEfilePayee'Underscoremiddle'Underscorename :: Text -- ^
  , scheduleEEfilePayee'Underscorename :: Text -- ^
  , scheduleEEfilePayee'Underscoreprefix :: Text -- ^
  , scheduleEEfilePayee'Underscorestate :: Text -- ^
  , scheduleEEfilePayee'Underscorestreet'Underscore1 :: Text -- ^
  , scheduleEEfilePayee'Underscorestreet'Underscore2 :: Text -- ^
  , scheduleEEfilePayee'Underscoresuffix :: Text -- ^
  , scheduleEEfilePayee'Underscorezip :: Text -- ^
  , scheduleEEfilePdf'Underscoreurl :: Text -- ^
  , scheduleEEfileRelated'Underscoreline'Underscorenumber :: Int -- ^
  , scheduleEEfileReport'Underscoretype :: Text -- ^
  , scheduleEEfileSupport'Underscoreoppose'Underscoreindicator :: Text -- ^
  , scheduleEEfileTransaction'Underscoreid :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleEEfile where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleEEfile")
instance ToJSON ScheduleEEfile where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleEEfile")

-- |
data ScheduleEEfilePage = ScheduleEEfilePage
  { scheduleEEfilePagePagination :: OffsetInfo -- ^
  , scheduleEEfilePageResults    :: [ScheduleEEfile] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleEEfilePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleEEfilePage")
instance ToJSON ScheduleEEfilePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleEEfilePage")

-- |
data ScheduleEPage = ScheduleEPage
  { scheduleEPagePagination :: SeekInfo -- ^
  , scheduleEPageResults    :: [ScheduleE] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleEPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleEPage")
instance ToJSON ScheduleEPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleEPage")

-- |
data SeekInfo = SeekInfo
  { seekInfoCount                  :: Int -- ^
  , seekInfoLast'Underscoreindexes :: Text -- ^
  , seekInfoPages                  :: Int -- ^
  , seekInfoPer'Underscorepage     :: Int -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON SeekInfo where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "seekInfo")
instance ToJSON SeekInfo where
  toJSON = genericToJSON (removeFieldLabelPrefix False "seekInfo")

-- |
data StateElectionOfficeInfo = StateElectionOfficeInfo
  { stateElectionOfficeInfoAddress'Underscoreline1                    :: Text -- ^
  , stateElectionOfficeInfoAddress'Underscoreline2                    :: Text -- ^
  , stateElectionOfficeInfoCity                                       :: Text -- ^
  , stateElectionOfficeInfoEmail                                      :: Text -- ^
  , stateElectionOfficeInfoFax'Underscorenumber                       :: Text -- ^
  , stateElectionOfficeInfoMailing'Underscoreaddress1                 :: Text -- ^
  , stateElectionOfficeInfoMailing'Underscoreaddress2                 :: Text -- ^
  , stateElectionOfficeInfoMailing'Underscorecity                     :: Text -- ^
  , stateElectionOfficeInfoMailing'Underscorestate                    :: Text -- ^
  , stateElectionOfficeInfoMailing'Underscorezipcode                  :: Text -- ^
  , stateElectionOfficeInfoOffice'Underscorename                      :: Text -- ^
  , stateElectionOfficeInfoOffice'Underscoretype                      :: Text -- ^
  , stateElectionOfficeInfoPrimary'Underscorephone'Underscorenumber   :: Text -- ^
  , stateElectionOfficeInfoSecondary'Underscorephone'Underscorenumber :: Text -- ^
  , stateElectionOfficeInfoState                                      :: Text -- ^
  , stateElectionOfficeInfoState'Underscorefull'Underscorename        :: Text -- ^
  , stateElectionOfficeInfoWebsite'Underscoreurl1                     :: Text -- ^
  , stateElectionOfficeInfoWebsite'Underscoreurl2                     :: Text -- ^
  , stateElectionOfficeInfoZip'Underscorecode                         :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON StateElectionOfficeInfo where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "stateElectionOfficeInfo")
instance ToJSON StateElectionOfficeInfo where
  toJSON = genericToJSON (removeFieldLabelPrefix False "stateElectionOfficeInfo")

-- |
data StateElectionOfficeInfoPage = StateElectionOfficeInfoPage
  { stateElectionOfficeInfoPagePagination :: OffsetInfo -- ^
  , stateElectionOfficeInfoPageResults    :: [StateElectionOfficeInfo] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON StateElectionOfficeInfoPage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "stateElectionOfficeInfoPage")
instance ToJSON StateElectionOfficeInfoPage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "stateElectionOfficeInfoPage")

-- |
data TotalsCommittee = TotalsCommittee
  { totalsCommitteeCandidate'Underscoreids :: [Text] -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , totalsCommitteeCash'Underscoreon'Underscorehand'Underscoreend'Underscoreperiod :: Double -- ^
  , totalsCommitteeCity :: Text -- ^ City of committee as reported on the Form 1
  , totalsCommitteeCommittee'Underscoreid :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , totalsCommitteeCommittee'Underscoretype :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , totalsCommitteeCommittee'Underscoretype'Underscorefull :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , totalsCommitteeCycle :: Int -- ^  A two year election cycle that the committee was active- (after original registration date but before expiration date in FEC Form 1s) The cycle begins with an odd year and is named for its ending, even year.
  , totalsCommitteeCycles :: [Int] -- ^  A two year election cycle that the committee was active- (after original registration date but before expiration date in FEC Form 1s) The cycle begins with an odd year and is named for its ending, even year.
  , totalsCommitteeDebts'Underscoreowed'Underscoreby'Underscorecommittee :: Double -- ^
  , totalsCommitteeDesignation :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , totalsCommitteeDesignation'Underscorefull :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , totalsCommitteeDisbursements :: Double -- ^
  , totalsCommitteeFiling'Underscorefrequency :: Text -- ^ The one-letter      code of the filing frequency:          - A Administratively terminated          - D Debt          - M Monthly filer          - Q Quarterly filer          - T Terminated          - W Waived
  , totalsCommitteeIndependent'Underscoreexpenditures :: Double -- ^
  , totalsCommitteeName :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , totalsCommitteeOrganization'Underscoretype :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , totalsCommitteeOrganization'Underscoretype'Underscorefull :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , totalsCommitteeParty :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , totalsCommitteeParty'Underscorefull :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , totalsCommitteeReceipts :: Double -- ^
  , totalsCommitteeState :: Text -- ^ State of the committee's address as filed on the Form 1
  , totalsCommitteeState'Underscorefull :: Text -- ^ State of committee as reported on the Form 1
  , totalsCommitteeStreet'Underscore1 :: Text -- ^ Street address of committee as reported on the Form 1
  , totalsCommitteeStreet'Underscore2 :: Text -- ^ Second line of street address of committee as reported on the Form 1
  , totalsCommitteeTreasurer'Underscorename :: Text -- ^ Name of the Committee's treasurer. If multiple treasurers for the committee, the most recent treasurer will be shown.
  , totalsCommitteeZip :: Text -- ^ Zip code of committee as reported on the Form 1
  } deriving (Show, Eq, Generic)

instance FromJSON TotalsCommittee where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "totalsCommittee")
instance ToJSON TotalsCommittee where
  toJSON = genericToJSON (removeFieldLabelPrefix False "totalsCommittee")

-- |
data TotalsCommitteePage = TotalsCommitteePage
  { totalsCommitteePagePagination :: OffsetInfo -- ^
  , totalsCommitteePageResults    :: [TotalsCommittee] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON TotalsCommitteePage where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "totalsCommitteePage")
instance ToJSON TotalsCommitteePage where
  toJSON = genericToJSON (removeFieldLabelPrefix False "totalsCommitteePage")

-- Remove a field label prefix during JSON parsing.
-- Also perform any replacements for special characters.
removeFieldLabelPrefix :: Bool -> String -> Options
removeFieldLabelPrefix forParsing prefix =
  defaultOptions
  {fieldLabelModifier = fromMaybe (error ("did not find prefix " ++ prefix)) . stripPrefix prefix . replaceSpecialChars}
  where
    replaceSpecialChars field = foldl (&) field (map mkCharReplacement specialChars)
    specialChars =
      [ ("@", "'At")
      , ("\\", "'Back_Slash")
      , ("<=", "'Less_Than_Or_Equal_To")
      , ("\"", "'Double_Quote")
      , ("[", "'Left_Square_Bracket")
      , ("]", "'Right_Square_Bracket")
      , ("^", "'Caret")
      , ("_", "'Underscore")
      , ("`", "'Backtick")
      , ("!", "'Exclamation")
      , ("#", "'Hash")
      , ("$", "'Dollar")
      , ("%", "'Percent")
      , ("&", "'Ampersand")
      , ("'", "'Quote")
      , ("(", "'Left_Parenthesis")
      , (")", "'Right_Parenthesis")
      , ("*", "'Star")
      , ("+", "'Plus")
      , (",", "'Comma")
      , ("-", "'Dash")
      , (".", "'Period")
      , ("/", "'Slash")
      , (":", "'Colon")
      , ("{", "'Left_Curly_Bracket")
      , ("|", "'Pipe")
      , ("<", "'LessThan")
      , ("!=", "'Not_Equal")
      , ("=", "'Equal")
      , ("}", "'Right_Curly_Bracket")
      , (">", "'GreaterThan")
      , ("~", "'Tilde")
      , ("?", "'Question_Mark")
      , (">=", "'Greater_Than_Or_Equal_To")
      ]
    mkCharReplacement (replaceStr, searchStr) = T.unpack . replacer (T.pack searchStr) (T.pack replaceStr) . T.pack
    replacer =
      if forParsing
        then flip T.replace
        else T.replace
