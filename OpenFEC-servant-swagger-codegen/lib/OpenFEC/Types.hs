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
  { auditCaseAudit_case_id         :: Text -- ^
  , auditCaseAudit_id              :: Int -- ^
  , auditCaseCandidate_id          :: Text -- ^
  , auditCaseCandidate_name        :: Text -- ^
  , auditCaseCommittee_description :: Text -- ^
  , auditCaseCommittee_designation :: Text -- ^
  , auditCaseCommittee_id          :: Text -- ^
  , auditCaseCommittee_name        :: Text -- ^
  , auditCaseCommittee_type        :: Text -- ^
  , auditCaseCycle                 :: Int -- ^
  , auditCaseFar_release_date      :: Date -- ^
  , auditCaseLink_to_report        :: Text -- ^  URL for retrieving the PDF document
  , auditCasePrimary_category_list :: [AuditCaseCategoryRelation] -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON AuditCase where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "auditCase")
instance ToJSON AuditCase where
  toJSON = genericToJSON (removeFieldLabelPrefix False "auditCase")

-- |
data AuditCaseCategoryRelation = AuditCaseCategoryRelation
  { auditCaseCategoryRelationPrimary_category_id   :: Text -- ^
  , auditCaseCategoryRelationPrimary_category_name :: Text -- ^
  , auditCaseCategoryRelationSub_category_list     :: [AuditCaseSubCategory] -- ^
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
  { auditCaseSubCategorySub_category_id   :: Text -- ^
  , auditCaseSubCategorySub_category_name :: Text -- ^
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
  { auditCategoryPrimary_category_id   :: Text -- ^
  , auditCategoryPrimary_category_name :: Text -- ^
  , auditCategorySub_category_list     :: [AuditCategoryRelation] -- ^
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
  { auditCategoryRelationSub_category_id   :: Text -- ^
  , auditCategoryRelationSub_category_name :: Text -- ^
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
  { auditPrimaryCategoryPrimary_category_id   :: Text -- ^
  , auditPrimaryCategoryPrimary_category_name :: Text -- ^
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
  { baseF3FilingAmended_address               :: Text -- ^
  , baseF3FilingAmended_by                    :: Int -- ^
  , baseF3FilingAmendment                     :: Text -- ^
  , baseF3FilingAmendment_chain               :: [Int] -- ^
  , baseF3FilingBeginning_image_number        :: Text -- ^
  , baseF3FilingCandidate_first_name          :: Text -- ^
  , baseF3FilingCandidate_id                  :: Text -- ^
  , baseF3FilingCandidate_last_name           :: Text -- ^
  , baseF3FilingCandidate_middle_name         :: Text -- ^
  , baseF3FilingCandidate_name                :: Text -- ^
  , baseF3FilingCandidate_prefix              :: Text -- ^
  , baseF3FilingCandidate_suffix              :: Text -- ^
  , baseF3FilingCash_on_hand_beginning_period :: Int -- ^
  , baseF3FilingCity                          :: Text -- ^
  , baseF3FilingCommittee_id                  :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , baseF3FilingCommittee_name                :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , baseF3FilingCoverage_end_date             :: Date -- ^
  , baseF3FilingCoverage_start_date           :: Date -- ^
  , baseF3FilingCsv_url                       :: Text -- ^
  , baseF3FilingDistrict                      :: Int -- ^
  , baseF3FilingDocument_description          :: Text -- ^
  , baseF3FilingElection_date                 :: Date -- ^
  , baseF3FilingElection_state                :: Text -- ^
  , baseF3FilingF3z1                          :: Int -- ^
  , baseF3FilingFec_file_id                   :: Text -- ^
  , baseF3FilingFec_url                       :: Text -- ^
  , baseF3FilingFile_number                   :: Int -- ^
  , baseF3FilingGeneral_election              :: Text -- ^
  , baseF3FilingIs_amended                    :: Bool -- ^
  , baseF3FilingMost_recent                   :: Bool -- ^
  , baseF3FilingMost_recent_filing            :: Int -- ^
  , baseF3FilingPdf_url                       :: Text -- ^
  , baseF3FilingPrefix                        :: Text -- ^
  , baseF3FilingPrimary_election              :: Text -- ^
  , baseF3FilingReceipt_date                  :: Date -- ^
  , baseF3FilingReport                        :: Text -- ^
  , baseF3FilingReport_type                   :: Text -- ^
  , baseF3FilingReport_year                   :: Int -- ^
  , baseF3FilingRpt_pgi                       :: Text -- ^ Election type  Convention, Primary, General, Special, Runoff etc.
  , baseF3FilingRunoff_election               :: Text -- ^
  , baseF3FilingSign_date                     :: Date -- ^
  , baseF3FilingSpecial_election              :: Text -- ^
  , baseF3FilingState                         :: Text -- ^
  , baseF3FilingStreet_1                      :: Text -- ^
  , baseF3FilingStreet_2                      :: Text -- ^
  , baseF3FilingSuffix                        :: Text -- ^
  , baseF3FilingSummary_lines                 :: Text -- ^
  , baseF3FilingTreasurer_first_name          :: Text -- ^
  , baseF3FilingTreasurer_last_name           :: Text -- ^
  , baseF3FilingTreasurer_middle_name         :: Text -- ^
  , baseF3FilingTreasurer_name                :: Text -- ^
  , baseF3FilingZip                           :: Text -- ^
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
  { baseF3PFilingAmended_by                               :: Int -- ^
  , baseF3PFilingAmendment                                :: Text -- ^
  , baseF3PFilingAmendment_chain                          :: [Int] -- ^
  , baseF3PFilingBeginning_image_number                   :: Text -- ^
  , baseF3PFilingCash_on_hand_beginning_period            :: Float -- ^
  , baseF3PFilingCash_on_hand_end_period                  :: Float -- ^
  , baseF3PFilingCity                                     :: Text -- ^
  , baseF3PFilingCommittee_id                             :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , baseF3PFilingCommittee_name                           :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , baseF3PFilingCoverage_end_date                        :: Date -- ^
  , baseF3PFilingCoverage_start_date                      :: Date -- ^
  , baseF3PFilingCsv_url                                  :: Text -- ^
  , baseF3PFilingDebts_owed_by_committee                  :: Float -- ^
  , baseF3PFilingDebts_owed_to_committee                  :: Float -- ^
  , baseF3PFilingDocument_description                     :: Text -- ^
  , baseF3PFilingElection_date                            :: Date -- ^
  , baseF3PFilingElection_state                           :: Text -- ^
  , baseF3PFilingExpenditure_subject_to_limits            :: Float -- ^
  , baseF3PFilingFec_file_id                              :: Text -- ^
  , baseF3PFilingFec_url                                  :: Text -- ^
  , baseF3PFilingFile_number                              :: Int -- ^
  , baseF3PFilingGeneral_election                         :: Text -- ^
  , baseF3PFilingIs_amended                               :: Bool -- ^
  , baseF3PFilingMost_recent                              :: Bool -- ^
  , baseF3PFilingMost_recent_filing                       :: Int -- ^
  , baseF3PFilingNet_contributions_cycle_to_date          :: Float -- ^
  , baseF3PFilingNet_operating_expenditures_cycle_to_date :: Float -- ^
  , baseF3PFilingPdf_url                                  :: Text -- ^
  , baseF3PFilingPrefix                                   :: Text -- ^
  , baseF3PFilingPrimary_election                         :: Text -- ^
  , baseF3PFilingReceipt_date                             :: Date -- ^
  , baseF3PFilingReport                                   :: Text -- ^
  , baseF3PFilingReport_type                              :: Text -- ^
  , baseF3PFilingReport_year                              :: Int -- ^
  , baseF3PFilingRpt_pgi                                  :: Text -- ^ Election type  Convention, Primary, General, Special, Runoff etc.
  , baseF3PFilingSign_date                                :: Date -- ^
  , baseF3PFilingState                                    :: Text -- ^
  , baseF3PFilingStreet_1                                 :: Text -- ^
  , baseF3PFilingStreet_2                                 :: Text -- ^
  , baseF3PFilingSubtotal_summary_period                  :: Text -- ^
  , baseF3PFilingSuffix                                   :: Text -- ^
  , baseF3PFilingSummary_lines                            :: Text -- ^
  , baseF3PFilingTreasurer_first_name                     :: Text -- ^
  , baseF3PFilingTreasurer_last_name                      :: Text -- ^
  , baseF3PFilingTreasurer_middle_name                    :: Text -- ^
  , baseF3PFilingTreasurer_name                           :: Text -- ^
  , baseF3PFilingZip                                      :: Text -- ^
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
  { baseF3XFilingAmend_address                      :: Text -- ^
  , baseF3XFilingAmended_by                         :: Int -- ^
  , baseF3XFilingAmendment                          :: Text -- ^
  , baseF3XFilingAmendment_chain                    :: [Int] -- ^
  , baseF3XFilingBeginning_image_number             :: Text -- ^
  , baseF3XFilingCity                               :: Text -- ^
  , baseF3XFilingCommittee_id                       :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , baseF3XFilingCommittee_name                     :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , baseF3XFilingCoverage_end_date                  :: Date -- ^
  , baseF3XFilingCoverage_start_date                :: Date -- ^
  , baseF3XFilingCsv_url                            :: Text -- ^
  , baseF3XFilingDocument_description               :: Text -- ^
  , baseF3XFilingElection_date                      :: Date -- ^
  , baseF3XFilingElection_state                     :: Text -- ^
  , baseF3XFilingFec_file_id                        :: Text -- ^
  , baseF3XFilingFec_url                            :: Text -- ^
  , baseF3XFilingFile_number                        :: Int -- ^
  , baseF3XFilingIs_amended                         :: Bool -- ^
  , baseF3XFilingMost_recent                        :: Bool -- ^
  , baseF3XFilingMost_recent_filing                 :: Int -- ^
  , baseF3XFilingPdf_url                            :: Text -- ^
  , baseF3XFilingQualified_multicandidate_committee :: Text -- ^
  , baseF3XFilingReceipt_date                       :: Date -- ^
  , baseF3XFilingReport                             :: Text -- ^
  , baseF3XFilingReport_type                        :: Text -- ^
  , baseF3XFilingReport_year                        :: Int -- ^
  , baseF3XFilingRpt_pgi                            :: Text -- ^ Election type  Convention, Primary, General, Special, Runoff etc.
  , baseF3XFilingSign_date                          :: Date -- ^
  , baseF3XFilingState                              :: Text -- ^
  , baseF3XFilingStreet_1                           :: Text -- ^
  , baseF3XFilingStreet_2                           :: Text -- ^
  , baseF3XFilingSummary_lines                      :: Text -- ^
  , baseF3XFilingZip                                :: Text -- ^
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
  { calendarDateAll_day              :: Bool -- ^
  , calendarDateCalendar_category_id :: Int -- ^  Each type of event has a calendar category with an integer id. Options are: Open Meetings: 32, Executive Sessions: 39, Public Hearings: 40, Conferences: 33, Roundtables: 34, Election Dates: 36, Federal Holidays: 37, FEA Periods: 38, Commission Meetings: 20, Reporting Deadlines: 21, Conferences and Outreach: 22, AOs and Rules: 23, Other: 24, Quarterly: 25, Monthly: 26, Pre and Post-Elections: 27, EC Periods:28, and IE Periods: 29
  , calendarDateCategory             :: Text -- ^  Each type of event has a calendar category with an integer id. Options are: Open Meetings: 32, Executive Sessions: 39, Public Hearings: 40, Conferences: 33, Roundtables: 34, Election Dates: 36, Federal Holidays: 37, FEA Periods: 38, Commission Meetings: 20, Reporting Deadlines: 21, Conferences and Outreach: 22, AOs and Rules: 23, Other: 24, Quarterly: 25, Monthly: 26, Pre and Post-Elections: 27, EC Periods:28, and IE Periods: 29
  , calendarDateDescription          :: Text -- ^
  , calendarDateEnd_date             :: Text -- ^
  , calendarDateEvent_id             :: Int -- ^ An unique ID for an event. Useful for downloading a single event to your calendar. This ID is not a permanent, persistent ID.
  , calendarDateLocation             :: Text -- ^ Can be state address or room.
  , calendarDateStart_date           :: Text -- ^
  , calendarDateState                :: [Text] -- ^ The state field only applies to election dates and reporting deadlines, reporting periods and all other dates do not have the array of states to filter on
  , calendarDateSummary              :: Text -- ^
  , calendarDateUrl                  :: Text -- ^ A url for that event
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
  { candidateActive_through           :: Int -- ^ Last year a candidate was active. This field is specific to the candidate_id so if the same person runs for another office, there may be a different record for them.
  , candidateCandidate_id             :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateCandidate_status         :: Text -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateCycles                   :: [Int] -- ^  Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the `election_full` flag.
  , candidateDistrict                 :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateDistrict_number          :: Int -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateElection_districts       :: [Text] -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateElection_years           :: [Int] -- ^ Years in which a candidate ran for office.
  , candidateFederal_funds_flag       :: Bool -- ^
  , candidateFirst_file_date          :: Date -- ^ The day the FEC received the candidate's first filing. This is a F2 candidate registration.
  , candidateHas_raised_funds         :: Bool -- ^
  , candidateIncumbent_challenge      :: Text -- ^ One-letter code ('I', 'C', 'O') explaining if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateIncumbent_challenge_full :: Text -- ^ Explains if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateLast_f2_date             :: Date -- ^ The day the FEC received the candidate's most recent Form 2
  , candidateLast_file_date           :: Date -- ^ The day the FEC received the candidate's most recent filing
  , candidateLoad_date                :: Date -- ^ Date the information was loaded into the FEC systems. This can be affected by reseting systems and other factors, refer to receipt_date for the day that the FEC received the paper or electronic document. Keep in mind that paper filings take more time to process and there can be a lag between load_date and receipt_date. This field can be helpful to identify paper records that have been processed recently.
  , candidateName                     :: Text -- ^ Name of candidate running for office
  , candidateOffice                   :: Text -- ^ Federal office candidate runs for: H, S or P
  , candidateOffice_full              :: Text -- ^ Federal office candidate runs for: House, Senate or presidential
  , candidateParty                    :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , candidateParty_full               :: Text -- ^ Party affiliated with a candidate or committee
  , candidatePrincipal_committees     :: [Committee] -- ^
  , candidateState                    :: Text -- ^ US state or territory where a candidate runs for office
  } deriving (Show, Eq, Generic)

instance FromJSON Candidate where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "candidate")
instance ToJSON Candidate where
  toJSON = genericToJSON (removeFieldLabelPrefix False "candidate")

-- |
data CandidateCommitteeTotalsHouseSenate = CandidateCommitteeTotalsHouseSenate
  { candidateCommitteeTotalsHouseSenateAll_other_loans :: Double -- ^
  , candidateCommitteeTotalsHouseSenateCandidate_contribution :: Double -- ^
  , candidateCommitteeTotalsHouseSenateCandidate_id :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateCommitteeTotalsHouseSenateContribution_refunds :: Double -- ^
  , candidateCommitteeTotalsHouseSenateContributions :: Double -- ^ Contribution
  , candidateCommitteeTotalsHouseSenateCoverage_end_date :: Integer -- ^
  , candidateCommitteeTotalsHouseSenateCoverage_start_date :: Integer -- ^
  , candidateCommitteeTotalsHouseSenateCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , candidateCommitteeTotalsHouseSenateDisbursements :: Double -- ^ Disbursements
  , candidateCommitteeTotalsHouseSenateExempt_legal_accounting_disbursement :: Double -- ^
  , candidateCommitteeTotalsHouseSenateFederal_funds :: Double -- ^
  , candidateCommitteeTotalsHouseSenateFull_election :: Bool -- ^
  , candidateCommitteeTotalsHouseSenateFundraising_disbursements :: Double -- ^
  , candidateCommitteeTotalsHouseSenateIndividual_contributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateIndividual_itemized_contributions :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less.
  , candidateCommitteeTotalsHouseSenateIndividual_unitemized_contributions :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total.
  , candidateCommitteeTotalsHouseSenateLast_beginning_image_number :: Text -- ^
  , candidateCommitteeTotalsHouseSenateLast_cash_on_hand_end_period :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLast_debts_owed_by_committee :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLast_debts_owed_to_committee :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLast_net_contributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLast_net_operating_expenditures :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLast_report_type_full :: Text -- ^
  , candidateCommitteeTotalsHouseSenateLast_report_year :: Int -- ^
  , candidateCommitteeTotalsHouseSenateLoan_repayments :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLoan_repayments_candidate_loans :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLoan_repayments_other_loans :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLoans :: Double -- ^
  , candidateCommitteeTotalsHouseSenateLoans_made_by_candidate :: Double -- ^
  , candidateCommitteeTotalsHouseSenateNet_contributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateNet_operating_expenditures :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOffsets_to_fundraising_expenditures :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOffsets_to_legal_accounting :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOffsets_to_operating_expenditures :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOperating_expenditures :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOther_disbursements :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOther_political_committee_contributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateOther_receipts :: Double -- ^
  , candidateCommitteeTotalsHouseSenatePolitical_party_committee_contributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateReceipts :: Double -- ^
  , candidateCommitteeTotalsHouseSenateRefunded_individual_contributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateRefunded_other_political_committee_contributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateRefunded_political_party_committee_contributions :: Double -- ^
  , candidateCommitteeTotalsHouseSenateTotal_offsets_to_operating_expenditures :: Double -- ^
  , candidateCommitteeTotalsHouseSenateTransaction_coverage_date :: Integer -- ^
  , candidateCommitteeTotalsHouseSenateTransfers_from_other_authorized_committee :: Double -- ^
  , candidateCommitteeTotalsHouseSenateTransfers_to_other_authorized_committee :: Double -- ^
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
  { candidateCommitteeTotalsPresidentialCandidate_contribution :: Double -- ^
  , candidateCommitteeTotalsPresidentialCandidate_id :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateCommitteeTotalsPresidentialContribution_refunds :: Double -- ^
  , candidateCommitteeTotalsPresidentialContributions :: Double -- ^ Contribution
  , candidateCommitteeTotalsPresidentialCoverage_end_date :: Integer -- ^
  , candidateCommitteeTotalsPresidentialCoverage_start_date :: Integer -- ^
  , candidateCommitteeTotalsPresidentialCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , candidateCommitteeTotalsPresidentialDisbursements :: Double -- ^ Disbursements
  , candidateCommitteeTotalsPresidentialExempt_legal_accounting_disbursement :: Double -- ^
  , candidateCommitteeTotalsPresidentialFederal_funds :: Double -- ^
  , candidateCommitteeTotalsPresidentialFull_election :: Bool -- ^
  , candidateCommitteeTotalsPresidentialFundraising_disbursements :: Double -- ^
  , candidateCommitteeTotalsPresidentialIndividual_contributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialIndividual_itemized_contributions :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less.
  , candidateCommitteeTotalsPresidentialIndividual_unitemized_contributions :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total.
  , candidateCommitteeTotalsPresidentialLast_beginning_image_number :: Text -- ^
  , candidateCommitteeTotalsPresidentialLast_cash_on_hand_end_period :: Double -- ^
  , candidateCommitteeTotalsPresidentialLast_debts_owed_by_committee :: Double -- ^
  , candidateCommitteeTotalsPresidentialLast_debts_owed_to_committee :: Double -- ^
  , candidateCommitteeTotalsPresidentialLast_report_type_full :: Text -- ^
  , candidateCommitteeTotalsPresidentialLast_report_year :: Int -- ^
  , candidateCommitteeTotalsPresidentialLoan_repayments_made :: Double -- ^
  , candidateCommitteeTotalsPresidentialLoans_received :: Double -- ^
  , candidateCommitteeTotalsPresidentialLoans_received_from_candidate :: Double -- ^
  , candidateCommitteeTotalsPresidentialNet_contributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialNet_operating_expenditures :: Double -- ^
  , candidateCommitteeTotalsPresidentialOffsets_to_fundraising_expenditures :: Double -- ^
  , candidateCommitteeTotalsPresidentialOffsets_to_legal_accounting :: Double -- ^
  , candidateCommitteeTotalsPresidentialOffsets_to_operating_expenditures :: Double -- ^
  , candidateCommitteeTotalsPresidentialOperating_expenditures :: Double -- ^
  , candidateCommitteeTotalsPresidentialOther_disbursements :: Double -- ^
  , candidateCommitteeTotalsPresidentialOther_loans_received :: Double -- ^
  , candidateCommitteeTotalsPresidentialOther_political_committee_contributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialOther_receipts :: Double -- ^
  , candidateCommitteeTotalsPresidentialPolitical_party_committee_contributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialReceipts :: Double -- ^
  , candidateCommitteeTotalsPresidentialRefunded_individual_contributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialRefunded_other_political_committee_contributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialRefunded_political_party_committee_contributions :: Double -- ^
  , candidateCommitteeTotalsPresidentialRepayments_loans_made_by_candidate :: Double -- ^
  , candidateCommitteeTotalsPresidentialRepayments_other_loans :: Double -- ^
  , candidateCommitteeTotalsPresidentialTotal_offsets_to_operating_expenditures :: Double -- ^
  , candidateCommitteeTotalsPresidentialTransaction_coverage_date :: Integer -- ^
  , candidateCommitteeTotalsPresidentialTransfers_from_affiliated_committee :: Double -- ^
  , candidateCommitteeTotalsPresidentialTransfers_to_other_authorized_committee :: Double -- ^
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
  { candidateDetailActive_through           :: Int -- ^ Last year a candidate was active. This field is specific to the candidate_id so if the same person runs for another office, there may be a different record for them.
  , candidateDetailAddress_city             :: Text -- ^ City of candidate's address, as reported on their Form 2.
  , candidateDetailAddress_state            :: Text -- ^ State of candidate's address, as reported on their Form 2.
  , candidateDetailAddress_street_1         :: Text -- ^ Street of candidate's address, as reported on their Form 2.
  , candidateDetailAddress_street_2         :: Text -- ^ Additional street information of candidate's address, as reported on their Form 2.
  , candidateDetailAddress_zip              :: Text -- ^ Zip code of candidate's address, as reported on their Form 2.
  , candidateDetailCandidate_id             :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateDetailCandidate_inactive       :: Bool -- ^ True indicates that a candidate is inactive.
  , candidateDetailCandidate_status         :: Text -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateDetailCycles                   :: [Int] -- ^  Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the `election_full` flag.
  , candidateDetailDistrict                 :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateDetailDistrict_number          :: Int -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateDetailElection_districts       :: [Text] -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateDetailElection_years           :: [Int] -- ^ Years in which a candidate ran for office.
  , candidateDetailFederal_funds_flag       :: Bool -- ^
  , candidateDetailFirst_file_date          :: Date -- ^ The day the FEC received the candidate's first filing. This is a F2 candidate registration.
  , candidateDetailFlags                    :: Text -- ^
  , candidateDetailHas_raised_funds         :: Bool -- ^
  , candidateDetailIncumbent_challenge      :: Text -- ^ One-letter code ('I', 'C', 'O') explaining if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateDetailIncumbent_challenge_full :: Text -- ^ Explains if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateDetailLast_f2_date             :: Date -- ^ The day the FEC received the candidate's most recent Form 2
  , candidateDetailLast_file_date           :: Date -- ^ The day the FEC received the candidate's most recent filing
  , candidateDetailLoad_date                :: Date -- ^ Date the information was loaded into the FEC systems. This can be affected by reseting systems and other factors, refer to receipt_date for the day that the FEC received the paper or electronic document. Keep in mind that paper filings take more time to process and there can be a lag between load_date and receipt_date. This field can be helpful to identify paper records that have been processed recently.
  , candidateDetailName                     :: Text -- ^ Name of candidate running for office
  , candidateDetailOffice                   :: Text -- ^ Federal office candidate runs for: H, S or P
  , candidateDetailOffice_full              :: Text -- ^ Federal office candidate runs for: House, Senate or presidential
  , candidateDetailParty                    :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , candidateDetailParty_full               :: Text -- ^ Party affiliated with a candidate or committee
  , candidateDetailState                    :: Text -- ^ US state or territory where a candidate runs for office
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
  { candidateFlagsCandidate_id       :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateFlagsFederal_funds_flag :: Bool -- ^ A boolean the describes if a presidential candidate has accepted federal funds. The flag will be false for House and Senate candidates.
  , candidateFlagsHas_raised_funds   :: Bool -- ^ A boolean that describes if a candidate's committee has ever received any receipts for their campaign for this particular office. (Candidates have separate candidate IDs for each office.)
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
  { candidateHistoryActive_through           :: Int -- ^ Last year a candidate was active. This field is specific to the candidate_id so if the same person runs for another office, there may be a different record for them.
  , candidateHistoryAddress_city             :: Text -- ^ City of candidate's address, as reported on their Form 2.
  , candidateHistoryAddress_state            :: Text -- ^ State of candidate's address, as reported on their Form 2.
  , candidateHistoryAddress_street_1         :: Text -- ^ Street of candidate's address, as reported on their Form 2.
  , candidateHistoryAddress_street_2         :: Text -- ^ Additional street information of candidate's address, as reported on their Form 2.
  , candidateHistoryAddress_zip              :: Text -- ^ Zip code of candidate's address, as reported on their Form 2.
  , candidateHistoryCandidate_election_year  :: Int -- ^ The last year of the cycle for this election.
  , candidateHistoryCandidate_id             :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateHistoryCandidate_inactive       :: Bool -- ^ True indicates that a candidate is inactive.
  , candidateHistoryCandidate_status         :: Text -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateHistoryCycles                   :: [Int] -- ^  Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the `election_full` flag.
  , candidateHistoryDistrict                 :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateHistoryDistrict_number          :: Int -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateHistoryElection_districts       :: [Text] -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateHistoryElection_years           :: [Int] -- ^ Years in which a candidate ran for office.
  , candidateHistoryFirst_file_date          :: Date -- ^ The day the FEC received the candidate's first filing. This is a F2 candidate registration.
  , candidateHistoryFlags                    :: Text -- ^
  , candidateHistoryIncumbent_challenge      :: Text -- ^ One-letter code ('I', 'C', 'O') explaining if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateHistoryIncumbent_challenge_full :: Text -- ^ Explains if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateHistoryLast_f2_date             :: Date -- ^ The day the FEC received the candidate's most recent Form 2
  , candidateHistoryLast_file_date           :: Date -- ^ The day the FEC received the candidate's most recent filing
  , candidateHistoryLoad_date                :: Date -- ^ Date the information was loaded into the FEC systems. This can be affected by reseting systems and other factors, refer to receipt_date for the day that the FEC received the paper or electronic document. Keep in mind that paper filings take more time to process and there can be a lag between load_date and receipt_date. This field can be helpful to identify paper records that have been processed recently.
  , candidateHistoryName                     :: Text -- ^ Name of candidate running for office
  , candidateHistoryOffice                   :: Text -- ^ Federal office candidate runs for: H, S or P
  , candidateHistoryOffice_full              :: Text -- ^ Federal office candidate runs for: House, Senate or presidential
  , candidateHistoryParty                    :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , candidateHistoryParty_full               :: Text -- ^ Party affiliated with a candidate or committee
  , candidateHistoryState                    :: Text -- ^ US state or territory where a candidate runs for office
  , candidateHistoryTwo_year_period          :: Int -- ^  Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the `election_full` flag.
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
  { candidateHistoryTotalActive_through           :: Int -- ^ Last year a candidate was active. This field is specific to the candidate_id so if the same person runs for another office, there may be a different record for them.
  , candidateHistoryTotalAddress_city             :: Text -- ^ City of candidate's address, as reported on their Form 2.
  , candidateHistoryTotalAddress_state            :: Text -- ^ State of candidate's address, as reported on their Form 2.
  , candidateHistoryTotalAddress_street_1         :: Text -- ^ Street of candidate's address, as reported on their Form 2.
  , candidateHistoryTotalAddress_street_2         :: Text -- ^ Additional street information of candidate's address, as reported on their Form 2.
  , candidateHistoryTotalAddress_zip              :: Text -- ^ Zip code of candidate's address, as reported on their Form 2.
  , candidateHistoryTotalCandidate_election_year  :: Int -- ^ The last year of the cycle for this election.
  , candidateHistoryTotalCandidate_id             :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , candidateHistoryTotalCandidate_inactive       :: Bool -- ^ True indicates that a candidate is inactive.
  , candidateHistoryTotalCandidate_status         :: Text -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateHistoryTotalCash_on_hand_end_period  :: Double -- ^
  , candidateHistoryTotalCoverage_end_date        :: Date -- ^ Ending date of the reporting period
  , candidateHistoryTotalCoverage_start_date      :: Date -- ^ Beginning date of the reporting period
  , candidateHistoryTotalCycle                    :: Int -- ^
  , candidateHistoryTotalCycles                   :: [Int] -- ^  Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the `election_full` flag.
  , candidateHistoryTotalDebts_owed_by_committee  :: Double -- ^
  , candidateHistoryTotalDisbursements            :: Double -- ^
  , candidateHistoryTotalDistrict                 :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateHistoryTotalDistrict_number          :: Int -- ^ One-letter code explaining if the candidate is:         - C present candidate         - F future candidate         - N not yet a candidate         - P prior candidate
  , candidateHistoryTotalElection_districts       :: [Text] -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , candidateHistoryTotalElection_year            :: Int -- ^
  , candidateHistoryTotalElection_years           :: [Int] -- ^ Years in which a candidate ran for office.
  , candidateHistoryTotalFederal_funds_flag       :: Bool -- ^ A boolean the describes if a presidential candidate has accepted federal funds. The flag will be false for House and Senate candidates.
  , candidateHistoryTotalFirst_file_date          :: Date -- ^ The day the FEC received the candidate's first filing. This is a F2 candidate registration.
  , candidateHistoryTotalFlags                    :: Text -- ^
  , candidateHistoryTotalHas_raised_funds         :: Bool -- ^ A boolean that describes if a candidate's committee has ever received any receipts for their campaign for this particular office. (Candidates have separate candidate IDs for each office.)
  , candidateHistoryTotalIncumbent_challenge      :: Text -- ^ One-letter code ('I', 'C', 'O') explaining if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateHistoryTotalIncumbent_challenge_full :: Text -- ^ Explains if the candidate is an incumbent, a challenger, or if the seat is open.
  , candidateHistoryTotalIs_election              :: Bool -- ^
  , candidateHistoryTotalLast_f2_date             :: Date -- ^ The day the FEC received the candidate's most recent Form 2
  , candidateHistoryTotalLast_file_date           :: Date -- ^ The day the FEC received the candidate's most recent filing
  , candidateHistoryTotalLoad_date                :: Date -- ^ Date the information was loaded into the FEC systems. This can be affected by reseting systems and other factors, refer to receipt_date for the day that the FEC received the paper or electronic document. Keep in mind that paper filings take more time to process and there can be a lag between load_date and receipt_date. This field can be helpful to identify paper records that have been processed recently.
  , candidateHistoryTotalName                     :: Text -- ^ Name of candidate running for office
  , candidateHistoryTotalOffice                   :: Text -- ^ Federal office candidate runs for: H, S or P
  , candidateHistoryTotalOffice_full              :: Text -- ^ Federal office candidate runs for: House, Senate or presidential
  , candidateHistoryTotalParty                    :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , candidateHistoryTotalParty_full               :: Text -- ^ Party affiliated with a candidate or committee
  , candidateHistoryTotalReceipts                 :: Double -- ^
  , candidateHistoryTotalState                    :: Text -- ^ US state or territory where a candidate runs for office
  , candidateHistoryTotalTwo_year_period          :: Int -- ^  Two-year election cycle in which a candidate runs for office. Calculated from FEC Form 2. The cycle begins with an odd year and is named for its ending, even year. This cycle follows the traditional house election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. To see data for the entire four years of a presidential term or six years of a senatorial term, you will need the `election_full` flag.
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
  { candidateSearchId            :: Text -- ^
  , candidateSearchName          :: Text -- ^
  , candidateSearchOffice_sought :: Text -- ^
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
  { candidateTotalCandidate_id            :: Text -- ^
  , candidateTotalCash_on_hand_end_period :: Double -- ^
  , candidateTotalCoverage_end_date       :: Date -- ^ Ending date of the reporting period
  , candidateTotalCoverage_start_date     :: Date -- ^ Beginning date of the reporting period
  , candidateTotalCycle                   :: Int -- ^
  , candidateTotalDebts_owed_by_committee :: Double -- ^
  , candidateTotalDisbursements           :: Double -- ^
  , candidateTotalElection_year           :: Int -- ^
  , candidateTotalFederal_funds_flag      :: Bool -- ^ A boolean the describes if a presidential candidate has accepted federal funds. The flag will be false for House and Senate candidates.
  , candidateTotalHas_raised_funds        :: Bool -- ^ A boolean that describes if a candidate's committee has ever received any receipts for their campaign for this particular office. (Candidates have separate candidate IDs for each office.)
  , candidateTotalIs_election             :: Bool -- ^
  , candidateTotalReceipts                :: Double -- ^
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
  { committeeCandidate_ids          :: [Text] -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , committeeCommittee_id           :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeCommittee_type         :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeCommittee_type_full    :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeCycles                 :: [Int] -- ^  A two year election cycle that the committee was active- (after original registration date but before expiration date in FEC Form 1s) The cycle begins with an odd year and is named for its ending, even year.
  , committeeDesignation            :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeDesignation_full       :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeFiling_frequency       :: Text -- ^ The one-letter      code of the filing frequency:          - A Administratively terminated          - D Debt          - M Monthly filer          - Q Quarterly filer          - T Terminated          - W Waived
  , committeeFirst_file_date        :: Date -- ^ The day the FEC received the committee's first filing. This is usually a Form 1 committee registration.
  , committeeLast_f1_date           :: Date -- ^ The day the FEC received the committee's most recent Form 1
  , committeeLast_file_date         :: Date -- ^ The day the FEC received the committee's most recent filing
  , committeeName                   :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeOrganization_type      :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , committeeOrganization_type_full :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , committeeParty                  :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , committeeParty_full             :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , committeeState                  :: Text -- ^ State of the committee's address as filed on the Form 1
  , committeeTreasurer_name         :: Text -- ^ Name of the Committee's treasurer. If multiple treasurers for the committee, the most recent treasurer will be shown.
  } deriving (Show, Eq, Generic)

instance FromJSON Committee where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committee")
instance ToJSON Committee where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committee")

-- |
data CommitteeDetail = CommitteeDetail
  { committeeDetailCandidate_ids           :: [Text] -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , committeeDetailCity                    :: Text -- ^ City of committee as reported on the Form 1
  , committeeDetailCommittee_id            :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeDetailCommittee_type          :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeDetailCommittee_type_full     :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeDetailCustodian_city          :: Text -- ^ City of committee custodian as reported on the Form 1
  , committeeDetailCustodian_name_1        :: Text -- ^
  , committeeDetailCustodian_name_2        :: Text -- ^
  , committeeDetailCustodian_name_full     :: Text -- ^ Name of custodian
  , committeeDetailCustodian_name_middle   :: Text -- ^
  , committeeDetailCustodian_name_prefix   :: Text -- ^
  , committeeDetailCustodian_name_suffix   :: Text -- ^
  , committeeDetailCustodian_name_title    :: Text -- ^
  , committeeDetailCustodian_phone         :: Text -- ^ Phone number of the committee custodian as reported on the Form 1
  , committeeDetailCustodian_state         :: Text -- ^ State of the committee custodian as reported on the Form 1
  , committeeDetailCustodian_street_1      :: Text -- ^ Street address of the committee custodian as reported on the Form 1
  , committeeDetailCustodian_street_2      :: Text -- ^ Second line of the street address of the committee custodian as reported on the Form 1
  , committeeDetailCustodian_zip           :: Text -- ^ Zip code of the committee custodian as reported on the Form 1
  , committeeDetailCycles                  :: [Int] -- ^  A two year election cycle that the committee was active- (after original registration date but before expiration date in FEC Form 1s) The cycle begins with an odd year and is named for its ending, even year.
  , committeeDetailDesignation             :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeDetailDesignation_full        :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeDetailEmail                   :: Text -- ^ Email as reported on the Form 1
  , committeeDetailFax                     :: Text -- ^ Fax as reported on the Form 1
  , committeeDetailFiling_frequency        :: Text -- ^ The one-letter      code of the filing frequency:          - A Administratively terminated          - D Debt          - M Monthly filer          - Q Quarterly filer          - T Terminated          - W Waived
  , committeeDetailFirst_file_date         :: Date -- ^ The day the FEC received the committee's first filing. This is usually a Form 1 committee registration.
  , committeeDetailForm_type               :: Text -- ^ Form where the information was reported
  , committeeDetailLast_file_date          :: Date -- ^ The day the FEC received the committee's most recent filing
  , committeeDetailLeadership_pac          :: Text -- ^ Indicates if the committee is a leadership PAC
  , committeeDetailLobbyist_registrant_pac :: Text -- ^ Indicates if the committee is a lobbyist registrant PAC
  , committeeDetailName                    :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeDetailOrganization_type       :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , committeeDetailOrganization_type_full  :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , committeeDetailParty                   :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , committeeDetailParty_full              :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , committeeDetailParty_type              :: Text -- ^ Code for the type of party the committee is, only if applicable
  , committeeDetailParty_type_full         :: Text -- ^ Description of the type of party the committee is, only if applicable
  , committeeDetailQualifying_date         :: Date -- ^ Date the committee became a qualified committee.
  , committeeDetailState                   :: Text -- ^ State of the committee's address as filed on the Form 1
  , committeeDetailState_full              :: Text -- ^ State of committee as reported on the Form 1
  , committeeDetailStreet_1                :: Text -- ^ Street address of committee as reported on the Form 1
  , committeeDetailStreet_2                :: Text -- ^ Second line of street address of committee as reported on the Form 1
  , committeeDetailTreasurer_city          :: Text -- ^ City of committee treasurer as reported on the Form 1
  , committeeDetailTreasurer_name          :: Text -- ^ Name of the Committee's treasurer. If multiple treasurers for the committee, the most recent treasurer will be shown.
  , committeeDetailTreasurer_name_1        :: Text -- ^
  , committeeDetailTreasurer_name_2        :: Text -- ^
  , committeeDetailTreasurer_name_middle   :: Text -- ^
  , committeeDetailTreasurer_name_prefix   :: Text -- ^
  , committeeDetailTreasurer_name_suffix   :: Text -- ^
  , committeeDetailTreasurer_name_title    :: Text -- ^
  , committeeDetailTreasurer_phone         :: Text -- ^ Phone number of the committee treasurer as reported on the Form 1
  , committeeDetailTreasurer_state         :: Text -- ^ State of the committee treasurer as reported on the Form 1
  , committeeDetailTreasurer_street_1      :: Text -- ^ Street of the committee treasurer as reported on the Form 1
  , committeeDetailTreasurer_street_2      :: Text -- ^ Second line of the street address of the committee treasurer as reported on the Form 1
  , committeeDetailTreasurer_zip           :: Text -- ^ Zip code of the committee treasurer as reported on the Form 1
  , committeeDetailWebsite                 :: Text -- ^ Website url as reported on the Form 1
  , committeeDetailZip                     :: Text -- ^ Zip code of committee as reported on the Form 1
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
  { committeeHistoryCandidate_ids          :: [Text] -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , committeeHistoryCity                   :: Text -- ^ City of committee as reported on the Form 1
  , committeeHistoryCommittee_id           :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeHistoryCommittee_type         :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeHistoryCommittee_type_full    :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeHistoryCycle                  :: Int -- ^  A two year election cycle that the committee was active- (after original registration date but before expiration date in FEC Form 1s) The cycle begins with an odd year and is named for its ending, even year.
  , committeeHistoryCycles                 :: [Int] -- ^  A two year election cycle that the committee was active- (after original registration date but before expiration date in FEC Form 1s) The cycle begins with an odd year and is named for its ending, even year.
  , committeeHistoryDesignation            :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeHistoryDesignation_full       :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeHistoryFiling_frequency       :: Text -- ^ The one-letter      code of the filing frequency:          - A Administratively terminated          - D Debt          - M Monthly filer          - Q Quarterly filer          - T Terminated          - W Waived
  , committeeHistoryName                   :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeHistoryOrganization_type      :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , committeeHistoryOrganization_type_full :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , committeeHistoryParty                  :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , committeeHistoryParty_full             :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , committeeHistoryState                  :: Text -- ^ State of the committee's address as filed on the Form 1
  , committeeHistoryState_full             :: Text -- ^ State of committee as reported on the Form 1
  , committeeHistoryStreet_1               :: Text -- ^ Street address of committee as reported on the Form 1
  , committeeHistoryStreet_2               :: Text -- ^ Second line of street address of committee as reported on the Form 1
  , committeeHistoryTreasurer_name         :: Text -- ^ Name of the Committee's treasurer. If multiple treasurers for the committee, the most recent treasurer will be shown.
  , committeeHistoryZip                    :: Text -- ^ Zip code of committee as reported on the Form 1
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
  { committeeReportsAggregate_amount_personal_contributions_general :: Double -- ^
  , committeeReportsAggregate_contributions_personal_funds_primary :: Double -- ^
  , committeeReportsAll_loans_received_period :: Double -- ^
  , committeeReportsAll_loans_received_ytd :: Double -- ^
  , committeeReportsAll_other_loans_period :: Double -- ^
  , committeeReportsAll_other_loans_ytd :: Double -- ^
  , committeeReportsAllocated_federal_election_levin_share_period :: Double -- ^
  , committeeReportsAmendment_chain :: [Double] -- ^  The first value in the chain is the original filing.  The ordering in the chain reflects the order the amendments were filed up to the amendment being viewed.
  , committeeReportsAmendment_indicator :: Text -- ^
  , committeeReportsAmendment_indicator_full :: Text -- ^
  , committeeReportsBeginning_image_number :: Text -- ^
  , committeeReportsCalendar_ytd :: Int -- ^
  , committeeReportsCandidate_contribution_period :: Double -- ^
  , committeeReportsCandidate_contribution_ytd :: Double -- ^
  , committeeReportsCash_on_hand_beginning_calendar_ytd :: Double -- ^
  , committeeReportsCash_on_hand_beginning_period :: Double -- ^ Balance for the committee at the start of the two-year period
  , committeeReportsCash_on_hand_close_ytd :: Double -- ^
  , committeeReportsCash_on_hand_end_period :: Double -- ^ Ending cash balance on the most recent filing
  , committeeReportsCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeReportsCommittee_name :: Text -- ^
  , committeeReportsCommittee_type :: Text -- ^
  , committeeReportsCoordinated_expenditures_by_party_committee_period :: Double -- ^
  , committeeReportsCoordinated_expenditures_by_party_committee_ytd :: Double -- ^
  , committeeReportsCoverage_end_date :: Integer -- ^ Ending date of the reporting period
  , committeeReportsCoverage_start_date :: Integer -- ^ Beginning date of the reporting period
  , committeeReportsCsv_url :: Text -- ^
  , committeeReportsCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeReportsDebts_owed_by_committee :: Double -- ^ Debts owed by the committee
  , committeeReportsDebts_owed_to_committee :: Double -- ^ Debts owed to the committee
  , committeeReportsDocument_description :: Text -- ^
  , committeeReportsEnd_image_number :: Text -- ^
  , committeeReportsExempt_legal_accounting_disbursement_period :: Double -- ^
  , committeeReportsExempt_legal_accounting_disbursement_ytd :: Double -- ^
  , committeeReportsExpenditure_subject_to_limits :: Double -- ^
  , committeeReportsFec_file_id :: Text -- ^
  , committeeReportsFec_url :: Text -- ^
  , committeeReportsFed_candidate_committee_contribution_refunds_ytd :: Double -- ^
  , committeeReportsFed_candidate_committee_contributions_period :: Double -- ^
  , committeeReportsFed_candidate_committee_contributions_ytd :: Double -- ^
  , committeeReportsFed_candidate_contribution_refunds_period :: Double -- ^
  , committeeReportsFederal_funds_period :: Double -- ^
  , committeeReportsFederal_funds_ytd :: Double -- ^
  , committeeReportsFile_number :: Int -- ^
  , committeeReportsFundraising_disbursements_period :: Double -- ^
  , committeeReportsFundraising_disbursements_ytd :: Double -- ^
  , committeeReportsGross_receipt_authorized_committee_general :: Double -- ^
  , committeeReportsGross_receipt_authorized_committee_primary :: Double -- ^
  , committeeReportsGross_receipt_minus_personal_contribution_general :: Double -- ^
  , committeeReportsGross_receipt_minus_personal_contributions_primary :: Double -- ^
  , committeeReportsHtml_url :: Text -- ^ HTML link to the filing.
  , committeeReportsIndependent_contributions_period :: Double -- ^
  , committeeReportsIndependent_expenditures_period :: Double -- ^
  , committeeReportsIndependent_expenditures_ytd :: Double -- ^
  , committeeReportsIndividual_itemized_contributions_period :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the reporting period
  , committeeReportsIndividual_itemized_contributions_ytd :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the year to date
  , committeeReportsIndividual_unitemized_contributions_period :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the reporting period
  , committeeReportsIndividual_unitemized_contributions_ytd :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the year to date
  , committeeReportsIs_amended :: Bool -- ^ False indicates that a report is the most recent. True indicates that the report has been superseded by an amendment.
  , committeeReportsItems_on_hand_liquidated :: Double -- ^
  , committeeReportsLoan_repayments_candidate_loans_period :: Double -- ^
  , committeeReportsLoan_repayments_candidate_loans_ytd :: Double -- ^
  , committeeReportsLoan_repayments_made_period :: Double -- ^
  , committeeReportsLoan_repayments_made_ytd :: Double -- ^
  , committeeReportsLoan_repayments_other_loans_period :: Double -- ^
  , committeeReportsLoan_repayments_other_loans_ytd :: Double -- ^
  , committeeReportsLoan_repayments_received_period :: Double -- ^
  , committeeReportsLoan_repayments_received_ytd :: Double -- ^
  , committeeReportsLoans_made_by_candidate_period :: Double -- ^
  , committeeReportsLoans_made_by_candidate_ytd :: Double -- ^
  , committeeReportsLoans_made_period :: Double -- ^
  , committeeReportsLoans_made_ytd :: Double -- ^
  , committeeReportsLoans_received_from_candidate_period :: Double -- ^
  , committeeReportsLoans_received_from_candidate_ytd :: Double -- ^
  , committeeReportsMeans_filed :: Text -- ^ The method used to file with the FEC, either electronic or on paper.
  , committeeReportsMost_recent :: Bool -- ^
  , committeeReportsMost_recent_file_number :: Double -- ^
  , committeeReportsNet_contributions_cycle_to_date :: Double -- ^
  , committeeReportsNet_contributions_period :: Double -- ^
  , committeeReportsNet_contributions_ytd :: Double -- ^
  , committeeReportsNet_operating_expenditures_cycle_to_date :: Double -- ^
  , committeeReportsNet_operating_expenditures_period :: Double -- ^
  , committeeReportsNet_operating_expenditures_ytd :: Double -- ^
  , committeeReportsNon_allocated_fed_election_activity_period :: Double -- ^
  , committeeReportsNon_allocated_fed_election_activity_ytd :: Double -- ^
  , committeeReportsNonfed_share_allocated_disbursements_period :: Double -- ^
  , committeeReportsOffsets_to_fundraising_expenditures_period :: Double -- ^
  , committeeReportsOffsets_to_fundraising_expenditures_ytd :: Double -- ^
  , committeeReportsOffsets_to_legal_accounting_period :: Double -- ^
  , committeeReportsOffsets_to_legal_accounting_ytd :: Double -- ^
  , committeeReportsOffsets_to_operating_expenditures_period :: Double -- ^ Offsets to operating expenditures total for the reporting period
  , committeeReportsOffsets_to_operating_expenditures_ytd :: Double -- ^ Offsets to operating expenditures total for the year to date
  , committeeReportsOperating_expenditures_period :: Double -- ^
  , committeeReportsOperating_expenditures_ytd :: Double -- ^
  , committeeReportsOther_disbursements_period :: Double -- ^ Other disbursements total for the reporting period
  , committeeReportsOther_disbursements_ytd :: Double -- ^ Other disbursements total for the year to date
  , committeeReportsOther_fed_operating_expenditures_period :: Double -- ^
  , committeeReportsOther_fed_operating_expenditures_ytd :: Double -- ^
  , committeeReportsOther_fed_receipts_period :: Double -- ^
  , committeeReportsOther_fed_receipts_ytd :: Double -- ^
  , committeeReportsOther_loans_received_period :: Double -- ^
  , committeeReportsOther_loans_received_ytd :: Double -- ^
  , committeeReportsOther_political_committee_contributions_period :: Double -- ^ Other committees contributions total for the reporting period
  , committeeReportsOther_political_committee_contributions_ytd :: Double -- ^ Other committees contributions total for the year to date
  , committeeReportsOther_receipts_period :: Double -- ^
  , committeeReportsOther_receipts_ytd :: Double -- ^
  , committeeReportsPdf_url :: Text -- ^
  , committeeReportsPolitical_party_committee_contributions_period :: Double -- ^ Party committees contributions total for the reporting period
  , committeeReportsPolitical_party_committee_contributions_ytd :: Double -- ^ Party committees contributions total for the year to date
  , committeeReportsPrevious_file_number :: Double -- ^
  , committeeReportsReceipt_date :: Date -- ^ Date the FEC received the electronic or paper record
  , committeeReportsRefunded_individual_contributions_period :: Double -- ^ Individual refunds total for the reporting period
  , committeeReportsRefunded_individual_contributions_ytd :: Double -- ^ Individual refunds total for the year to date
  , committeeReportsRefunded_other_political_committee_contributions_period :: Double -- ^ Other committee refunds total for the reporting period
  , committeeReportsRefunded_other_political_committee_contributions_ytd :: Double -- ^ Other committee refunds total for the year to date
  , committeeReportsRefunded_political_party_committee_contributions_period :: Double -- ^ Political party refunds total for the reporting period
  , committeeReportsRefunded_political_party_committee_contributions_ytd :: Double -- ^ Political party refunds total for the year to date
  , committeeReportsRefunds_total_contributions_col_total_ytd :: Double -- ^
  , committeeReportsRepayments_loans_made_by_candidate_period :: Double -- ^
  , committeeReportsRepayments_loans_made_candidate_ytd :: Double -- ^
  , committeeReportsRepayments_other_loans_period :: Double -- ^
  , committeeReportsRepayments_other_loans_ytd :: Double -- ^
  , committeeReportsReport_form :: Text -- ^
  , committeeReportsReport_type :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsReport_type_full :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsReport_year :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , committeeReportsShared_fed_activity_nonfed_ytd :: Double -- ^
  , committeeReportsShared_fed_activity_period :: Double -- ^
  , committeeReportsShared_fed_activity_ytd :: Double -- ^
  , committeeReportsShared_fed_operating_expenditures_period :: Double -- ^
  , committeeReportsShared_fed_operating_expenditures_ytd :: Double -- ^
  , committeeReportsShared_nonfed_operating_expenditures_period :: Double -- ^
  , committeeReportsShared_nonfed_operating_expenditures_ytd :: Double -- ^
  , committeeReportsSubtotal_period :: Double -- ^
  , committeeReportsSubtotal_summary_page_period :: Double -- ^
  , committeeReportsSubtotal_summary_period :: Double -- ^
  , committeeReportsSubtotal_summary_ytd :: Double -- ^
  , committeeReportsTotal_contribution_refunds_col_total_period :: Double -- ^
  , committeeReportsTotal_contribution_refunds_period :: Double -- ^ Total contribution refunds total for the reporting period
  , committeeReportsTotal_contribution_refunds_ytd :: Double -- ^ Total contribution refunds total for the year to date
  , committeeReportsTotal_contributions_column_total_period :: Double -- ^
  , committeeReportsTotal_contributions_period :: Double -- ^ Contribution total for the reporting period
  , committeeReportsTotal_contributions_ytd :: Double -- ^ Contribution total for the year to date
  , committeeReportsTotal_disbursements_period :: Double -- ^ Disbursements total for the reporting period
  , committeeReportsTotal_disbursements_ytd :: Double -- ^ Disbursements total for the year to date
  , committeeReportsTotal_fed_disbursements_period :: Double -- ^
  , committeeReportsTotal_fed_disbursements_ytd :: Double -- ^
  , committeeReportsTotal_fed_election_activity_period :: Double -- ^
  , committeeReportsTotal_fed_election_activity_ytd :: Double -- ^
  , committeeReportsTotal_fed_operating_expenditures_period :: Double -- ^
  , committeeReportsTotal_fed_operating_expenditures_ytd :: Double -- ^
  , committeeReportsTotal_fed_receipts_period :: Double -- ^
  , committeeReportsTotal_fed_receipts_ytd :: Double -- ^
  , committeeReportsTotal_individual_contributions_period :: Double -- ^ Individual contributions total for the reporting period
  , committeeReportsTotal_individual_contributions_ytd :: Double -- ^ Individual contributions total for the year to date
  , committeeReportsTotal_loan_repayments_made_period :: Double -- ^
  , committeeReportsTotal_loan_repayments_made_ytd :: Double -- ^
  , committeeReportsTotal_loans_received_period :: Double -- ^
  , committeeReportsTotal_loans_received_ytd :: Double -- ^
  , committeeReportsTotal_nonfed_transfers_period :: Double -- ^
  , committeeReportsTotal_nonfed_transfers_ytd :: Double -- ^
  , committeeReportsTotal_offsets_to_operating_expenditures_period :: Double -- ^
  , committeeReportsTotal_offsets_to_operating_expenditures_ytd :: Double -- ^
  , committeeReportsTotal_operating_expenditures_period :: Double -- ^
  , committeeReportsTotal_operating_expenditures_ytd :: Double -- ^
  , committeeReportsTotal_period :: Double -- ^
  , committeeReportsTotal_receipts_period :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the reporting period
  , committeeReportsTotal_receipts_ytd :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the year to date
  , committeeReportsTotal_ytd :: Double -- ^
  , committeeReportsTransfers_from_affiliated_committee_period :: Double -- ^
  , committeeReportsTransfers_from_affiliated_committee_ytd :: Double -- ^
  , committeeReportsTransfers_from_affiliated_party_period :: Double -- ^
  , committeeReportsTransfers_from_affiliated_party_ytd :: Double -- ^
  , committeeReportsTransfers_from_nonfed_account_period :: Double -- ^
  , committeeReportsTransfers_from_nonfed_account_ytd :: Double -- ^
  , committeeReportsTransfers_from_nonfed_levin_period :: Double -- ^
  , committeeReportsTransfers_from_nonfed_levin_ytd :: Double -- ^
  , committeeReportsTransfers_from_other_authorized_committee_period :: Double -- ^
  , committeeReportsTransfers_from_other_authorized_committee_ytd :: Double -- ^
  , committeeReportsTransfers_to_affiliated_committee_period :: Double -- ^
  , committeeReportsTransfers_to_affilitated_committees_ytd :: Double -- ^
  , committeeReportsTransfers_to_other_authorized_committee_period :: Double -- ^
  , committeeReportsTransfers_to_other_authorized_committee_ytd :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeReports where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeReports")
instance ToJSON CommitteeReports where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeReports")

-- |
data CommitteeReportsHouseSenate = CommitteeReportsHouseSenate
  { committeeReportsHouseSenateAggregate_amount_personal_contributions_general :: Double -- ^
  , committeeReportsHouseSenateAggregate_contributions_personal_funds_primary :: Double -- ^
  , committeeReportsHouseSenateAll_other_loans_period :: Double -- ^
  , committeeReportsHouseSenateAll_other_loans_ytd :: Double -- ^
  , committeeReportsHouseSenateAmendment_chain :: [Double] -- ^  The first value in the chain is the original filing.  The ordering in the chain reflects the order the amendments were filed up to the amendment being viewed.
  , committeeReportsHouseSenateAmendment_indicator :: Text -- ^
  , committeeReportsHouseSenateAmendment_indicator_full :: Text -- ^
  , committeeReportsHouseSenateBeginning_image_number :: Text -- ^
  , committeeReportsHouseSenateCandidate_contribution_period :: Double -- ^
  , committeeReportsHouseSenateCandidate_contribution_ytd :: Double -- ^
  , committeeReportsHouseSenateCash_on_hand_beginning_period :: Double -- ^ Balance for the committee at the start of the two-year period
  , committeeReportsHouseSenateCash_on_hand_end_period :: Double -- ^ Ending cash balance on the most recent filing
  , committeeReportsHouseSenateCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeReportsHouseSenateCommittee_name :: Text -- ^
  , committeeReportsHouseSenateCommittee_type :: Text -- ^
  , committeeReportsHouseSenateCoverage_end_date :: Integer -- ^ Ending date of the reporting period
  , committeeReportsHouseSenateCoverage_start_date :: Integer -- ^ Beginning date of the reporting period
  , committeeReportsHouseSenateCsv_url :: Text -- ^
  , committeeReportsHouseSenateCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeReportsHouseSenateDebts_owed_by_committee :: Double -- ^ Debts owed by the committee
  , committeeReportsHouseSenateDebts_owed_to_committee :: Double -- ^ Debts owed to the committee
  , committeeReportsHouseSenateDocument_description :: Text -- ^
  , committeeReportsHouseSenateEnd_image_number :: Text -- ^
  , committeeReportsHouseSenateFec_file_id :: Text -- ^
  , committeeReportsHouseSenateFec_url :: Text -- ^
  , committeeReportsHouseSenateFile_number :: Int -- ^
  , committeeReportsHouseSenateGross_receipt_authorized_committee_general :: Double -- ^
  , committeeReportsHouseSenateGross_receipt_authorized_committee_primary :: Double -- ^
  , committeeReportsHouseSenateGross_receipt_minus_personal_contribution_general :: Double -- ^
  , committeeReportsHouseSenateGross_receipt_minus_personal_contributions_primary :: Double -- ^
  , committeeReportsHouseSenateHtml_url :: Text -- ^ HTML link to the filing.
  , committeeReportsHouseSenateIndividual_itemized_contributions_period :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the reporting period
  , committeeReportsHouseSenateIndividual_itemized_contributions_ytd :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the year to date
  , committeeReportsHouseSenateIndividual_unitemized_contributions_period :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the reporting period
  , committeeReportsHouseSenateIndividual_unitemized_contributions_ytd :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the year to date
  , committeeReportsHouseSenateIs_amended :: Bool -- ^ False indicates that a report is the most recent. True indicates that the report has been superseded by an amendment.
  , committeeReportsHouseSenateLoan_repayments_candidate_loans_period :: Double -- ^
  , committeeReportsHouseSenateLoan_repayments_candidate_loans_ytd :: Double -- ^
  , committeeReportsHouseSenateLoan_repayments_other_loans_period :: Double -- ^
  , committeeReportsHouseSenateLoan_repayments_other_loans_ytd :: Double -- ^
  , committeeReportsHouseSenateLoans_made_by_candidate_period :: Double -- ^
  , committeeReportsHouseSenateLoans_made_by_candidate_ytd :: Double -- ^
  , committeeReportsHouseSenateMeans_filed :: Text -- ^ The method used to file with the FEC, either electronic or on paper.
  , committeeReportsHouseSenateMost_recent :: Bool -- ^
  , committeeReportsHouseSenateMost_recent_file_number :: Double -- ^
  , committeeReportsHouseSenateNet_contributions_period :: Double -- ^
  , committeeReportsHouseSenateNet_contributions_ytd :: Double -- ^
  , committeeReportsHouseSenateNet_operating_expenditures_period :: Double -- ^
  , committeeReportsHouseSenateNet_operating_expenditures_ytd :: Double -- ^
  , committeeReportsHouseSenateOffsets_to_operating_expenditures_period :: Double -- ^ Offsets to operating expenditures total for the reporting period
  , committeeReportsHouseSenateOffsets_to_operating_expenditures_ytd :: Double -- ^ Offsets to operating expenditures total for the year to date
  , committeeReportsHouseSenateOperating_expenditures_period :: Double -- ^
  , committeeReportsHouseSenateOperating_expenditures_ytd :: Double -- ^
  , committeeReportsHouseSenateOther_disbursements_period :: Double -- ^ Other disbursements total for the reporting period
  , committeeReportsHouseSenateOther_disbursements_ytd :: Double -- ^ Other disbursements total for the year to date
  , committeeReportsHouseSenateOther_political_committee_contributions_period :: Double -- ^ Other committees contributions total for the reporting period
  , committeeReportsHouseSenateOther_political_committee_contributions_ytd :: Double -- ^ Other committees contributions total for the year to date
  , committeeReportsHouseSenateOther_receipts_period :: Double -- ^
  , committeeReportsHouseSenateOther_receipts_ytd :: Double -- ^
  , committeeReportsHouseSenatePdf_url :: Text -- ^
  , committeeReportsHouseSenatePolitical_party_committee_contributions_period :: Double -- ^ Party committees contributions total for the reporting period
  , committeeReportsHouseSenatePolitical_party_committee_contributions_ytd :: Double -- ^ Party committees contributions total for the year to date
  , committeeReportsHouseSenatePrevious_file_number :: Double -- ^
  , committeeReportsHouseSenateReceipt_date :: Date -- ^ Date the FEC received the electronic or paper record
  , committeeReportsHouseSenateRefunded_individual_contributions_period :: Double -- ^ Individual refunds total for the reporting period
  , committeeReportsHouseSenateRefunded_individual_contributions_ytd :: Double -- ^ Individual refunds total for the year to date
  , committeeReportsHouseSenateRefunded_other_political_committee_contributions_period :: Double -- ^ Other committee refunds total for the reporting period
  , committeeReportsHouseSenateRefunded_other_political_committee_contributions_ytd :: Double -- ^ Other committee refunds total for the year to date
  , committeeReportsHouseSenateRefunded_political_party_committee_contributions_period :: Double -- ^ Political party refunds total for the reporting period
  , committeeReportsHouseSenateRefunded_political_party_committee_contributions_ytd :: Double -- ^ Political party refunds total for the year to date
  , committeeReportsHouseSenateRefunds_total_contributions_col_total_ytd :: Double -- ^
  , committeeReportsHouseSenateReport_form :: Text -- ^
  , committeeReportsHouseSenateReport_type :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsHouseSenateReport_type_full :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsHouseSenateReport_year :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , committeeReportsHouseSenateSubtotal_period :: Double -- ^
  , committeeReportsHouseSenateTotal_contribution_refunds_col_total_period :: Double -- ^
  , committeeReportsHouseSenateTotal_contribution_refunds_period :: Double -- ^ Total contribution refunds total for the reporting period
  , committeeReportsHouseSenateTotal_contribution_refunds_ytd :: Double -- ^ Total contribution refunds total for the year to date
  , committeeReportsHouseSenateTotal_contributions_column_total_period :: Double -- ^
  , committeeReportsHouseSenateTotal_contributions_period :: Double -- ^ Contribution total for the reporting period
  , committeeReportsHouseSenateTotal_contributions_ytd :: Double -- ^ Contribution total for the year to date
  , committeeReportsHouseSenateTotal_disbursements_period :: Double -- ^ Disbursements total for the reporting period
  , committeeReportsHouseSenateTotal_disbursements_ytd :: Double -- ^ Disbursements total for the year to date
  , committeeReportsHouseSenateTotal_individual_contributions_period :: Double -- ^ Individual contributions total for the reporting period
  , committeeReportsHouseSenateTotal_individual_contributions_ytd :: Double -- ^ Individual contributions total for the year to date
  , committeeReportsHouseSenateTotal_loan_repayments_made_period :: Double -- ^
  , committeeReportsHouseSenateTotal_loan_repayments_made_ytd :: Double -- ^
  , committeeReportsHouseSenateTotal_loans_received_period :: Double -- ^
  , committeeReportsHouseSenateTotal_loans_received_ytd :: Double -- ^
  , committeeReportsHouseSenateTotal_offsets_to_operating_expenditures_period :: Double -- ^
  , committeeReportsHouseSenateTotal_offsets_to_operating_expenditures_ytd :: Double -- ^
  , committeeReportsHouseSenateTotal_operating_expenditures_period :: Double -- ^
  , committeeReportsHouseSenateTotal_operating_expenditures_ytd :: Double -- ^
  , committeeReportsHouseSenateTotal_receipts_period :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the reporting period
  , committeeReportsHouseSenateTotal_receipts_ytd :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the year to date
  , committeeReportsHouseSenateTransfers_from_other_authorized_committee_period :: Double -- ^
  , committeeReportsHouseSenateTransfers_from_other_authorized_committee_ytd :: Double -- ^
  , committeeReportsHouseSenateTransfers_to_other_authorized_committee_period :: Double -- ^
  , committeeReportsHouseSenateTransfers_to_other_authorized_committee_ytd :: Double -- ^
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
  { committeeReportsIEOnlyBeginning_image_number           :: Text -- ^
  , committeeReportsIEOnlyCommittee_id                     :: Text -- ^
  , committeeReportsIEOnlyCommittee_name                   :: Text -- ^
  , committeeReportsIEOnlyCommittee_type                   :: Text -- ^
  , committeeReportsIEOnlyCoverage_end_date                :: Integer -- ^
  , committeeReportsIEOnlyCoverage_start_date              :: Integer -- ^
  , committeeReportsIEOnlyCsv_url                          :: Text -- ^
  , committeeReportsIEOnlyCycle                            :: Int -- ^
  , committeeReportsIEOnlyDocument_description             :: Text -- ^
  , committeeReportsIEOnlyEnd_image_number                 :: Text -- ^
  , committeeReportsIEOnlyFec_file_id                      :: Text -- ^
  , committeeReportsIEOnlyFec_url                          :: Text -- ^
  , committeeReportsIEOnlyIndependent_contributions_period :: Double -- ^
  , committeeReportsIEOnlyIndependent_expenditures_period  :: Double -- ^
  , committeeReportsIEOnlyIs_amended                       :: Bool -- ^ False indicates that a report is the most recent. True indicates that the report has been superseded by an amendment.
  , committeeReportsIEOnlyMeans_filed                      :: Text -- ^ The method used to file with the FEC, either electronic or on paper.
  , committeeReportsIEOnlyPdf_url                          :: Text -- ^
  , committeeReportsIEOnlyReceipt_date                     :: Date -- ^ Date the FEC received the electronic or paper record
  , committeeReportsIEOnlyReport_form                      :: Text -- ^
  , committeeReportsIEOnlyReport_type                      :: Text -- ^
  , committeeReportsIEOnlyReport_type_full                 :: Text -- ^
  , committeeReportsIEOnlyReport_year                      :: Int -- ^
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
  { committeeReportsPacPartyAll_loans_received_period :: Double -- ^
  , committeeReportsPacPartyAll_loans_received_ytd :: Double -- ^
  , committeeReportsPacPartyAllocated_federal_election_levin_share_period :: Double -- ^
  , committeeReportsPacPartyAmendment_chain :: [Double] -- ^  The first value in the chain is the original filing.  The ordering in the chain reflects the order the amendments were filed up to the amendment being viewed.
  , committeeReportsPacPartyAmendment_indicator :: Text -- ^
  , committeeReportsPacPartyAmendment_indicator_full :: Text -- ^
  , committeeReportsPacPartyBeginning_image_number :: Text -- ^
  , committeeReportsPacPartyCalendar_ytd :: Int -- ^
  , committeeReportsPacPartyCash_on_hand_beginning_calendar_ytd :: Double -- ^
  , committeeReportsPacPartyCash_on_hand_beginning_period :: Double -- ^ Balance for the committee at the start of the two-year period
  , committeeReportsPacPartyCash_on_hand_close_ytd :: Double -- ^
  , committeeReportsPacPartyCash_on_hand_end_period :: Double -- ^ Ending cash balance on the most recent filing
  , committeeReportsPacPartyCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeReportsPacPartyCommittee_name :: Text -- ^
  , committeeReportsPacPartyCommittee_type :: Text -- ^
  , committeeReportsPacPartyCoordinated_expenditures_by_party_committee_period :: Double -- ^
  , committeeReportsPacPartyCoordinated_expenditures_by_party_committee_ytd :: Double -- ^
  , committeeReportsPacPartyCoverage_end_date :: Integer -- ^ Ending date of the reporting period
  , committeeReportsPacPartyCoverage_start_date :: Integer -- ^ Beginning date of the reporting period
  , committeeReportsPacPartyCsv_url :: Text -- ^
  , committeeReportsPacPartyCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeReportsPacPartyDebts_owed_by_committee :: Double -- ^ Debts owed by the committee
  , committeeReportsPacPartyDebts_owed_to_committee :: Double -- ^ Debts owed to the committee
  , committeeReportsPacPartyDocument_description :: Text -- ^
  , committeeReportsPacPartyEnd_image_number :: Text -- ^
  , committeeReportsPacPartyFec_file_id :: Text -- ^
  , committeeReportsPacPartyFec_url :: Text -- ^
  , committeeReportsPacPartyFed_candidate_committee_contribution_refunds_ytd :: Double -- ^
  , committeeReportsPacPartyFed_candidate_committee_contributions_period :: Double -- ^
  , committeeReportsPacPartyFed_candidate_committee_contributions_ytd :: Double -- ^
  , committeeReportsPacPartyFed_candidate_contribution_refunds_period :: Double -- ^
  , committeeReportsPacPartyFile_number :: Int -- ^
  , committeeReportsPacPartyHtml_url :: Text -- ^ HTML link to the filing.
  , committeeReportsPacPartyIndependent_expenditures_period :: Double -- ^
  , committeeReportsPacPartyIndependent_expenditures_ytd :: Double -- ^
  , committeeReportsPacPartyIndividual_itemized_contributions_period :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the reporting period
  , committeeReportsPacPartyIndividual_itemized_contributions_ytd :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the year to date
  , committeeReportsPacPartyIndividual_unitemized_contributions_period :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the reporting period
  , committeeReportsPacPartyIndividual_unitemized_contributions_ytd :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the year to date
  , committeeReportsPacPartyIs_amended :: Bool -- ^ False indicates that a report is the most recent. True indicates that the report has been superseded by an amendment.
  , committeeReportsPacPartyLoan_repayments_made_period :: Double -- ^
  , committeeReportsPacPartyLoan_repayments_made_ytd :: Double -- ^
  , committeeReportsPacPartyLoan_repayments_received_period :: Double -- ^
  , committeeReportsPacPartyLoan_repayments_received_ytd :: Double -- ^
  , committeeReportsPacPartyLoans_made_period :: Double -- ^
  , committeeReportsPacPartyLoans_made_ytd :: Double -- ^
  , committeeReportsPacPartyMeans_filed :: Text -- ^ The method used to file with the FEC, either electronic or on paper.
  , committeeReportsPacPartyMost_recent :: Bool -- ^
  , committeeReportsPacPartyMost_recent_file_number :: Double -- ^
  , committeeReportsPacPartyNet_contributions_period :: Double -- ^
  , committeeReportsPacPartyNet_contributions_ytd :: Double -- ^
  , committeeReportsPacPartyNet_operating_expenditures_period :: Double -- ^
  , committeeReportsPacPartyNet_operating_expenditures_ytd :: Double -- ^
  , committeeReportsPacPartyNon_allocated_fed_election_activity_period :: Double -- ^
  , committeeReportsPacPartyNon_allocated_fed_election_activity_ytd :: Double -- ^
  , committeeReportsPacPartyNonfed_share_allocated_disbursements_period :: Double -- ^
  , committeeReportsPacPartyOffsets_to_operating_expenditures_period :: Double -- ^ Offsets to operating expenditures total for the reporting period
  , committeeReportsPacPartyOffsets_to_operating_expenditures_ytd :: Double -- ^ Offsets to operating expenditures total for the year to date
  , committeeReportsPacPartyOther_disbursements_period :: Double -- ^ Other disbursements total for the reporting period
  , committeeReportsPacPartyOther_disbursements_ytd :: Double -- ^ Other disbursements total for the year to date
  , committeeReportsPacPartyOther_fed_operating_expenditures_period :: Double -- ^
  , committeeReportsPacPartyOther_fed_operating_expenditures_ytd :: Double -- ^
  , committeeReportsPacPartyOther_fed_receipts_period :: Double -- ^
  , committeeReportsPacPartyOther_fed_receipts_ytd :: Double -- ^
  , committeeReportsPacPartyOther_political_committee_contributions_period :: Double -- ^ Other committees contributions total for the reporting period
  , committeeReportsPacPartyOther_political_committee_contributions_ytd :: Double -- ^ Other committees contributions total for the year to date
  , committeeReportsPacPartyPdf_url :: Text -- ^
  , committeeReportsPacPartyPolitical_party_committee_contributions_period :: Double -- ^ Party committees contributions total for the reporting period
  , committeeReportsPacPartyPolitical_party_committee_contributions_ytd :: Double -- ^ Party committees contributions total for the year to date
  , committeeReportsPacPartyPrevious_file_number :: Double -- ^
  , committeeReportsPacPartyReceipt_date :: Date -- ^ Date the FEC received the electronic or paper record
  , committeeReportsPacPartyRefunded_individual_contributions_period :: Double -- ^ Individual refunds total for the reporting period
  , committeeReportsPacPartyRefunded_individual_contributions_ytd :: Double -- ^ Individual refunds total for the year to date
  , committeeReportsPacPartyRefunded_other_political_committee_contributions_period :: Double -- ^ Other committee refunds total for the reporting period
  , committeeReportsPacPartyRefunded_other_political_committee_contributions_ytd :: Double -- ^ Other committee refunds total for the year to date
  , committeeReportsPacPartyRefunded_political_party_committee_contributions_period :: Double -- ^ Political party refunds total for the reporting period
  , committeeReportsPacPartyRefunded_political_party_committee_contributions_ytd :: Double -- ^ Political party refunds total for the year to date
  , committeeReportsPacPartyReport_form :: Text -- ^
  , committeeReportsPacPartyReport_type :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsPacPartyReport_type_full :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsPacPartyReport_year :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , committeeReportsPacPartyShared_fed_activity_nonfed_ytd :: Double -- ^
  , committeeReportsPacPartyShared_fed_activity_period :: Double -- ^
  , committeeReportsPacPartyShared_fed_activity_ytd :: Double -- ^
  , committeeReportsPacPartyShared_fed_operating_expenditures_period :: Double -- ^
  , committeeReportsPacPartyShared_fed_operating_expenditures_ytd :: Double -- ^
  , committeeReportsPacPartyShared_nonfed_operating_expenditures_period :: Double -- ^
  , committeeReportsPacPartyShared_nonfed_operating_expenditures_ytd :: Double -- ^
  , committeeReportsPacPartySubtotal_summary_page_period :: Double -- ^
  , committeeReportsPacPartySubtotal_summary_ytd :: Double -- ^
  , committeeReportsPacPartyTotal_contribution_refunds_period :: Double -- ^ Total contribution refunds total for the reporting period
  , committeeReportsPacPartyTotal_contribution_refunds_ytd :: Double -- ^ Total contribution refunds total for the year to date
  , committeeReportsPacPartyTotal_contributions_period :: Double -- ^ Contribution total for the reporting period
  , committeeReportsPacPartyTotal_contributions_ytd :: Double -- ^ Contribution total for the year to date
  , committeeReportsPacPartyTotal_disbursements_period :: Double -- ^ Disbursements total for the reporting period
  , committeeReportsPacPartyTotal_disbursements_ytd :: Double -- ^ Disbursements total for the year to date
  , committeeReportsPacPartyTotal_fed_disbursements_period :: Double -- ^
  , committeeReportsPacPartyTotal_fed_disbursements_ytd :: Double -- ^
  , committeeReportsPacPartyTotal_fed_election_activity_period :: Double -- ^
  , committeeReportsPacPartyTotal_fed_election_activity_ytd :: Double -- ^
  , committeeReportsPacPartyTotal_fed_operating_expenditures_period :: Double -- ^
  , committeeReportsPacPartyTotal_fed_operating_expenditures_ytd :: Double -- ^
  , committeeReportsPacPartyTotal_fed_receipts_period :: Double -- ^
  , committeeReportsPacPartyTotal_fed_receipts_ytd :: Double -- ^
  , committeeReportsPacPartyTotal_individual_contributions_period :: Double -- ^ Individual contributions total for the reporting period
  , committeeReportsPacPartyTotal_individual_contributions_ytd :: Double -- ^ Individual contributions total for the year to date
  , committeeReportsPacPartyTotal_nonfed_transfers_period :: Double -- ^
  , committeeReportsPacPartyTotal_nonfed_transfers_ytd :: Double -- ^
  , committeeReportsPacPartyTotal_operating_expenditures_period :: Double -- ^
  , committeeReportsPacPartyTotal_operating_expenditures_ytd :: Double -- ^
  , committeeReportsPacPartyTotal_receipts_period :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the reporting period
  , committeeReportsPacPartyTotal_receipts_ytd :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the year to date
  , committeeReportsPacPartyTransfers_from_affiliated_party_period :: Double -- ^
  , committeeReportsPacPartyTransfers_from_affiliated_party_ytd :: Double -- ^
  , committeeReportsPacPartyTransfers_from_nonfed_account_period :: Double -- ^
  , committeeReportsPacPartyTransfers_from_nonfed_account_ytd :: Double -- ^
  , committeeReportsPacPartyTransfers_from_nonfed_levin_period :: Double -- ^
  , committeeReportsPacPartyTransfers_from_nonfed_levin_ytd :: Double -- ^
  , committeeReportsPacPartyTransfers_to_affiliated_committee_period :: Double -- ^
  , committeeReportsPacPartyTransfers_to_affilitated_committees_ytd :: Double -- ^
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
  { committeeReportsPresidentialAmendment_chain :: [Double] -- ^  The first value in the chain is the original filing.  The ordering in the chain reflects the order the amendments were filed up to the amendment being viewed.
  , committeeReportsPresidentialAmendment_indicator :: Text -- ^
  , committeeReportsPresidentialAmendment_indicator_full :: Text -- ^
  , committeeReportsPresidentialBeginning_image_number :: Text -- ^
  , committeeReportsPresidentialCandidate_contribution_period :: Double -- ^
  , committeeReportsPresidentialCandidate_contribution_ytd :: Double -- ^
  , committeeReportsPresidentialCash_on_hand_beginning_period :: Double -- ^ Balance for the committee at the start of the two-year period
  , committeeReportsPresidentialCash_on_hand_end_period :: Double -- ^ Ending cash balance on the most recent filing
  , committeeReportsPresidentialCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeReportsPresidentialCommittee_name :: Text -- ^
  , committeeReportsPresidentialCommittee_type :: Text -- ^
  , committeeReportsPresidentialCoverage_end_date :: Integer -- ^ Ending date of the reporting period
  , committeeReportsPresidentialCoverage_start_date :: Integer -- ^ Beginning date of the reporting period
  , committeeReportsPresidentialCsv_url :: Text -- ^
  , committeeReportsPresidentialCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeReportsPresidentialDebts_owed_by_committee :: Double -- ^ Debts owed by the committee
  , committeeReportsPresidentialDebts_owed_to_committee :: Double -- ^ Debts owed to the committee
  , committeeReportsPresidentialDocument_description :: Text -- ^
  , committeeReportsPresidentialEnd_image_number :: Text -- ^
  , committeeReportsPresidentialExempt_legal_accounting_disbursement_period :: Double -- ^
  , committeeReportsPresidentialExempt_legal_accounting_disbursement_ytd :: Double -- ^
  , committeeReportsPresidentialExpenditure_subject_to_limits :: Double -- ^
  , committeeReportsPresidentialFec_file_id :: Text -- ^
  , committeeReportsPresidentialFec_url :: Text -- ^
  , committeeReportsPresidentialFederal_funds_period :: Double -- ^
  , committeeReportsPresidentialFederal_funds_ytd :: Double -- ^
  , committeeReportsPresidentialFile_number :: Int -- ^
  , committeeReportsPresidentialFundraising_disbursements_period :: Double -- ^
  , committeeReportsPresidentialFundraising_disbursements_ytd :: Double -- ^
  , committeeReportsPresidentialHtml_url :: Text -- ^ HTML link to the filing.
  , committeeReportsPresidentialIndividual_itemized_contributions_period :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the reporting period
  , committeeReportsPresidentialIndividual_itemized_contributions_ytd :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less. total for the year to date
  , committeeReportsPresidentialIndividual_unitemized_contributions_period :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the reporting period
  , committeeReportsPresidentialIndividual_unitemized_contributions_ytd :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total. total for the year to date
  , committeeReportsPresidentialIs_amended :: Bool -- ^ False indicates that a report is the most recent. True indicates that the report has been superseded by an amendment.
  , committeeReportsPresidentialItems_on_hand_liquidated :: Double -- ^
  , committeeReportsPresidentialLoans_received_from_candidate_period :: Double -- ^
  , committeeReportsPresidentialLoans_received_from_candidate_ytd :: Double -- ^
  , committeeReportsPresidentialMeans_filed :: Text -- ^ The method used to file with the FEC, either electronic or on paper.
  , committeeReportsPresidentialMost_recent :: Bool -- ^
  , committeeReportsPresidentialMost_recent_file_number :: Double -- ^
  , committeeReportsPresidentialNet_contributions_cycle_to_date :: Double -- ^
  , committeeReportsPresidentialNet_operating_expenditures_cycle_to_date :: Double -- ^
  , committeeReportsPresidentialOffsets_to_fundraising_expenditures_period :: Double -- ^
  , committeeReportsPresidentialOffsets_to_fundraising_expenditures_ytd :: Double -- ^
  , committeeReportsPresidentialOffsets_to_legal_accounting_period :: Double -- ^
  , committeeReportsPresidentialOffsets_to_legal_accounting_ytd :: Double -- ^
  , committeeReportsPresidentialOffsets_to_operating_expenditures_period :: Double -- ^ Offsets to operating expenditures total for the reporting period
  , committeeReportsPresidentialOffsets_to_operating_expenditures_ytd :: Double -- ^ Offsets to operating expenditures total for the year to date
  , committeeReportsPresidentialOperating_expenditures_period :: Double -- ^
  , committeeReportsPresidentialOperating_expenditures_ytd :: Double -- ^
  , committeeReportsPresidentialOther_disbursements_period :: Double -- ^ Other disbursements total for the reporting period
  , committeeReportsPresidentialOther_disbursements_ytd :: Double -- ^ Other disbursements total for the year to date
  , committeeReportsPresidentialOther_loans_received_period :: Double -- ^
  , committeeReportsPresidentialOther_loans_received_ytd :: Double -- ^
  , committeeReportsPresidentialOther_political_committee_contributions_period :: Double -- ^ Other committees contributions total for the reporting period
  , committeeReportsPresidentialOther_political_committee_contributions_ytd :: Double -- ^ Other committees contributions total for the year to date
  , committeeReportsPresidentialOther_receipts_period :: Double -- ^
  , committeeReportsPresidentialOther_receipts_ytd :: Double -- ^
  , committeeReportsPresidentialPdf_url :: Text -- ^
  , committeeReportsPresidentialPolitical_party_committee_contributions_period :: Double -- ^ Party committees contributions total for the reporting period
  , committeeReportsPresidentialPolitical_party_committee_contributions_ytd :: Double -- ^ Party committees contributions total for the year to date
  , committeeReportsPresidentialPrevious_file_number :: Double -- ^
  , committeeReportsPresidentialReceipt_date :: Date -- ^ Date the FEC received the electronic or paper record
  , committeeReportsPresidentialRefunded_individual_contributions_period :: Double -- ^ Individual refunds total for the reporting period
  , committeeReportsPresidentialRefunded_individual_contributions_ytd :: Double -- ^ Individual refunds total for the year to date
  , committeeReportsPresidentialRefunded_other_political_committee_contributions_period :: Double -- ^ Other committee refunds total for the reporting period
  , committeeReportsPresidentialRefunded_other_political_committee_contributions_ytd :: Double -- ^ Other committee refunds total for the year to date
  , committeeReportsPresidentialRefunded_political_party_committee_contributions_period :: Double -- ^ Political party refunds total for the reporting period
  , committeeReportsPresidentialRefunded_political_party_committee_contributions_ytd :: Double -- ^ Political party refunds total for the year to date
  , committeeReportsPresidentialRepayments_loans_made_by_candidate_period :: Double -- ^
  , committeeReportsPresidentialRepayments_loans_made_candidate_ytd :: Double -- ^
  , committeeReportsPresidentialRepayments_other_loans_period :: Double -- ^
  , committeeReportsPresidentialRepayments_other_loans_ytd :: Double -- ^
  , committeeReportsPresidentialReport_form :: Text -- ^
  , committeeReportsPresidentialReport_type :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsPresidentialReport_type_full :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , committeeReportsPresidentialReport_year :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , committeeReportsPresidentialSubtotal_summary_period :: Double -- ^
  , committeeReportsPresidentialTotal_contribution_refunds_period :: Double -- ^ Total contribution refunds total for the reporting period
  , committeeReportsPresidentialTotal_contribution_refunds_ytd :: Double -- ^ Total contribution refunds total for the year to date
  , committeeReportsPresidentialTotal_contributions_period :: Double -- ^ Contribution total for the reporting period
  , committeeReportsPresidentialTotal_contributions_ytd :: Double -- ^ Contribution total for the year to date
  , committeeReportsPresidentialTotal_disbursements_period :: Double -- ^ Disbursements total for the reporting period
  , committeeReportsPresidentialTotal_disbursements_ytd :: Double -- ^ Disbursements total for the year to date
  , committeeReportsPresidentialTotal_individual_contributions_period :: Double -- ^ Individual contributions total for the reporting period
  , committeeReportsPresidentialTotal_individual_contributions_ytd :: Double -- ^ Individual contributions total for the year to date
  , committeeReportsPresidentialTotal_loan_repayments_made_period :: Double -- ^
  , committeeReportsPresidentialTotal_loan_repayments_made_ytd :: Double -- ^
  , committeeReportsPresidentialTotal_loans_received_period :: Double -- ^
  , committeeReportsPresidentialTotal_loans_received_ytd :: Double -- ^
  , committeeReportsPresidentialTotal_offsets_to_operating_expenditures_period :: Double -- ^
  , committeeReportsPresidentialTotal_offsets_to_operating_expenditures_ytd :: Double -- ^
  , committeeReportsPresidentialTotal_period :: Double -- ^
  , committeeReportsPresidentialTotal_receipts_period :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the reporting period
  , committeeReportsPresidentialTotal_receipts_ytd :: Double -- ^ Anything of value (money, goods, services or property) received by a political committee total for the year to date
  , committeeReportsPresidentialTotal_ytd :: Double -- ^
  , committeeReportsPresidentialTransfers_from_affiliated_committee_period :: Double -- ^
  , committeeReportsPresidentialTransfers_from_affiliated_committee_ytd :: Double -- ^
  , committeeReportsPresidentialTransfers_to_other_authorized_committee_period :: Double -- ^
  , committeeReportsPresidentialTransfers_to_other_authorized_committee_ytd :: Double -- ^
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
  { committeeTotalsAll_loans_received                               :: Double -- ^
  , committeeTotalsAll_other_loans                                  :: Double -- ^
  , committeeTotalsAllocated_federal_election_levin_share           :: Double -- ^
  , committeeTotalsCandidate_contribution                           :: Double -- ^
  , committeeTotalsCash_on_hand_beginning_period                    :: Double -- ^
  , committeeTotalsCommittee_designation                            :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsCommittee_designation_full                       :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsCommittee_id                                     :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeTotalsCommittee_name                                   :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeTotalsCommittee_type                                   :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsCommittee_type_full                              :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsContribution_refunds                             :: Double -- ^
  , committeeTotalsContributions                                    :: Double -- ^ Contribution
  , committeeTotalsConvention_exp                                   :: Double -- ^
  , committeeTotalsCoordinated_expenditures_by_party_committee      :: Double -- ^
  , committeeTotalsCoverage_end_date                                :: Integer -- ^
  , committeeTotalsCoverage_start_date                              :: Integer -- ^
  , committeeTotalsCycle                                            :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeTotalsDisbursements                                    :: Double -- ^ Disbursements
  , committeeTotalsExempt_legal_accounting_disbursement             :: Double -- ^
  , committeeTotalsExp_prior_years_subject_limits                   :: Double -- ^
  , committeeTotalsExp_subject_limits                               :: Double -- ^
  , committeeTotalsFed_candidate_committee_contributions            :: Double -- ^
  , committeeTotalsFed_candidate_contribution_refunds               :: Double -- ^
  , committeeTotalsFed_disbursements                                :: Double -- ^
  , committeeTotalsFed_election_activity                            :: Double -- ^
  , committeeTotalsFed_operating_expenditures                       :: Double -- ^
  , committeeTotalsFed_receipts                                     :: Double -- ^
  , committeeTotalsFederal_funds                                    :: Double -- ^
  , committeeTotalsFundraising_disbursements                        :: Double -- ^
  , committeeTotalsIndependent_expenditures                         :: Double -- ^
  , committeeTotalsIndividual_contributions                         :: Double -- ^
  , committeeTotalsIndividual_itemized_contributions                :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less.
  , committeeTotalsIndividual_unitemized_contributions              :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total.
  , committeeTotalsItemized_convention_exp                          :: Double -- ^
  , committeeTotalsItemized_other_disb                              :: Double -- ^
  , committeeTotalsItemized_other_income                            :: Double -- ^
  , committeeTotalsItemized_other_refunds                           :: Double -- ^
  , committeeTotalsItemized_refunds_relating_convention_exp         :: Double -- ^
  , committeeTotalsLast_beginning_image_number                      :: Text -- ^
  , committeeTotalsLast_cash_on_hand_end_period                     :: Double -- ^
  , committeeTotalsLast_debts_owed_by_committee                     :: Double -- ^
  , committeeTotalsLast_debts_owed_to_committee                     :: Double -- ^
  , committeeTotalsLast_report_type_full                            :: Text -- ^
  , committeeTotalsLast_report_year                                 :: Int -- ^
  , committeeTotalsLoan_repayments                                  :: Double -- ^
  , committeeTotalsLoan_repayments_candidate_loans                  :: Double -- ^
  , committeeTotalsLoan_repayments_made                             :: Double -- ^
  , committeeTotalsLoan_repayments_other_loans                      :: Double -- ^
  , committeeTotalsLoan_repayments_received                         :: Double -- ^
  , committeeTotalsLoans                                            :: Double -- ^
  , committeeTotalsLoans_and_loan_repayments_made                   :: Double -- ^
  , committeeTotalsLoans_and_loan_repayments_received               :: Double -- ^
  , committeeTotalsLoans_made                                       :: Double -- ^
  , committeeTotalsLoans_made_by_candidate                          :: Double -- ^
  , committeeTotalsLoans_received                                   :: Double -- ^
  , committeeTotalsLoans_received_from_candidate                    :: Double -- ^
  , committeeTotalsNet_contributions                                :: Double -- ^
  , committeeTotalsNet_operating_expenditures                       :: Double -- ^
  , committeeTotalsNon_allocated_fed_election_activity              :: Double -- ^
  , committeeTotalsOffsets_to_fundraising_expenditures              :: Double -- ^
  , committeeTotalsOffsets_to_legal_accounting                      :: Double -- ^
  , committeeTotalsOffsets_to_operating_expenditures                :: Double -- ^
  , committeeTotalsOperating_expenditures                           :: Double -- ^
  , committeeTotalsOther_disbursements                              :: Double -- ^
  , committeeTotalsOther_fed_operating_expenditures                 :: Double -- ^
  , committeeTotalsOther_fed_receipts                               :: Double -- ^
  , committeeTotalsOther_loans_received                             :: Double -- ^
  , committeeTotalsOther_political_committee_contributions          :: Double -- ^
  , committeeTotalsOther_receipts                                   :: Double -- ^
  , committeeTotalsOther_refunds                                    :: Double -- ^
  , committeeTotalsParty_full                                       :: Text -- ^ Party affiliated with a candidate or committee
  , committeeTotalsPdf_url                                          :: Text -- ^
  , committeeTotalsPolitical_party_committee_contributions          :: Double -- ^
  , committeeTotalsReceipts                                         :: Double -- ^
  , committeeTotalsRefunded_individual_contributions                :: Double -- ^
  , committeeTotalsRefunded_other_political_committee_contributions :: Double -- ^
  , committeeTotalsRefunded_political_party_committee_contributions :: Double -- ^
  , committeeTotalsRefunds_relating_convention_exp                  :: Double -- ^
  , committeeTotalsRepayments_loans_made_by_candidate               :: Double -- ^
  , committeeTotalsRepayments_other_loans                           :: Double -- ^
  , committeeTotalsReport_form                                      :: Text -- ^
  , committeeTotalsShared_fed_activity                              :: Double -- ^
  , committeeTotalsShared_fed_activity_nonfed                       :: Double -- ^
  , committeeTotalsShared_fed_operating_expenditures                :: Double -- ^
  , committeeTotalsShared_nonfed_operating_expenditures             :: Double -- ^
  , committeeTotalsTotal_exp_subject_limits                         :: Double -- ^
  , committeeTotalsTotal_independent_contributions                  :: Double -- ^
  , committeeTotalsTotal_independent_expenditures                   :: Double -- ^
  , committeeTotalsTotal_offsets_to_operating_expenditures          :: Double -- ^
  , committeeTotalsTotal_transfers                                  :: Double -- ^
  , committeeTotalsTransaction_coverage_date                        :: Date -- ^
  , committeeTotalsTransfers_from_affiliated_committee              :: Double -- ^
  , committeeTotalsTransfers_from_affiliated_party                  :: Double -- ^
  , committeeTotalsTransfers_from_nonfed_account                    :: Double -- ^
  , committeeTotalsTransfers_from_nonfed_levin                      :: Double -- ^
  , committeeTotalsTransfers_from_other_authorized_committee        :: Double -- ^
  , committeeTotalsTransfers_to_affiliated_committee                :: Double -- ^
  , committeeTotalsTransfers_to_other_authorized_committee          :: Double -- ^
  , committeeTotalsUnitemized_convention_exp                        :: Double -- ^
  , committeeTotalsUnitemized_other_disb                            :: Double -- ^
  , committeeTotalsUnitemized_other_income                          :: Double -- ^
  , committeeTotalsUnitemized_other_refunds                         :: Double -- ^
  , committeeTotalsUnitemized_refunds_relating_convention_exp       :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommitteeTotals where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "committeeTotals")
instance ToJSON CommitteeTotals where
  toJSON = genericToJSON (removeFieldLabelPrefix False "committeeTotals")

-- |
data CommitteeTotalsHouseSenate = CommitteeTotalsHouseSenate
  { committeeTotalsHouseSenateAll_other_loans :: Double -- ^
  , committeeTotalsHouseSenateCandidate_contribution :: Double -- ^
  , committeeTotalsHouseSenateCash_on_hand_beginning_period :: Double -- ^
  , committeeTotalsHouseSenateCommittee_designation :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsHouseSenateCommittee_designation_full :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsHouseSenateCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeTotalsHouseSenateCommittee_name :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeTotalsHouseSenateCommittee_type :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsHouseSenateCommittee_type_full :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsHouseSenateContribution_refunds :: Double -- ^
  , committeeTotalsHouseSenateContributions :: Double -- ^ Contribution
  , committeeTotalsHouseSenateCoverage_end_date :: Integer -- ^
  , committeeTotalsHouseSenateCoverage_start_date :: Integer -- ^
  , committeeTotalsHouseSenateCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeTotalsHouseSenateDisbursements :: Double -- ^ Disbursements
  , committeeTotalsHouseSenateIndividual_contributions :: Double -- ^
  , committeeTotalsHouseSenateIndividual_itemized_contributions :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less.
  , committeeTotalsHouseSenateIndividual_unitemized_contributions :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total.
  , committeeTotalsHouseSenateLast_beginning_image_number :: Text -- ^
  , committeeTotalsHouseSenateLast_cash_on_hand_end_period :: Double -- ^
  , committeeTotalsHouseSenateLast_debts_owed_by_committee :: Double -- ^
  , committeeTotalsHouseSenateLast_debts_owed_to_committee :: Double -- ^
  , committeeTotalsHouseSenateLast_report_type_full :: Text -- ^
  , committeeTotalsHouseSenateLast_report_year :: Int -- ^
  , committeeTotalsHouseSenateLoan_repayments :: Double -- ^
  , committeeTotalsHouseSenateLoan_repayments_candidate_loans :: Double -- ^
  , committeeTotalsHouseSenateLoan_repayments_other_loans :: Double -- ^
  , committeeTotalsHouseSenateLoans :: Double -- ^
  , committeeTotalsHouseSenateLoans_made_by_candidate :: Double -- ^
  , committeeTotalsHouseSenateNet_contributions :: Double -- ^
  , committeeTotalsHouseSenateNet_operating_expenditures :: Double -- ^
  , committeeTotalsHouseSenateOffsets_to_operating_expenditures :: Double -- ^
  , committeeTotalsHouseSenateOperating_expenditures :: Double -- ^
  , committeeTotalsHouseSenateOther_disbursements :: Double -- ^
  , committeeTotalsHouseSenateOther_political_committee_contributions :: Double -- ^
  , committeeTotalsHouseSenateOther_receipts :: Double -- ^
  , committeeTotalsHouseSenateParty_full :: Text -- ^ Party affiliated with a candidate or committee
  , committeeTotalsHouseSenatePdf_url :: Text -- ^
  , committeeTotalsHouseSenatePolitical_party_committee_contributions :: Double -- ^
  , committeeTotalsHouseSenateReceipts :: Double -- ^
  , committeeTotalsHouseSenateRefunded_individual_contributions :: Double -- ^
  , committeeTotalsHouseSenateRefunded_other_political_committee_contributions :: Double -- ^
  , committeeTotalsHouseSenateRefunded_political_party_committee_contributions :: Double -- ^
  , committeeTotalsHouseSenateReport_form :: Text -- ^
  , committeeTotalsHouseSenateTransaction_coverage_date :: Date -- ^
  , committeeTotalsHouseSenateTransfers_from_other_authorized_committee :: Double -- ^
  , committeeTotalsHouseSenateTransfers_to_other_authorized_committee :: Double -- ^
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
  { committeeTotalsIEOnlyCommittee_id                    :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeTotalsIEOnlyCoverage_end_date               :: Integer -- ^ Ending date of the reporting period
  , committeeTotalsIEOnlyCoverage_start_date             :: Integer -- ^ Beginning date of the reporting period
  , committeeTotalsIEOnlyCycle                           :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeTotalsIEOnlyLast_beginning_image_number     :: Text -- ^
  , committeeTotalsIEOnlyLast_cash_on_hand_end_period    :: Double -- ^
  , committeeTotalsIEOnlyPdf_url                         :: Text -- ^
  , committeeTotalsIEOnlyReport_form                     :: Text -- ^
  , committeeTotalsIEOnlyTotal_independent_contributions :: Double -- ^
  , committeeTotalsIEOnlyTotal_independent_expenditures  :: Double -- ^
  , committeeTotalsIEOnlyTransaction_coverage_date       :: Date -- ^
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
  { committeeTotalsPacPartyAll_loans_received :: Double -- ^
  , committeeTotalsPacPartyAllocated_federal_election_levin_share :: Double -- ^
  , committeeTotalsPacPartyCash_on_hand_beginning_period :: Double -- ^
  , committeeTotalsPacPartyCommittee_designation :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsPacPartyCommittee_designation_full :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsPacPartyCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeTotalsPacPartyCommittee_name :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeTotalsPacPartyCommittee_type :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsPacPartyCommittee_type_full :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsPacPartyContribution_refunds :: Double -- ^
  , committeeTotalsPacPartyContributions :: Double -- ^ Contribution
  , committeeTotalsPacPartyConvention_exp :: Double -- ^
  , committeeTotalsPacPartyCoordinated_expenditures_by_party_committee :: Double -- ^
  , committeeTotalsPacPartyCoverage_end_date :: Integer -- ^
  , committeeTotalsPacPartyCoverage_start_date :: Integer -- ^
  , committeeTotalsPacPartyCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeTotalsPacPartyDisbursements :: Double -- ^ Disbursements
  , committeeTotalsPacPartyExp_prior_years_subject_limits :: Double -- ^
  , committeeTotalsPacPartyExp_subject_limits :: Double -- ^
  , committeeTotalsPacPartyFed_candidate_committee_contributions :: Double -- ^
  , committeeTotalsPacPartyFed_candidate_contribution_refunds :: Double -- ^
  , committeeTotalsPacPartyFed_disbursements :: Double -- ^
  , committeeTotalsPacPartyFed_election_activity :: Double -- ^
  , committeeTotalsPacPartyFed_operating_expenditures :: Double -- ^
  , committeeTotalsPacPartyFed_receipts :: Double -- ^
  , committeeTotalsPacPartyFederal_funds :: Double -- ^
  , committeeTotalsPacPartyIndependent_expenditures :: Double -- ^
  , committeeTotalsPacPartyIndividual_contributions :: Double -- ^
  , committeeTotalsPacPartyIndividual_itemized_contributions :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less.
  , committeeTotalsPacPartyIndividual_unitemized_contributions :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total.
  , committeeTotalsPacPartyItemized_convention_exp :: Double -- ^
  , committeeTotalsPacPartyItemized_other_disb :: Double -- ^
  , committeeTotalsPacPartyItemized_other_income :: Double -- ^
  , committeeTotalsPacPartyItemized_other_refunds :: Double -- ^
  , committeeTotalsPacPartyItemized_refunds_relating_convention_exp :: Double -- ^
  , committeeTotalsPacPartyLast_beginning_image_number :: Text -- ^
  , committeeTotalsPacPartyLast_cash_on_hand_end_period :: Double -- ^
  , committeeTotalsPacPartyLast_debts_owed_by_committee :: Double -- ^
  , committeeTotalsPacPartyLast_debts_owed_to_committee :: Double -- ^
  , committeeTotalsPacPartyLast_report_type_full :: Text -- ^
  , committeeTotalsPacPartyLast_report_year :: Int -- ^
  , committeeTotalsPacPartyLoan_repayments_made :: Double -- ^
  , committeeTotalsPacPartyLoan_repayments_received :: Double -- ^
  , committeeTotalsPacPartyLoans_and_loan_repayments_made :: Double -- ^
  , committeeTotalsPacPartyLoans_and_loan_repayments_received :: Double -- ^
  , committeeTotalsPacPartyLoans_made :: Double -- ^
  , committeeTotalsPacPartyNet_contributions :: Double -- ^
  , committeeTotalsPacPartyNet_operating_expenditures :: Double -- ^
  , committeeTotalsPacPartyNon_allocated_fed_election_activity :: Double -- ^
  , committeeTotalsPacPartyOffsets_to_operating_expenditures :: Double -- ^
  , committeeTotalsPacPartyOperating_expenditures :: Double -- ^
  , committeeTotalsPacPartyOther_disbursements :: Double -- ^
  , committeeTotalsPacPartyOther_fed_operating_expenditures :: Double -- ^
  , committeeTotalsPacPartyOther_fed_receipts :: Double -- ^
  , committeeTotalsPacPartyOther_political_committee_contributions :: Double -- ^
  , committeeTotalsPacPartyOther_refunds :: Double -- ^
  , committeeTotalsPacPartyParty_full :: Text -- ^ Party affiliated with a candidate or committee
  , committeeTotalsPacPartyPdf_url :: Text -- ^
  , committeeTotalsPacPartyPolitical_party_committee_contributions :: Double -- ^
  , committeeTotalsPacPartyReceipts :: Double -- ^
  , committeeTotalsPacPartyRefunded_individual_contributions :: Double -- ^
  , committeeTotalsPacPartyRefunded_other_political_committee_contributions :: Double -- ^
  , committeeTotalsPacPartyRefunded_political_party_committee_contributions :: Double -- ^
  , committeeTotalsPacPartyRefunds_relating_convention_exp :: Double -- ^
  , committeeTotalsPacPartyReport_form :: Text -- ^
  , committeeTotalsPacPartyShared_fed_activity :: Double -- ^
  , committeeTotalsPacPartyShared_fed_activity_nonfed :: Double -- ^
  , committeeTotalsPacPartyShared_fed_operating_expenditures :: Double -- ^
  , committeeTotalsPacPartyShared_nonfed_operating_expenditures :: Double -- ^
  , committeeTotalsPacPartyTotal_exp_subject_limits :: Double -- ^
  , committeeTotalsPacPartyTotal_transfers :: Double -- ^
  , committeeTotalsPacPartyTransaction_coverage_date :: Date -- ^
  , committeeTotalsPacPartyTransfers_from_affiliated_party :: Double -- ^
  , committeeTotalsPacPartyTransfers_from_nonfed_account :: Double -- ^
  , committeeTotalsPacPartyTransfers_from_nonfed_levin :: Double -- ^
  , committeeTotalsPacPartyTransfers_to_affiliated_committee :: Double -- ^
  , committeeTotalsPacPartyUnitemized_convention_exp :: Double -- ^
  , committeeTotalsPacPartyUnitemized_other_disb :: Double -- ^
  , committeeTotalsPacPartyUnitemized_other_income :: Double -- ^
  , committeeTotalsPacPartyUnitemized_other_refunds :: Double -- ^
  , committeeTotalsPacPartyUnitemized_refunds_relating_convention_exp :: Double -- ^
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
  { committeeTotalsPresidentialCandidate_contribution :: Double -- ^
  , committeeTotalsPresidentialCash_on_hand_beginning_period :: Double -- ^
  , committeeTotalsPresidentialCommittee_designation :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsPresidentialCommittee_designation_full :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , committeeTotalsPresidentialCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , committeeTotalsPresidentialCommittee_name :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , committeeTotalsPresidentialCommittee_type :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsPresidentialCommittee_type_full :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , committeeTotalsPresidentialContribution_refunds :: Double -- ^
  , committeeTotalsPresidentialContributions :: Double -- ^ Contribution
  , committeeTotalsPresidentialCoverage_end_date :: Integer -- ^
  , committeeTotalsPresidentialCoverage_start_date :: Integer -- ^
  , committeeTotalsPresidentialCycle :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , committeeTotalsPresidentialDisbursements :: Double -- ^ Disbursements
  , committeeTotalsPresidentialExempt_legal_accounting_disbursement :: Double -- ^
  , committeeTotalsPresidentialFederal_funds :: Double -- ^
  , committeeTotalsPresidentialFundraising_disbursements :: Double -- ^
  , committeeTotalsPresidentialIndividual_contributions :: Double -- ^
  , committeeTotalsPresidentialIndividual_itemized_contributions :: Double -- ^ Individual itemized contributions are from individuals whose aggregate contributions total over $200 per individual per year. Be aware, some filers choose to itemize donations $200 or less.
  , committeeTotalsPresidentialIndividual_unitemized_contributions :: Double -- ^ Unitemized contributions are made individuals whose aggregate contributions total $200 or less per individual per year. Be aware, some filers choose to itemize donations $200 or less and in that case those donations will appear in the itemized total.
  , committeeTotalsPresidentialLast_beginning_image_number :: Text -- ^
  , committeeTotalsPresidentialLast_cash_on_hand_end_period :: Double -- ^
  , committeeTotalsPresidentialLast_debts_owed_by_committee :: Double -- ^
  , committeeTotalsPresidentialLast_debts_owed_to_committee :: Double -- ^
  , committeeTotalsPresidentialLast_report_type_full :: Text -- ^
  , committeeTotalsPresidentialLast_report_year :: Int -- ^
  , committeeTotalsPresidentialLoan_repayments_made :: Double -- ^
  , committeeTotalsPresidentialLoans_received :: Double -- ^
  , committeeTotalsPresidentialLoans_received_from_candidate :: Double -- ^
  , committeeTotalsPresidentialNet_contributions :: Double -- ^
  , committeeTotalsPresidentialNet_operating_expenditures :: Double -- ^
  , committeeTotalsPresidentialOffsets_to_fundraising_expenditures :: Double -- ^
  , committeeTotalsPresidentialOffsets_to_legal_accounting :: Double -- ^
  , committeeTotalsPresidentialOffsets_to_operating_expenditures :: Double -- ^
  , committeeTotalsPresidentialOperating_expenditures :: Double -- ^
  , committeeTotalsPresidentialOther_disbursements :: Double -- ^
  , committeeTotalsPresidentialOther_loans_received :: Double -- ^
  , committeeTotalsPresidentialOther_political_committee_contributions :: Double -- ^
  , committeeTotalsPresidentialOther_receipts :: Double -- ^
  , committeeTotalsPresidentialParty_full :: Text -- ^ Party affiliated with a candidate or committee
  , committeeTotalsPresidentialPdf_url :: Text -- ^
  , committeeTotalsPresidentialPolitical_party_committee_contributions :: Double -- ^
  , committeeTotalsPresidentialReceipts :: Double -- ^
  , committeeTotalsPresidentialRefunded_individual_contributions :: Double -- ^
  , committeeTotalsPresidentialRefunded_other_political_committee_contributions :: Double -- ^
  , committeeTotalsPresidentialRefunded_political_party_committee_contributions :: Double -- ^
  , committeeTotalsPresidentialRepayments_loans_made_by_candidate :: Double -- ^
  , committeeTotalsPresidentialRepayments_other_loans :: Double -- ^
  , committeeTotalsPresidentialReport_form :: Text -- ^
  , committeeTotalsPresidentialTotal_offsets_to_operating_expenditures :: Double -- ^
  , committeeTotalsPresidentialTransaction_coverage_date :: Date -- ^
  , committeeTotalsPresidentialTransfers_from_affiliated_committee :: Double -- ^
  , committeeTotalsPresidentialTransfers_to_other_authorized_committee :: Double -- ^
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
  { communicationCostAction_code                           :: Text -- ^
  , communicationCostAction_code_full                      :: Text -- ^
  , communicationCostCandidate_first_name                  :: Text -- ^
  , communicationCostCandidate_id                          :: Text -- ^
  , communicationCostCandidate_last_name                   :: Text -- ^
  , communicationCostCandidate_middle_name                 :: Text -- ^
  , communicationCostCandidate_name                        :: Text -- ^
  , communicationCostCandidate_office                      :: Text -- ^
  , communicationCostCandidate_office_district             :: Text -- ^
  , communicationCostCandidate_office_full                 :: Text -- ^
  , communicationCostCandidate_office_state                :: Text -- ^
  , communicationCostCommittee_id                          :: Text -- ^
  , communicationCostCommittee_name                        :: Text -- ^
  , communicationCostCommunication_class                   :: Text -- ^
  , communicationCostCommunication_type                    :: Text -- ^
  , communicationCostCommunication_type_full               :: Text -- ^
  , communicationCostCycle                                 :: Int -- ^
  , communicationCostFile_number                           :: Int -- ^
  , communicationCostForm_type_code                        :: Text -- ^
  , communicationCostImage_number                          :: Text -- ^
  , communicationCostOriginal_sub_id                       :: Int -- ^
  , communicationCostPdf_url                               :: Text -- ^
  , communicationCostPrimary_general_indicator             :: Text -- ^
  , communicationCostPrimary_general_indicator_description :: Text -- ^
  , communicationCostPurpose                               :: Text -- ^
  , communicationCostReport_type                           :: Text -- ^
  , communicationCostReport_year                           :: Int -- ^
  , communicationCostSchedule_type                         :: Text -- ^
  , communicationCostSchedule_type_full                    :: Text -- ^
  , communicationCostState_full                            :: Text -- ^
  , communicationCostSub_id                                :: Int -- ^
  , communicationCostSupport_oppose_indicator              :: Text -- ^
  , communicationCostTran_id                               :: Text -- ^
  , communicationCostTransaction_amount                    :: Double -- ^
  , communicationCostTransaction_date                      :: Date -- ^
  , communicationCostTransaction_type                      :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON CommunicationCost where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "communicationCost")
instance ToJSON CommunicationCost where
  toJSON = genericToJSON (removeFieldLabelPrefix False "communicationCost")

-- |
data CommunicationCostByCandidate = CommunicationCostByCandidate
  { communicationCostByCandidateCandidate_id             :: Text -- ^
  , communicationCostByCandidateCandidate_name           :: Text -- ^
  , communicationCostByCandidateCommittee_id             :: Text -- ^
  , communicationCostByCandidateCommittee_name           :: Text -- ^
  , communicationCostByCandidateCount                    :: Int -- ^ Number of records making up the total
  , communicationCostByCandidateCycle                    :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , communicationCostByCandidateSupport_oppose_indicator :: Text -- ^ Explains if the money was spent in order to support or oppose a candidate or candidates. (Coded S or O for support or oppose.) This indicator applies to independent expenditures and communication costs.
  , communicationCostByCandidateTotal                    :: Double -- ^
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
  { eFilingsAmended_by             :: Int -- ^
  , eFilingsAmendment_chain        :: [Int] -- ^
  , eFilingsAmendment_number       :: Int -- ^  Number of times the report has been amended.
  , eFilingsAmends_file            :: Int -- ^  For amendments, this file_number is the file_number of the previous report that is being amended. See amended_by for the most recent version of the report.
  , eFilingsBeginning_image_number :: Text -- ^
  , eFilingsCommittee_id           :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , eFilingsCommittee_name         :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , eFilingsCoverage_end_date      :: Date -- ^ Ending date of the reporting period
  , eFilingsCoverage_start_date    :: Date -- ^ Beginning date of the reporting period
  , eFilingsCsv_url                :: Text -- ^
  , eFilingsDocument_description   :: Text -- ^
  , eFilingsEnding_image_number    :: Text -- ^
  , eFilingsFec_file_id            :: Text -- ^
  , eFilingsFec_url                :: Text -- ^
  , eFilingsFile_number            :: Int -- ^ Filing ID number
  , eFilingsForm_type              :: Text -- ^  Indicates the type of form that was filed. ex: F1, F2, F3P, F3X etc...
  , eFilingsHtml_url               :: Text -- ^
  , eFilingsIs_amended             :: Bool -- ^
  , eFilingsLoad_timestamp         :: Integer -- ^ Date the information was loaded into the FEC systems. This can be affected by reseting systems and other factors, refer to receipt_date for the day that the FEC received the paper or electronic document. Keep in mind that paper filings take more time to process and there can be a lag between load_date and receipt_date. This field can be helpful to identify paper records that have been processed recently.
  , eFilingsMost_recent            :: Bool -- ^
  , eFilingsMost_recent_filing     :: Int -- ^
  , eFilingsPdf_url                :: Text -- ^
  , eFilingsReceipt_date           :: Date -- ^ Date the FEC received the electronic or paper record
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
  { efilingsAmendmentsAmendment_chain      :: [Double] -- ^
  , efilingsAmendmentsDepth                :: Double -- ^
  , efilingsAmendmentsFile_number          :: Int -- ^ Filing ID number
  , efilingsAmendmentsLast                 :: Double -- ^
  , efilingsAmendmentsLongest_chain        :: [Double] -- ^
  , efilingsAmendmentsMost_recent_filing   :: Double -- ^
  , efilingsAmendmentsPrevious_file_number :: Double -- ^
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
  { electionCandidate_election_year  :: Int -- ^
  , electionCandidate_id             :: Text -- ^
  , electionCandidate_name           :: Text -- ^
  , electionCandidate_pcc_id         :: Text -- ^
  , electionCandidate_pcc_name       :: Text -- ^
  , electionCash_on_hand_end_period  :: Double -- ^
  , electionCommittee_ids            :: [Text] -- ^
  , electionCoverage_end_date        :: Date -- ^
  , electionIncumbent_challenge_full :: Text -- ^
  , electionParty_full               :: Text -- ^
  , electionTotal_disbursements      :: Double -- ^
  , electionTotal_receipts           :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Election where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "election")
instance ToJSON Election where
  toJSON = genericToJSON (removeFieldLabelPrefix False "election")

-- |
data ElectionDate = ElectionDate
  { electionDateActive_election      :: Bool -- ^
  , electionDateCreate_date          :: Integer -- ^ Date the record was created
  , electionDateElection_date        :: Date -- ^ Date of election
  , electionDateElection_district    :: Int -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , electionDateElection_notes       :: Text -- ^
  , electionDateElection_party       :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , electionDateElection_state       :: Text -- ^ US state or territory where a candidate runs for office
  , electionDateElection_type_full   :: Text -- ^
  , electionDateElection_type_id     :: Text -- ^ Election type  Convention, Primary, General, Special, Runoff etc.
  , electionDateElection_year        :: Int -- ^ Year of election
  , electionDateOffice_sought        :: Text -- ^ Federal office candidate runs for: H, S or P
  , electionDatePrimary_general_date :: Date -- ^
  , electionDateUpdate_date          :: Integer -- ^ Date the record was updated
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
  { electionSearchCandidate_status :: Text -- ^
  , electionSearchCycle            :: Int -- ^
  , electionSearchDistrict         :: Text -- ^
  , electionSearchIncumbent_id     :: Text -- ^
  , electionSearchIncumbent_name   :: Text -- ^
  , electionSearchOffice           :: Text -- ^
  , electionSearchState            :: Text -- ^
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
  { electionSummaryCount                    :: Int -- ^
  , electionSummaryDisbursements            :: Double -- ^
  , electionSummaryIndependent_expenditures :: Double -- ^
  , electionSummaryReceipts                 :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ElectionSummary where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electionSummary")
instance ToJSON ElectionSummary where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electionSummary")

-- |
data Electioneering = Electioneering
  { electioneeringAmendment_indicator        :: Text -- ^
  , electioneeringBeginning_image_number     :: Text -- ^
  , electioneeringCalculated_candidate_share :: Double -- ^ If an electioneering cost targets several candidates, the total cost is divided by the number of candidates. If it only mentions one candidate the full cost of the communication is listed.
  , electioneeringCandidate_district         :: Text -- ^
  , electioneeringCandidate_id               :: Text -- ^
  , electioneeringCandidate_name             :: Text -- ^
  , electioneeringCandidate_office           :: Text -- ^
  , electioneeringCandidate_state            :: Text -- ^
  , electioneeringCommittee_id               :: Text -- ^
  , electioneeringCommittee_name             :: Text -- ^
  , electioneeringCommunication_date         :: Date -- ^ It is the airing, broadcast, cablecast or other dissemination of the communication
  , electioneeringDisbursement_amount        :: Double -- ^
  , electioneeringDisbursement_date          :: Date -- ^ Disbursement date includes actual disbursements and execution of contracts creating an obligation to make disbursements (SB date of disbursement)
  , electioneeringElection_type              :: Text -- ^
  , electioneeringFile_number                :: Int -- ^
  , electioneeringLink_id                    :: Int -- ^
  , electioneeringNumber_of_candidates       :: Double -- ^
  , electioneeringPdf_url                    :: Text -- ^
  , electioneeringPublic_distribution_date   :: Date -- ^ The pubic distribution date is the date that triggers disclosure of the electioneering communication (date reported on page 1 of Form 9)
  , electioneeringPurpose_description        :: Text -- ^
  , electioneeringReceipt_date               :: Date -- ^
  , electioneeringReport_year                :: Int -- ^
  , electioneeringSb_image_num               :: Text -- ^
  , electioneeringSb_link_id                 :: Text -- ^
  , electioneeringSub_id                     :: Int -- ^ The identifier for each electioneering record
  } deriving (Show, Eq, Generic)

instance FromJSON Electioneering where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "electioneering")
instance ToJSON Electioneering where
  toJSON = genericToJSON (removeFieldLabelPrefix False "electioneering")

-- |
data ElectioneeringByCandidate = ElectioneeringByCandidate
  { electioneeringByCandidateCandidate_id   :: Text -- ^
  , electioneeringByCandidateCandidate_name :: Text -- ^
  , electioneeringByCandidateCommittee_id   :: Text -- ^
  , electioneeringByCandidateCommittee_name :: Text -- ^
  , electioneeringByCandidateCount          :: Int -- ^ Number of records making up the total
  , electioneeringByCandidateCycle          :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , electioneeringByCandidateTotal          :: Double -- ^
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
  { entityReceiptDisbursementTotalsCumulative_candidate_disbursements :: Float -- ^ Cumulative candidate disbursements in a two year period, adjusted to avoid double counting.
  , entityReceiptDisbursementTotalsCumulative_candidate_receipts      :: Float -- ^ Cumulative candidate receipts in a two year period, adjusted to avoid double counting.
  , entityReceiptDisbursementTotalsCumulative_pac_disbursements       :: Float -- ^ Cumulative PAC disbursements in a two year period, adjusted to avoid double counting.
  , entityReceiptDisbursementTotalsCumulative_pac_receipts            :: Float -- ^ Cumulative PAC recipts in a two year period, adjusted to avoid double counting.
  , entityReceiptDisbursementTotalsCumulative_party_disbursements     :: Float -- ^ Cumulative party disbursements in a two year period, adjusted to avoid double counting.
  , entityReceiptDisbursementTotalsCumulative_party_receipts          :: Float -- ^ Cumulative party receipts in a two year period, adjusted to avoid double counting.
  , entityReceiptDisbursementTotalsCycle                              :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , entityReceiptDisbursementTotalsEnd_date                           :: Date -- ^
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
  { filingsAmendment_chain                :: [Double] -- ^
  , filingsAmendment_indicator            :: Text -- ^  The first value in the chain is the original filing.  The ordering in the chain reflects the order the amendments were filed up to the amendment being viewed.
  , filingsAmendment_version              :: Int -- ^
  , filingsBeginning_image_number         :: Text -- ^
  , filingsCandidate_id                   :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , filingsCandidate_name                 :: Text -- ^ Name of candidate running for office
  , filingsCash_on_hand_beginning_period  :: Double -- ^ Balance for the committee at the start of the two-year period
  , filingsCash_on_hand_end_period        :: Double -- ^ Ending cash balance on the most recent filing
  , filingsCmte_tp                        :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , filingsCommittee_id                   :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , filingsCommittee_name                 :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , filingsCoverage_end_date              :: Date -- ^ Ending date of the reporting period
  , filingsCoverage_start_date            :: Date -- ^ Beginning date of the reporting period
  , filingsCsv_url                        :: Text -- ^
  , filingsCycle                          :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , filingsDebts_owed_by_committee        :: Double -- ^ Debts owed by the committee
  , filingsDebts_owed_to_committee        :: Double -- ^ Debts owed to the committee
  , filingsDocument_description           :: Text -- ^
  , filingsDocument_type                  :: Text -- ^  The type of document for documents other than reports:     - 2 24 Hour Contribution Notice     - 4 48 Hour Contribution Notice     - A Debt Settlement Statement     - B Acknowledgment of Receipt of Debt Settlement Statement     - C RFAI: Debt Settlement First Notice     - D Commission Debt Settlement Review     - E Commission Response TO Debt Settlement Request     - F Administrative Termination     - G Debt Settlement Plan Amendment     - H Disavowal Notice     - I Disavowal Response     - J Conduit Report     - K Termination Approval     - L Repeat Non-Filer Notice     - M Filing Frequency Change Notice     - N Paper Amendment to Electronic Report     - O Acknowledgment of Filing Frequency Change     - S RFAI: Debt Settlement Second     - T Miscellaneous Report TO FEC     - V Repeat Violation Notice (441A OR 441B)     - P Notice of Paper Filing     - R F3L Filing Frequency Change Notice     - Q Acknowledgment of F3L Filing Frequency Change     - U Unregistered Committee Notice
  , filingsDocument_type_full             :: Text -- ^  The type of document for documents other than reports:     - 2 24 Hour Contribution Notice     - 4 48 Hour Contribution Notice     - A Debt Settlement Statement     - B Acknowledgment of Receipt of Debt Settlement Statement     - C RFAI: Debt Settlement First Notice     - D Commission Debt Settlement Review     - E Commission Response TO Debt Settlement Request     - F Administrative Termination     - G Debt Settlement Plan Amendment     - H Disavowal Notice     - I Disavowal Response     - J Conduit Report     - K Termination Approval     - L Repeat Non-Filer Notice     - M Filing Frequency Change Notice     - N Paper Amendment to Electronic Report     - O Acknowledgment of Filing Frequency Change     - S RFAI: Debt Settlement Second     - T Miscellaneous Report TO FEC     - V Repeat Violation Notice (441A OR 441B)     - P Notice of Paper Filing     - R F3L Filing Frequency Change Notice     - Q Acknowledgment of F3L Filing Frequency Change     - U Unregistered Committee Notice
  , filingsElection_year                  :: Int -- ^ Year of election
  , filingsEnding_image_number            :: Text -- ^
  , filingsFec_file_id                    :: Text -- ^
  , filingsFec_url                        :: Text -- ^
  , filingsFile_number                    :: Int -- ^
  , filingsForm_type                      :: Text -- ^  Indicates the type of form that was filed. ex: F1, F2, F3P, F3X etc...
  , filingsHouse_personal_funds           :: Double -- ^
  , filingsHtml_url                       :: Text -- ^ HTML link to the filing.
  , filingsIs_amended                     :: Bool -- ^
  , filingsMeans_filed                    :: Text -- ^ The method used to file with the FEC, either electronic or on paper.
  , filingsMost_recent                    :: Bool -- ^
  , filingsMost_recent_file_number        :: Int -- ^
  , filingsNet_donations                  :: Double -- ^
  , filingsOffice                         :: Text -- ^ Federal office candidate runs for: H, S or P
  , filingsOpposition_personal_funds      :: Double -- ^
  , filingsPages                          :: Int -- ^ Number of pages in the document
  , filingsParty                          :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , filingsPdf_url                        :: Text -- ^
  , filingsPrevious_file_number           :: Int -- ^
  , filingsPrimary_general_indicator      :: Text -- ^
  , filingsReceipt_date                   :: Date -- ^ Date the FEC received the electronic or paper record
  , filingsReport_type                    :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , filingsReport_type_full               :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , filingsReport_year                    :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , filingsRequest_type                   :: Text -- ^
  , filingsSenate_personal_funds          :: Double -- ^
  , filingsState                          :: Text -- ^ US state or territory where a candidate runs for office
  , filingsSub_id                         :: Text -- ^
  , filingsTotal_communication_cost       :: Double -- ^
  , filingsTotal_disbursements            :: Double -- ^
  , filingsTotal_independent_expenditures :: Double -- ^
  , filingsTotal_individual_contributions :: Double -- ^
  , filingsTotal_receipts                 :: Double -- ^
  , filingsTreasurer_name                 :: Text -- ^ Name of the Committee's treasurer. If multiple treasurers for the committee, the most recent treasurer will be shown.
  , filingsUpdate_date                    :: Date -- ^
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
  { inlineResponseDefault1Admin_fines :: [Inline_response_default_1_admin_fines] -- ^
  , inlineResponseDefault1Adrs :: [Inline_response_default_1_adrs] -- ^
  , inlineResponseDefault1Advisory_opinions :: [Inline_response_default_1_advisory_opinions] -- ^
  , inlineResponseDefault1Murs :: [Inline_response_default_1_murs] -- ^
  , inlineResponseDefault1Regulations :: [Inline_response_default_1_regulations] -- ^
  , inlineResponseDefault1Statutes :: [Inline_response_default_1_statutes] -- ^
  , inlineResponseDefault1Total_admin_fines :: Int -- ^ Total number of Admin Fines matching the search criteria
  , inlineResponseDefault1Total_adrs :: Int -- ^ Total number of ADRs matching the search criteria
  , inlineResponseDefault1Total_advisory_opinions :: Int -- ^ Total number of Advisory Opinions matching the search criteria
  , inlineResponseDefault1Total_all :: Int -- ^ Total number of legal documents matching the search criteria
  , inlineResponseDefault1Total_murs :: Int -- ^ Total number of MURs matching the search criteria
  , inlineResponseDefault1Total_regulations :: Int -- ^ Total number of Regulations matching the search criteria
  , inlineResponseDefault1Total_statutes :: Int -- ^ Total number of Statutes matching the search criteria
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1 where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1")
instance ToJSON Inline_response_default_1 where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1")

-- |
data Inline_response_default_1_admin_fines = Inline_response_default_1_admin_fines
  { inlineResponseDefault1AdminFinesChallenge_outcome :: Text -- ^
  , inlineResponseDefault1AdminFinesChallenge_receipt_date :: Date -- ^
  , inlineResponseDefault1AdminFinesCheck_amount :: Double -- ^
  , inlineResponseDefault1AdminFinesCommission_votes :: [Inline_response_default_1_commission_votes] -- ^
  , inlineResponseDefault1AdminFinesCommittee_id :: Text -- ^
  , inlineResponseDefault1AdminFinesDoc_id :: Text -- ^
  , inlineResponseDefault1AdminFinesDocument_highlights :: Value -- ^
  , inlineResponseDefault1AdminFinesDocuments :: [Inline_response_default_1_documents] -- ^
  , inlineResponseDefault1AdminFinesFinal_determination_amount :: Double -- ^
  , inlineResponseDefault1AdminFinesFinal_determination_date :: Date -- ^
  , inlineResponseDefault1AdminFinesHighlights :: [Text] -- ^
  , inlineResponseDefault1AdminFinesName :: Text -- ^
  , inlineResponseDefault1AdminFinesNo :: Text -- ^
  , inlineResponseDefault1AdminFinesPetition_court_decision_date :: Date -- ^
  , inlineResponseDefault1AdminFinesPetition_court_filing_date :: Date -- ^
  , inlineResponseDefault1AdminFinesReason_to_believe_action_date :: Date -- ^
  , inlineResponseDefault1AdminFinesReason_to_believe_fine_amount :: Double -- ^
  , inlineResponseDefault1AdminFinesReport_type :: Text -- ^
  , inlineResponseDefault1AdminFinesReport_year :: Text -- ^
  , inlineResponseDefault1AdminFinesTreasury_referral_amount :: Double -- ^
  , inlineResponseDefault1AdminFinesTreasury_referral_date :: Date -- ^
  , inlineResponseDefault1AdminFinesUrl :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_admin_fines where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1AdminFines")
instance ToJSON Inline_response_default_1_admin_fines where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1AdminFines")

-- |
data Inline_response_default_1_adrs = Inline_response_default_1_adrs
  { inlineResponseDefault1AdrsClose_date :: Date -- ^
  , inlineResponseDefault1AdrsCommission_votes :: [Inline_response_default_1_commission_votes] -- ^
  , inlineResponseDefault1AdrsDispositions :: [Inline_response_default_1_dispositions] -- ^
  , inlineResponseDefault1AdrsDoc_id :: Text -- ^
  , inlineResponseDefault1AdrsDocument_highlights :: Value -- ^
  , inlineResponseDefault1AdrsDocuments :: [Inline_response_default_1_documents] -- ^
  , inlineResponseDefault1AdrsElection_cycles :: Int -- ^
  , inlineResponseDefault1AdrsHighlights :: [Text] -- ^
  , inlineResponseDefault1AdrsName :: Text -- ^
  , inlineResponseDefault1AdrsNo :: Text -- ^
  , inlineResponseDefault1AdrsOpen_date :: Date -- ^
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
  { inlineResponseDefault1AdvisoryOpinionsAo_citations :: [Inline_response_default_1_ao_citations] -- ^
  , inlineResponseDefault1AdvisoryOpinionsAos_cited_by :: [Inline_response_default_1_ao_citations] -- ^
  , inlineResponseDefault1AdvisoryOpinionsCommenter_names :: [Text] -- ^
  , inlineResponseDefault1AdvisoryOpinionsDocument_highlights :: Value -- ^
  , inlineResponseDefault1AdvisoryOpinionsDocuments :: [Inline_response_default_1_documents_1] -- ^
  , inlineResponseDefault1AdvisoryOpinionsEntities :: [Inline_response_default_1_entities] -- ^
  , inlineResponseDefault1AdvisoryOpinionsHighlights :: [Text] -- ^
  , inlineResponseDefault1AdvisoryOpinionsIs_pending :: Bool -- ^
  , inlineResponseDefault1AdvisoryOpinionsIssue_date :: Date -- ^
  , inlineResponseDefault1AdvisoryOpinionsName :: Text -- ^
  , inlineResponseDefault1AdvisoryOpinionsNo :: Text -- ^
  , inlineResponseDefault1AdvisoryOpinionsRegulatory_citations :: [Inline_response_default_1_regulatory_citations] -- ^
  , inlineResponseDefault1AdvisoryOpinionsRepresentative_names :: [Text] -- ^
  , inlineResponseDefault1AdvisoryOpinionsRequest_date :: Date -- ^
  , inlineResponseDefault1AdvisoryOpinionsRequestor_names :: [Text] -- ^
  , inlineResponseDefault1AdvisoryOpinionsRequestor_types :: [Text] -- ^
  , inlineResponseDefault1AdvisoryOpinionsStatus :: Text -- ^
  , inlineResponseDefault1AdvisoryOpinionsStatutory_citations :: [Inline_response_default_1_statutory_citations] -- ^
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
  { inlineResponseDefault1CommissionVotesAction    :: Text -- ^
  , inlineResponseDefault1CommissionVotesVote_date :: Date -- ^
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
  { inlineResponseDefault1DocumentsCategory      :: Text -- ^
  , inlineResponseDefault1DocumentsDescription   :: Text -- ^
  , inlineResponseDefault1DocumentsDocument_date :: Date -- ^
  , inlineResponseDefault1DocumentsDocument_id   :: Int -- ^
  , inlineResponseDefault1DocumentsLength        :: Int -- ^
  , inlineResponseDefault1DocumentsUrl           :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_1_documents where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault1Documents")
instance ToJSON Inline_response_default_1_documents where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault1Documents")

-- |
data Inline_response_default_1_documents_1 = Inline_response_default_1_documents_1
  { inlineResponseDefault1Documents1Category    :: Text -- ^
  , inlineResponseDefault1Documents1Date        :: Date -- ^
  , inlineResponseDefault1Documents1Description :: Text -- ^
  , inlineResponseDefault1Documents1Document_id :: Int -- ^
  , inlineResponseDefault1Documents1Url         :: Text -- ^
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
  { inlineResponseDefault1MursClose_date :: Date -- ^
  , inlineResponseDefault1MursCommission_votes :: [Inline_response_default_1_commission_votes] -- ^
  , inlineResponseDefault1MursDispositions :: [Inline_response_default_1_dispositions] -- ^
  , inlineResponseDefault1MursDoc_id :: Text -- ^
  , inlineResponseDefault1MursDocument_highlights :: Value -- ^
  , inlineResponseDefault1MursDocuments :: [Inline_response_default_1_documents] -- ^
  , inlineResponseDefault1MursElection_cycles :: Int -- ^
  , inlineResponseDefault1MursHighlights :: [Text] -- ^
  , inlineResponseDefault1MursMur_type :: Text -- ^
  , inlineResponseDefault1MursName :: Text -- ^
  , inlineResponseDefault1MursNo :: Text -- ^
  , inlineResponseDefault1MursOpen_date :: Date -- ^
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
  { inlineResponseDefault1RegulationsDoc_id              :: Text -- ^
  , inlineResponseDefault1RegulationsDocument_highlights :: Value -- ^
  , inlineResponseDefault1RegulationsHighlights          :: [Text] -- ^
  , inlineResponseDefault1RegulationsName                :: Text -- ^
  , inlineResponseDefault1RegulationsNo                  :: Text -- ^
  , inlineResponseDefault1RegulationsUrl                 :: Text -- ^
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
  { inlineResponseDefault1StatutesChapter             :: Text -- ^
  , inlineResponseDefault1StatutesDoc_id              :: Text -- ^
  , inlineResponseDefault1StatutesDocument_highlights :: Value -- ^
  , inlineResponseDefault1StatutesHighlights          :: [Text] -- ^
  , inlineResponseDefault1StatutesName                :: Text -- ^
  , inlineResponseDefault1StatutesNo                  :: Text -- ^
  , inlineResponseDefault1StatutesTitle               :: Text -- ^
  , inlineResponseDefault1StatutesUrl                 :: Text -- ^
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
  { inlineResponseDefault3ResultsAction_code                 :: Text -- ^
  , inlineResponseDefault3ResultsAction_code_full            :: Text -- ^
  , inlineResponseDefault3ResultsCandidate_first_name        :: Text -- ^
  , inlineResponseDefault3ResultsCandidate_id                :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , inlineResponseDefault3ResultsCandidate_last_name         :: Text -- ^
  , inlineResponseDefault3ResultsCandidate_middle_name       :: Text -- ^
  , inlineResponseDefault3ResultsCandidate_name              :: Text -- ^ Name of candidate running for office
  , inlineResponseDefault3ResultsCandidate_office            :: Text -- ^
  , inlineResponseDefault3ResultsCandidate_office_district   :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , inlineResponseDefault3ResultsCandidate_office_full       :: Text -- ^
  , inlineResponseDefault3ResultsCandidate_office_state      :: Text -- ^
  , inlineResponseDefault3ResultsCandidate_office_state_full :: Text -- ^
  , inlineResponseDefault3ResultsCandidate_prefix            :: Text -- ^
  , inlineResponseDefault3ResultsCandidate_suffix            :: Text -- ^
  , inlineResponseDefault3ResultsCommittee                   :: CommitteeHistory -- ^
  , inlineResponseDefault3ResultsCommittee_id                :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , inlineResponseDefault3ResultsCycle                       :: Int -- ^
  , inlineResponseDefault3ResultsDue_date_terms              :: Text -- ^
  , inlineResponseDefault3ResultsElection_type               :: Text -- ^
  , inlineResponseDefault3ResultsElection_type_full          :: Text -- ^
  , inlineResponseDefault3ResultsEntity_type                 :: Text -- ^
  , inlineResponseDefault3ResultsEntity_type_full            :: Text -- ^
  , inlineResponseDefault3ResultsFec_committee_id            :: Text -- ^
  , inlineResponseDefault3ResultsFec_election_type_full      :: Text -- ^
  , inlineResponseDefault3ResultsFec_election_type_year      :: Text -- ^
  , inlineResponseDefault3ResultsFile_number                 :: Int -- ^
  , inlineResponseDefault3ResultsFiling_form                 :: Text -- ^
  , inlineResponseDefault3ResultsImage_number                :: Text -- ^ An unique identifier for each page the electronic or paper report.
  , inlineResponseDefault3ResultsIncurred_date               :: Integer -- ^
  , inlineResponseDefault3ResultsInterest_rate_terms         :: Text -- ^
  , inlineResponseDefault3ResultsLine_number                 :: Text -- ^
  , inlineResponseDefault3ResultsLink_id                     :: Int -- ^
  , inlineResponseDefault3ResultsLoad_date                   :: Integer -- ^
  , inlineResponseDefault3ResultsLoan_balance                :: Float -- ^
  , inlineResponseDefault3ResultsLoan_source_city            :: Text -- ^
  , inlineResponseDefault3ResultsLoan_source_first_name      :: Text -- ^
  , inlineResponseDefault3ResultsLoan_source_last_name       :: Text -- ^
  , inlineResponseDefault3ResultsLoan_source_middle_name     :: Text -- ^
  , inlineResponseDefault3ResultsLoan_source_name            :: Text -- ^ Source of the loan (i.e., bank loan, brokerage account, credit card, home equity line of credit,other line of credit, or personal funds of the candidate
  , inlineResponseDefault3ResultsLoan_source_prefix          :: Text -- ^
  , inlineResponseDefault3ResultsLoan_source_state           :: Text -- ^
  , inlineResponseDefault3ResultsLoan_source_street_1        :: Text -- ^
  , inlineResponseDefault3ResultsLoan_source_street_2        :: Text -- ^
  , inlineResponseDefault3ResultsLoan_source_suffix          :: Text -- ^
  , inlineResponseDefault3ResultsLoan_source_zip             :: Int -- ^
  , inlineResponseDefault3ResultsMemo_code                   :: Text -- ^
  , inlineResponseDefault3ResultsMemo_text                   :: Text -- ^
  , inlineResponseDefault3ResultsOriginal_loan_amount        :: Float -- ^
  , inlineResponseDefault3ResultsOriginal_sub_id             :: Int -- ^
  , inlineResponseDefault3ResultsPayment_to_date             :: Float -- ^
  , inlineResponseDefault3ResultsPdf_url                     :: Text -- ^
  , inlineResponseDefault3ResultsPersonally_funded           :: Text -- ^
  , inlineResponseDefault3ResultsReport_type                 :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , inlineResponseDefault3ResultsReport_year                 :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , inlineResponseDefault3ResultsSchedule_a_line_number      :: Int -- ^
  , inlineResponseDefault3ResultsSchedule_type               :: Text -- ^
  , inlineResponseDefault3ResultsSchedule_type_full          :: Text -- ^
  , inlineResponseDefault3ResultsSecured_ind                 :: Text -- ^
  , inlineResponseDefault3ResultsSub_id                      :: Text -- ^
  , inlineResponseDefault3ResultsTransaction_id              :: Text -- ^
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
  { inlineResponseDefault4ResultsAction_code :: Text -- ^
  , inlineResponseDefault4ResultsAction_code_full :: Text -- ^
  , inlineResponseDefault4ResultsAmount_incurred_period :: Float -- ^
  , inlineResponseDefault4ResultsCandidate_first_name :: Text -- ^
  , inlineResponseDefault4ResultsCandidate_id :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , inlineResponseDefault4ResultsCandidate_last_name :: Text -- ^
  , inlineResponseDefault4ResultsCandidate_office :: Text -- ^
  , inlineResponseDefault4ResultsCandidate_office_district :: Text -- ^
  , inlineResponseDefault4ResultsCandidate_office_state :: Text -- ^
  , inlineResponseDefault4ResultsCandidate_office_state_full :: Text -- ^
  , inlineResponseDefault4ResultsCanidate_name :: Text -- ^ Name of candidate running for office
  , inlineResponseDefault4ResultsCommittee :: CommitteeHistory -- ^
  , inlineResponseDefault4ResultsCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , inlineResponseDefault4ResultsCommittee_name :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , inlineResponseDefault4ResultsConduit_committee_city :: Text -- ^
  , inlineResponseDefault4ResultsConduit_committee_id :: Text -- ^
  , inlineResponseDefault4ResultsConduit_committee_name :: Text -- ^
  , inlineResponseDefault4ResultsConduit_committee_state :: Text -- ^
  , inlineResponseDefault4ResultsConduit_committee_street1 :: Text -- ^
  , inlineResponseDefault4ResultsConduit_committee_street2 :: Text -- ^
  , inlineResponseDefault4ResultsConduit_committee_zip :: Int -- ^
  , inlineResponseDefault4ResultsCreditor_debtor_city :: Text -- ^
  , inlineResponseDefault4ResultsCreditor_debtor_first_name :: Text -- ^
  , inlineResponseDefault4ResultsCreditor_debtor_id :: Text -- ^
  , inlineResponseDefault4ResultsCreditor_debtor_last_name :: Text -- ^
  , inlineResponseDefault4ResultsCreditor_debtor_middle_name :: Text -- ^
  , inlineResponseDefault4ResultsCreditor_debtor_name :: Text -- ^
  , inlineResponseDefault4ResultsCreditor_debtor_prefix :: Text -- ^
  , inlineResponseDefault4ResultsCreditor_debtor_state :: Text -- ^
  , inlineResponseDefault4ResultsCreditor_debtor_street1 :: Text -- ^
  , inlineResponseDefault4ResultsCreditor_debtor_street2 :: Text -- ^
  , inlineResponseDefault4ResultsCreditor_debtor_suffix :: Text -- ^
  , inlineResponseDefault4ResultsElection_cycle :: Int -- ^
  , inlineResponseDefault4ResultsEntity_type :: Text -- ^
  , inlineResponseDefault4ResultsFile_number :: Int -- ^
  , inlineResponseDefault4ResultsFiling_form :: Text -- ^
  , inlineResponseDefault4ResultsImage_number :: Text -- ^ An unique identifier for each page the electronic or paper report.
  , inlineResponseDefault4ResultsLine_number :: Text -- ^
  , inlineResponseDefault4ResultsLink_id :: Int -- ^
  , inlineResponseDefault4ResultsLoad_date :: Integer -- ^
  , inlineResponseDefault4ResultsNature_of_debt :: Text -- ^
  , inlineResponseDefault4ResultsOriginal_sub_id :: Int -- ^
  , inlineResponseDefault4ResultsOutstanding_balance_beginning_of_period :: Float -- ^
  , inlineResponseDefault4ResultsOutstanding_balance_close_of_period :: Float -- ^
  , inlineResponseDefault4ResultsPayment_period :: Float -- ^
  , inlineResponseDefault4ResultsPdf_url :: Text -- ^
  , inlineResponseDefault4ResultsReport_type :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , inlineResponseDefault4ResultsReport_year :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , inlineResponseDefault4ResultsSchedule_type :: Text -- ^
  , inlineResponseDefault4ResultsSchedule_type_full :: Text -- ^
  , inlineResponseDefault4ResultsSub_id :: Text -- ^
  , inlineResponseDefault4ResultsTransaction_id :: Text -- ^
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
  { inlineResponseDefault5ResultsAction_code :: Text -- ^
  , inlineResponseDefault5ResultsAction_code_full :: Text -- ^
  , inlineResponseDefault5ResultsAggregate_general_election_expenditure :: Text -- ^
  , inlineResponseDefault5ResultsBack_reference_schedule_name :: Text -- ^
  , inlineResponseDefault5ResultsBack_reference_transaction_id :: Int -- ^
  , inlineResponseDefault5ResultsCandidate_first_name :: Text -- ^
  , inlineResponseDefault5ResultsCandidate_id :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , inlineResponseDefault5ResultsCandidate_last_name :: Text -- ^
  , inlineResponseDefault5ResultsCandidate_middle_name :: Text -- ^
  , inlineResponseDefault5ResultsCandidate_name :: Text -- ^ Name of candidate running for office
  , inlineResponseDefault5ResultsCandidate_office :: Text -- ^
  , inlineResponseDefault5ResultsCandidate_office_district :: Text -- ^
  , inlineResponseDefault5ResultsCandidate_office_full :: Text -- ^
  , inlineResponseDefault5ResultsCandidate_office_state :: Text -- ^
  , inlineResponseDefault5ResultsCandidate_office_state_full :: Text -- ^
  , inlineResponseDefault5ResultsCandidate_prefix :: Text -- ^
  , inlineResponseDefault5ResultsCandidate_suffix :: Text -- ^
  , inlineResponseDefault5ResultsCatolog_code :: Text -- ^
  , inlineResponseDefault5ResultsCatolog_code_full :: Text -- ^
  , inlineResponseDefault5ResultsCommittee :: CommitteeHistory -- ^
  , inlineResponseDefault5ResultsCommittee_designated_coordinated_expenditure_indicator :: Text -- ^
  , inlineResponseDefault5ResultsCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , inlineResponseDefault5ResultsCommittee_name :: Text -- ^
  , inlineResponseDefault5ResultsConduit_committee_city :: Text -- ^
  , inlineResponseDefault5ResultsConduit_committee_id :: Text -- ^
  , inlineResponseDefault5ResultsConduit_committee_name :: Text -- ^
  , inlineResponseDefault5ResultsConduit_committee_state :: Text -- ^
  , inlineResponseDefault5ResultsConduit_committee_street1 :: Text -- ^
  , inlineResponseDefault5ResultsConduit_committee_street2 :: Text -- ^
  , inlineResponseDefault5ResultsConduit_committee_zip :: Int -- ^
  , inlineResponseDefault5ResultsDesignated_committee_id :: Text -- ^
  , inlineResponseDefault5ResultsDesignated_committee_name :: Text -- ^
  , inlineResponseDefault5ResultsElection_cycle :: Int -- ^
  , inlineResponseDefault5ResultsEntity_type :: Text -- ^
  , inlineResponseDefault5ResultsEntity_type_desc :: Text -- ^
  , inlineResponseDefault5ResultsExpenditure_amount :: Int -- ^
  , inlineResponseDefault5ResultsExpenditure_date :: Integer -- ^
  , inlineResponseDefault5ResultsExpenditure_purpose_full :: Text -- ^
  , inlineResponseDefault5ResultsExpenditure_type :: Text -- ^
  , inlineResponseDefault5ResultsExpenditure_type_full :: Text -- ^
  , inlineResponseDefault5ResultsFile_number :: Int -- ^
  , inlineResponseDefault5ResultsFiling_form :: Text -- ^
  , inlineResponseDefault5ResultsImage_number :: Text -- ^ An unique identifier for each page the electronic or paper report.
  , inlineResponseDefault5ResultsLine_number :: Text -- ^
  , inlineResponseDefault5ResultsLink_id :: Int -- ^
  , inlineResponseDefault5ResultsLoad_date :: Integer -- ^
  , inlineResponseDefault5ResultsMemo_code :: Text -- ^
  , inlineResponseDefault5ResultsMemo_code_full :: Text -- ^
  , inlineResponseDefault5ResultsMemo_text :: Text -- ^
  , inlineResponseDefault5ResultsOriginal_sub_id :: Int -- ^
  , inlineResponseDefault5ResultsPayee_first_name :: Text -- ^
  , inlineResponseDefault5ResultsPayee_last_name :: Text -- ^
  , inlineResponseDefault5ResultsPayee_middle_name :: Text -- ^
  , inlineResponseDefault5ResultsPayee_name :: Text -- ^
  , inlineResponseDefault5ResultsPdf_url :: Text -- ^
  , inlineResponseDefault5ResultsReport_type :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , inlineResponseDefault5ResultsReport_year :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , inlineResponseDefault5ResultsSchedule_type :: Text -- ^
  , inlineResponseDefault5ResultsSchedule_type_full :: Text -- ^
  , inlineResponseDefault5ResultsSub_id :: Text -- ^
  , inlineResponseDefault5ResultsSubordinate_committee :: CommitteeHistory -- ^
  , inlineResponseDefault5ResultsSubordinate_committee_id :: Text -- ^
  , inlineResponseDefault5ResultsTransaction_id :: Text -- ^
  , inlineResponseDefault5ResultsUnlimited_spending_flag :: Text -- ^
  , inlineResponseDefault5ResultsUnlimited_spending_flag_full :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON Inline_response_default_5_results where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "inlineResponseDefault5Results")
instance ToJSON Inline_response_default_5_results where
  toJSON = genericToJSON (removeFieldLabelPrefix False "inlineResponseDefault5Results")

-- |
data OffsetInfo = OffsetInfo
  { offsetInfoCount    :: Int -- ^
  , offsetInfoPage     :: Int -- ^
  , offsetInfoPages    :: Int -- ^
  , offsetInfoPer_page :: Int -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON OffsetInfo where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "offsetInfo")
instance ToJSON OffsetInfo where
  toJSON = genericToJSON (removeFieldLabelPrefix False "offsetInfo")

-- |
data OperationsLog = OperationsLog
  { operationsLogAmendment_indicator            :: Text -- ^ Type of the report.N(new), A(amended) or T(cancel)
  , operationsLogBeginning_image_number         :: Text -- ^  Unique identifier for the electronic or paper report. This number is used to construct PDF URLs to the original document.
  , operationsLogCandidate_committee_id         :: Text -- ^  A unique identifier of the registered filer.
  , operationsLogCoverage_end_date              :: Integer -- ^ Ending date of the reporting period
  , operationsLogCoverage_start_date            :: Integer -- ^ Beginning date of the reporting period
  , operationsLogEnding_image_number            :: Text -- ^ Image number is an unique identifier for each page the electronic or paper report. The last image number corresponds to the image number for the last page of the document.
  , operationsLogForm_type                      :: Text -- ^  Indicates the type of form that was filed. ex: F1, F2, F3P, F3X etc...
  , operationsLogReceipt_date                   :: Integer -- ^ Date the FEC received the electronic or paper record
  , operationsLogReport_type                    :: Text -- ^ Monthly, quarterly or other period covered reports
  , operationsLogReport_year                    :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , operationsLogStatus_num                     :: Int -- ^  Status of the transactional report.     -0- Transaction is entered            into the system.           But not verified.     -1- Transaction is verified.
  , operationsLogSub_id                         :: Int -- ^  A unique identifier of the transactional report.
  , operationsLogSummary_data_complete_date     :: Integer -- ^ Date when the report is entered into the database
  , operationsLogSummary_data_verification_date :: Integer -- ^ Same day or a day after the report is loaded in the database
  , operationsLogTransaction_data_complete_date :: Date -- ^ Date when the report is processed completely
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
  { radAnalystAnalyst_email          :: Text -- ^ Email of RAD analyst
  , radAnalystAnalyst_id             :: Double -- ^ ID of RAD analyst.
  , radAnalystAnalyst_short_id       :: Double -- ^ Short ID of RAD analyst.
  , radAnalystAnalyst_title          :: Text -- ^ Title of RAD analyst
  , radAnalystAssignment_update_date :: Date -- ^ Date of most recent RAD analyst assignment change
  , radAnalystCommittee_id           :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , radAnalystCommittee_name         :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , radAnalystFirst_name             :: Text -- ^ Fist name of RAD analyst
  , radAnalystLast_name              :: Text -- ^ Last name of RAD analyst
  , radAnalystRad_branch             :: Text -- ^ Branch of RAD analyst
  , radAnalystTelephone_ext          :: Double -- ^ Telephone extension of RAD analyst
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
  { reportDateCreate_date      :: Date -- ^ Date the record was created
  , reportDateDue_date         :: Date -- ^ Date the report is due
  , reportDateReport_type      :: Text -- ^
  , reportDateReport_type_full :: Text -- ^
  , reportDateReport_year      :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , reportDateUpdate_date      :: Date -- ^ Date the record was updated
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
  { reportTypeReport_type      :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , reportTypeReport_type_full :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  } deriving (Show, Eq, Generic)

instance FromJSON ReportType where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "reportType")
instance ToJSON ReportType where
  toJSON = genericToJSON (removeFieldLabelPrefix False "reportType")

-- |
data ScheduleA = ScheduleA
  { scheduleAAmendment_indicator                   :: Text -- ^
  , scheduleAAmendment_indicator_desc              :: Text -- ^
  , scheduleABack_reference_schedule_name          :: Text -- ^
  , scheduleABack_reference_transaction_id         :: Text -- ^
  , scheduleACandidate_first_name                  :: Text -- ^
  , scheduleACandidate_id                          :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , scheduleACandidate_last_name                   :: Text -- ^
  , scheduleACandidate_middle_name                 :: Text -- ^
  , scheduleACandidate_name                        :: Text -- ^ Name of candidate running for office
  , scheduleACandidate_office                      :: Text -- ^
  , scheduleACandidate_office_district             :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , scheduleACandidate_office_full                 :: Text -- ^
  , scheduleACandidate_office_state                :: Text -- ^
  , scheduleACandidate_office_state_full           :: Text -- ^
  , scheduleACandidate_prefix                      :: Text -- ^
  , scheduleACandidate_suffix                      :: Text -- ^
  , scheduleACommittee                             :: CommitteeHistory -- ^
  , scheduleACommittee_id                          :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleACommittee_name                        :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , scheduleAConduit_committee_city                :: Text -- ^
  , scheduleAConduit_committee_id                  :: Text -- ^
  , scheduleAConduit_committee_name                :: Text -- ^
  , scheduleAConduit_committee_state               :: Text -- ^
  , scheduleAConduit_committee_street1             :: Text -- ^
  , scheduleAConduit_committee_street2             :: Text -- ^
  , scheduleAConduit_committee_zip                 :: Int -- ^
  , scheduleAContribution_receipt_amount           :: Double -- ^
  , scheduleAContribution_receipt_date             :: Date -- ^
  , scheduleAContributor                           :: CommitteeHistory -- ^
  , scheduleAContributor_aggregate_ytd             :: Double -- ^
  , scheduleAContributor_city                      :: Text -- ^ City of contributor
  , scheduleAContributor_employer                  :: Text -- ^ Employer of contributor, filers need to make an effort to gather this information
  , scheduleAContributor_first_name                :: Text -- ^
  , scheduleAContributor_id                        :: Text -- ^ The FEC identifier should be represented here if the contributor is registered with the FEC.
  , scheduleAContributor_last_name                 :: Text -- ^
  , scheduleAContributor_middle_name               :: Text -- ^
  , scheduleAContributor_name                      :: Text -- ^ Name of contributor
  , scheduleAContributor_occupation                :: Text -- ^ Occupation of contributor, filers need to make an effort to gather this information
  , scheduleAContributor_prefix                    :: Text -- ^
  , scheduleAContributor_state                     :: Text -- ^ State of contributor
  , scheduleAContributor_street_1                  :: Text -- ^
  , scheduleAContributor_street_2                  :: Text -- ^
  , scheduleAContributor_suffix                    :: Text -- ^
  , scheduleAContributor_zip                       :: Text -- ^ Zip code of contributor
  , scheduleADonor_committee_name                  :: Text -- ^
  , scheduleAElection_type                         :: Text -- ^
  , scheduleAElection_type_full                    :: Text -- ^
  , scheduleAEntity_type                           :: Text -- ^
  , scheduleAEntity_type_desc                      :: Text -- ^
  , scheduleAFec_election_type_desc                :: Text -- ^
  , scheduleAFec_election_year                     :: Text -- ^
  , scheduleAFile_number                           :: Int -- ^
  , scheduleAFiling_form                           :: Text -- ^
  , scheduleAImage_number                          :: Text -- ^
  , scheduleAIncreased_limit                       :: Text -- ^
  , scheduleAIs_individual                         :: Bool -- ^
  , scheduleALine_number                           :: Text -- ^
  , scheduleALine_number_label                     :: Text -- ^
  , scheduleALink_id                               :: Int -- ^
  , scheduleALoad_date                             :: Integer -- ^
  , scheduleAMemo_code                             :: Text -- ^
  , scheduleAMemo_code_full                        :: Text -- ^
  , scheduleAMemo_text                             :: Text -- ^
  , scheduleAMemoed_subtotal                       :: Bool -- ^
  , scheduleANational_committee_nonfederal_account :: Text -- ^
  , scheduleAOriginal_sub_id                       :: Text -- ^
  , scheduleAPdf_url                               :: Text -- ^
  , scheduleAReceipt_type                          :: Text -- ^
  , scheduleAReceipt_type_desc                     :: Text -- ^
  , scheduleAReceipt_type_full                     :: Text -- ^
  , scheduleAReport_type                           :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , scheduleAReport_year                           :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , scheduleASchedule_type                         :: Text -- ^
  , scheduleASchedule_type_full                    :: Text -- ^
  , scheduleASub_id                                :: Text -- ^
  , scheduleATransaction_id                        :: Text -- ^
  , scheduleATwo_year_transaction_period           :: Int -- ^  This is a two-year period that is derived from the year a transaction took place in the Itemized Schedule A and Schedule B tables. In cases where we have the date of the transaction (contribution_receipt_date in schedules/schedule_a, disbursement_date in schedules/schedule_b) the two_year_transaction_period is named after the ending, even-numbered year. If we do not have the date  of the transaction, we fall back to using the report year (report_year in both tables) instead,  making the same cycle adjustment as necessary. If no transaction year is specified, the results default to the most current cycle.
  , scheduleAUnused_contbr_id                      :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleA where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleA")
instance ToJSON ScheduleA where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleA")

-- |
data ScheduleAByEmployer = ScheduleAByEmployer
  { scheduleAByEmployerCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleAByEmployerCount        :: Int -- ^ Number of records making up the total
  , scheduleAByEmployerCycle        :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleAByEmployerEmployer     :: Text -- ^ Employer of contributor as reported on the committee's filing
  , scheduleAByEmployerTotal        :: Double -- ^
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
  { scheduleAByOccupationCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleAByOccupationCount        :: Int -- ^ Number of records making up the total
  , scheduleAByOccupationCycle        :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleAByOccupationOccupation   :: Text -- ^ Occupation of contributor as reported on the committee's filing
  , scheduleAByOccupationTotal        :: Double -- ^
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
  { scheduleABySizeCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleABySizeCount        :: Int -- ^ Number of records making up the total
  , scheduleABySizeCycle        :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleABySizeSize         :: Int -- ^
  , scheduleABySizeTotal        :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleABySize where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleABySize")
instance ToJSON ScheduleABySize where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleABySize")

-- |
data ScheduleABySizeCandidate = ScheduleABySizeCandidate
  { scheduleABySizeCandidateCandidate_id :: Text -- ^
  , scheduleABySizeCandidateCycle        :: Int -- ^
  , scheduleABySizeCandidateSize         :: Int -- ^
  , scheduleABySizeCandidateTotal        :: Double -- ^
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
  { scheduleAByStateCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleAByStateCount        :: Int -- ^ Number of records making up the total
  , scheduleAByStateCycle        :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleAByStateState        :: Text -- ^ US state or territory
  , scheduleAByStateState_full   :: Text -- ^ US state or territory
  , scheduleAByStateTotal        :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleAByState where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleAByState")
instance ToJSON ScheduleAByState where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleAByState")

-- |
data ScheduleAByStateCandidate = ScheduleAByStateCandidate
  { scheduleAByStateCandidateCandidate_id :: Text -- ^
  , scheduleAByStateCandidateCycle        :: Int -- ^
  , scheduleAByStateCandidateState        :: Text -- ^
  , scheduleAByStateCandidateState_full   :: Text -- ^
  , scheduleAByStateCandidateTotal        :: Double -- ^
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
  { scheduleAByStateRecipientTotalsCommittee_type      :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , scheduleAByStateRecipientTotalsCommittee_type_full :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , scheduleAByStateRecipientTotalsCount               :: Int -- ^ Number of records making up the total.
  , scheduleAByStateRecipientTotalsCycle               :: Int -- ^  Filter records to only those that are applicable to a given two-year period. This cycle follows the traditional House election cycle and subdivides the presidential and Senate elections into comparable two-year blocks. The cycle begins with an odd year and is named for its ending, even year.
  , scheduleAByStateRecipientTotalsState               :: Text -- ^ US state or territory
  , scheduleAByStateRecipientTotalsState_full          :: Text -- ^ US state or territory
  , scheduleAByStateRecipientTotalsTotal               :: Double -- ^ The calculated total.
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
  { scheduleAByZipCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleAByZipCount        :: Int -- ^ Number of records making up the total
  , scheduleAByZipCycle        :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleAByZipState        :: Text -- ^ US state or territory
  , scheduleAByZipState_full   :: Text -- ^ US state or territory
  , scheduleAByZipTotal        :: Double -- ^
  , scheduleAByZipZip          :: Text -- ^
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
  { scheduleAEfileAmendment_indicator           :: Text -- ^
  , scheduleAEfileBack_reference_schedule_name  :: Text -- ^
  , scheduleAEfileBack_reference_transaction_id :: Text -- ^
  , scheduleAEfileBeginning_image_number        :: Text -- ^
  , scheduleAEfileCommittee                     :: CommitteeHistory -- ^
  , scheduleAEfileCommittee_id                  :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleAEfileConduit_committee_city        :: Text -- ^
  , scheduleAEfileConduit_committee_id          :: Text -- ^
  , scheduleAEfileConduit_committee_name        :: Text -- ^
  , scheduleAEfileConduit_committee_state       :: Text -- ^
  , scheduleAEfileConduit_committee_street1     :: Text -- ^
  , scheduleAEfileConduit_committee_street2     :: Text -- ^
  , scheduleAEfileConduit_committee_zip         :: Int -- ^
  , scheduleAEfileContribution_receipt_amount   :: Double -- ^
  , scheduleAEfileContribution_receipt_date     :: Date -- ^
  , scheduleAEfileContributor_aggregate_ytd     :: Double -- ^
  , scheduleAEfileContributor_city              :: Text -- ^ City of contributor
  , scheduleAEfileContributor_employer          :: Text -- ^ Employer of contributor, filers need to make an effort to gather this information
  , scheduleAEfileContributor_first_name        :: Text -- ^
  , scheduleAEfileContributor_last_name         :: Text -- ^
  , scheduleAEfileContributor_middle_name       :: Text -- ^
  , scheduleAEfileContributor_name              :: Text -- ^
  , scheduleAEfileContributor_occupation        :: Text -- ^ Occupation of contributor, filers need to make an effort to gather this information
  , scheduleAEfileContributor_prefix            :: Text -- ^
  , scheduleAEfileContributor_state             :: Text -- ^ State of contributor
  , scheduleAEfileContributor_suffix            :: Text -- ^
  , scheduleAEfileContributor_zip               :: Text -- ^ Zip code of contributor
  , scheduleAEfileCsv_url                       :: Text -- ^
  , scheduleAEfileCycle                         :: Int -- ^
  , scheduleAEfileEntity_type                   :: Text -- ^
  , scheduleAEfileFec_election_type_desc        :: Text -- ^
  , scheduleAEfileFec_url                       :: Text -- ^
  , scheduleAEfileFile_number                   :: Int -- ^
  , scheduleAEfileFiling                        :: EFilings -- ^
  , scheduleAEfileImage_number                  :: Text -- ^ An unique identifier for each page the electronic or paper report.
  , scheduleAEfileLine_number                   :: Text -- ^
  , scheduleAEfileLoad_timestamp                :: Integer -- ^
  , scheduleAEfileMemo_code                     :: Text -- ^
  , scheduleAEfileMemo_text                     :: Text -- ^
  , scheduleAEfilePdf_url                       :: Text -- ^
  , scheduleAEfilePgo                           :: Text -- ^
  , scheduleAEfileRelated_line_number           :: Int -- ^
  , scheduleAEfileReport_type                   :: Text -- ^
  , scheduleAEfileTransaction_id                :: Text -- ^
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
  { scheduleBAmendment_indicator                   :: Text -- ^
  , scheduleBAmendment_indicator_desc              :: Text -- ^
  , scheduleBBack_reference_schedule_id            :: Text -- ^
  , scheduleBBack_reference_transaction_id         :: Text -- ^
  , scheduleBBeneficiary_committee_name            :: Text -- ^
  , scheduleBCandidate_first_name                  :: Text -- ^
  , scheduleBCandidate_id                          :: Text -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , scheduleBCandidate_last_name                   :: Text -- ^
  , scheduleBCandidate_middle_name                 :: Text -- ^
  , scheduleBCandidate_name                        :: Text -- ^ Name of candidate running for office
  , scheduleBCandidate_office                      :: Text -- ^
  , scheduleBCandidate_office_description          :: Text -- ^
  , scheduleBCandidate_office_district             :: Text -- ^
  , scheduleBCandidate_office_state                :: Text -- ^
  , scheduleBCandidate_office_state_full           :: Text -- ^
  , scheduleBCandidate_prefix                      :: Text -- ^
  , scheduleBCandidate_suffix                      :: Text -- ^
  , scheduleBCategory_code                         :: Text -- ^
  , scheduleBCategory_code_full                    :: Text -- ^
  , scheduleBComm_dt                               :: Date -- ^
  , scheduleBCommittee                             :: CommitteeHistory -- ^
  , scheduleBCommittee_id                          :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleBConduit_committee_city                :: Text -- ^
  , scheduleBConduit_committee_name                :: Text -- ^
  , scheduleBConduit_committee_state               :: Text -- ^
  , scheduleBConduit_committee_street1             :: Text -- ^
  , scheduleBConduit_committee_street2             :: Text -- ^
  , scheduleBConduit_committee_zip                 :: Int -- ^
  , scheduleBDisbursement_amount                   :: Double -- ^
  , scheduleBDisbursement_date                     :: Date -- ^
  , scheduleBDisbursement_description              :: Text -- ^
  , scheduleBDisbursement_purpose_category         :: Text -- ^
  , scheduleBDisbursement_type                     :: Text -- ^
  , scheduleBDisbursement_type_description         :: Text -- ^
  , scheduleBElection_type                         :: Text -- ^
  , scheduleBElection_type_full                    :: Text -- ^
  , scheduleBEntity_type                           :: Text -- ^
  , scheduleBEntity_type_desc                      :: Text -- ^
  , scheduleBFec_election_type_desc                :: Text -- ^
  , scheduleBFec_election_year                     :: Text -- ^
  , scheduleBFile_number                           :: Int -- ^
  , scheduleBFiling_form                           :: Text -- ^
  , scheduleBImage_number                          :: Text -- ^
  , scheduleBLine_number                           :: Text -- ^
  , scheduleBLine_number_label                     :: Text -- ^
  , scheduleBLink_id                               :: Int -- ^
  , scheduleBLoad_date                             :: Integer -- ^
  , scheduleBMemo_code                             :: Text -- ^
  , scheduleBMemo_code_full                        :: Text -- ^
  , scheduleBMemo_text                             :: Text -- ^
  , scheduleBMemoed_subtotal                       :: Bool -- ^
  , scheduleBNational_committee_nonfederal_account :: Text -- ^
  , scheduleBOriginal_sub_id                       :: Text -- ^
  , scheduleBPayee_employer                        :: Text -- ^
  , scheduleBPayee_first_name                      :: Text -- ^
  , scheduleBPayee_last_name                       :: Text -- ^
  , scheduleBPayee_middle_name                     :: Text -- ^
  , scheduleBPayee_occupation                      :: Text -- ^
  , scheduleBPayee_prefix                          :: Text -- ^
  , scheduleBPayee_suffix                          :: Text -- ^
  , scheduleBPdf_url                               :: Text -- ^
  , scheduleBRecipient_city                        :: Text -- ^
  , scheduleBRecipient_committee                   :: CommitteeHistory -- ^
  , scheduleBRecipient_committee_id                :: Text -- ^
  , scheduleBRecipient_name                        :: Text -- ^
  , scheduleBRecipient_state                       :: Text -- ^
  , scheduleBRecipient_zip                         :: Text -- ^
  , scheduleBRef_disp_excess_flg                   :: Text -- ^
  , scheduleBReport_type                           :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , scheduleBReport_year                           :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , scheduleBSchedule_type                         :: Text -- ^
  , scheduleBSchedule_type_full                    :: Text -- ^
  , scheduleBSemi_annual_bundled_refund            :: Double -- ^
  , scheduleBSpender_committee_type                :: Text -- ^
  , scheduleBSub_id                                :: Text -- ^
  , scheduleBTransaction_id                        :: Text -- ^
  , scheduleBTwo_year_transaction_period           :: Int -- ^  This is a two-year period that is derived from the year a transaction took place in the Itemized Schedule A and Schedule B tables. In cases where we have the date of the transaction (contribution_receipt_date in schedules/schedule_a, disbursement_date in schedules/schedule_b) the two_year_transaction_period is named after the ending, even-numbered year. If we do not have the date  of the transaction, we fall back to using the report year (report_year in both tables) instead,  making the same cycle adjustment as necessary. If no transaction year is specified, the results default to the most current cycle.
  , scheduleBUnused_recipient_committee_id         :: Text -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleB where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleB")
instance ToJSON ScheduleB where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleB")

-- |
data ScheduleBByPurpose = ScheduleBByPurpose
  { scheduleBByPurposeCommittee_id :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleBByPurposeCount        :: Int -- ^ Number of records making up the total
  , scheduleBByPurposeCycle        :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleBByPurposePurpose      :: Text -- ^ Purpose of the expenditure
  , scheduleBByPurposeTotal        :: Double -- ^
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
  { scheduleBByRecipientCommittee_id   :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleBByRecipientCount          :: Int -- ^ Number of records making up the total
  , scheduleBByRecipientCycle          :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleBByRecipientRecipient_name :: Text -- ^ Name of the entity receiving the disbursement
  , scheduleBByRecipientTotal          :: Double -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleBByRecipient where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleBByRecipient")
instance ToJSON ScheduleBByRecipient where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleBByRecipient")

-- |
data ScheduleBByRecipientID = ScheduleBByRecipientID
  { scheduleBByRecipientIDCommittee_id   :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleBByRecipientIDCommittee_name :: Text -- ^
  , scheduleBByRecipientIDCount          :: Int -- ^ Number of records making up the total
  , scheduleBByRecipientIDCycle          :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleBByRecipientIDIdx            :: Int -- ^
  , scheduleBByRecipientIDRecipient_id   :: Text -- ^ The FEC identifier should be represented here if the entity receiving the disbursement is registered with the FEC.
  , scheduleBByRecipientIDRecipient_name :: Text -- ^
  , scheduleBByRecipientIDTotal          :: Double -- ^
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
  { scheduleBEfileAmendment_indicator           :: Text -- ^
  , scheduleBEfileBack_reference_schedule_name  :: Text -- ^
  , scheduleBEfileBack_reference_transaction_id :: Text -- ^
  , scheduleBEfileBeginning_image_number        :: Text -- ^
  , scheduleBEfileBeneficiary_committee_name    :: Text -- ^
  , scheduleBEfileCandidate_office              :: Text -- ^
  , scheduleBEfileCandidate_office_district     :: Text -- ^
  , scheduleBEfileCommittee                     :: CommitteeHistory -- ^
  , scheduleBEfileCommittee_id                  :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleBEfileCsv_url                       :: Text -- ^
  , scheduleBEfileDisbursement_amount           :: Double -- ^
  , scheduleBEfileDisbursement_date             :: Date -- ^
  , scheduleBEfileDisbursement_description      :: Text -- ^
  , scheduleBEfileDisbursement_type             :: Text -- ^
  , scheduleBEfileEntity_type                   :: Text -- ^
  , scheduleBEfileFec_url                       :: Text -- ^
  , scheduleBEfileFile_number                   :: Int -- ^
  , scheduleBEfileFiling                        :: EFilings -- ^
  , scheduleBEfileImage_number                  :: Text -- ^ An unique identifier for each page the electronic or paper report.
  , scheduleBEfileIs_notice                     :: Bool -- ^
  , scheduleBEfileLine_number                   :: Text -- ^
  , scheduleBEfileLoad_timestamp                :: Integer -- ^
  , scheduleBEfileMemo_code                     :: Text -- ^
  , scheduleBEfileMemo_text                     :: Text -- ^
  , scheduleBEfilePayee_name                    :: Text -- ^
  , scheduleBEfilePdf_url                       :: Text -- ^
  , scheduleBEfileRecipient_city                :: Text -- ^
  , scheduleBEfileRecipient_name                :: Text -- ^
  , scheduleBEfileRecipient_prefix              :: Text -- ^
  , scheduleBEfileRecipient_state               :: Text -- ^
  , scheduleBEfileRecipient_suffix              :: Text -- ^
  , scheduleBEfileRecipient_zip                 :: Text -- ^
  , scheduleBEfileRelated_line_number           :: Int -- ^
  , scheduleBEfileReport_type                   :: Text -- ^
  , scheduleBEfileSemi_annual_bundled_refund    :: Int -- ^
  , scheduleBEfileTransaction_id                :: Text -- ^
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
  { scheduleEAction_code                       :: Text -- ^
  , scheduleEAction_code_full                  :: Text -- ^
  , scheduleEAmendment_indicator               :: Text -- ^      -N   new     -A   amendment     -T   terminated     -C   consolidated     -M   multi-candidate     -S   secondary      Null might be new or amendment. If amendment indicator is null and the filings is the first or first in a chain treat it as if it was a new. If it is not the first or first in a chain then treat the filing as an amendment.
  , scheduleEAmendment_number                  :: Int -- ^  Number of times the report has been amended.
  , scheduleEBack_reference_schedule_name      :: Text -- ^
  , scheduleEBack_reference_transaction_id     :: Text -- ^
  , scheduleECandidate                         :: Text -- ^
  , scheduleECandidate_first_name              :: Text -- ^
  , scheduleECandidate_id                      :: Text -- ^
  , scheduleECandidate_last_name               :: Text -- ^
  , scheduleECandidate_middle_name             :: Text -- ^
  , scheduleECandidate_name                    :: Text -- ^ Name of candidate running for office
  , scheduleECandidate_office                  :: Text -- ^ Federal office candidate runs for: H, S or P
  , scheduleECandidate_office_district         :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , scheduleECandidate_office_state            :: Text -- ^ US state or territory
  , scheduleECandidate_party                   :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , scheduleECandidate_prefix                  :: Text -- ^
  , scheduleECandidate_suffix                  :: Text -- ^
  , scheduleECategory_code                     :: Text -- ^
  , scheduleECategory_code_full                :: Text -- ^
  , scheduleECommittee                         :: CommitteeHistory -- ^
  , scheduleECommittee_id                      :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleEConduit_committee_city            :: Text -- ^
  , scheduleEConduit_committee_id              :: Text -- ^
  , scheduleEConduit_committee_name            :: Text -- ^
  , scheduleEConduit_committee_state           :: Text -- ^
  , scheduleEConduit_committee_street1         :: Text -- ^
  , scheduleEConduit_committee_street2         :: Text -- ^
  , scheduleEConduit_committee_zip             :: Int -- ^
  , scheduleEDissemination_date                :: Date -- ^
  , scheduleEElection_type                     :: Text -- ^ Election type  Convention, Primary, General, Special, Runoff etc.
  , scheduleEElection_type_full                :: Text -- ^ Election type  Convention, Primary, General, Special, Runoff etc.
  , scheduleEExpenditure_amount                :: Double -- ^
  , scheduleEExpenditure_date                  :: Date -- ^
  , scheduleEExpenditure_description           :: Text -- ^
  , scheduleEFile_number                       :: Int -- ^
  , scheduleEFiler_first_name                  :: Text -- ^
  , scheduleEFiler_last_name                   :: Text -- ^
  , scheduleEFiler_middle_name                 :: Text -- ^
  , scheduleEFiler_prefix                      :: Text -- ^
  , scheduleEFiler_suffix                      :: Text -- ^
  , scheduleEFiling_form                       :: Text -- ^
  , scheduleEImage_number                      :: Text -- ^
  , scheduleEIndependent_sign_date             :: Date -- ^
  , scheduleEIndependent_sign_name             :: Text -- ^
  , scheduleEIs_notice                         :: Bool -- ^
  , scheduleELine_number                       :: Text -- ^
  , scheduleELink_id                           :: Int -- ^
  , scheduleEMemo_code                         :: Text -- ^
  , scheduleEMemo_code_full                    :: Text -- ^
  , scheduleEMemo_text                         :: Text -- ^
  , scheduleEMemoed_subtotal                   :: Bool -- ^
  , scheduleENotary_commission_expiration_date :: Date -- ^
  , scheduleENotary_sign_date                  :: Date -- ^
  , scheduleENotary_sign_name                  :: Text -- ^
  , scheduleEOffice_total_ytd                  :: Double -- ^
  , scheduleEOriginal_sub_id                   :: Text -- ^
  , scheduleEPayee_city                        :: Text -- ^
  , scheduleEPayee_first_name                  :: Text -- ^
  , scheduleEPayee_last_name                   :: Text -- ^
  , scheduleEPayee_middle_name                 :: Text -- ^
  , scheduleEPayee_name                        :: Text -- ^
  , scheduleEPayee_prefix                      :: Text -- ^
  , scheduleEPayee_state                       :: Text -- ^
  , scheduleEPayee_street_1                    :: Text -- ^
  , scheduleEPayee_street_2                    :: Text -- ^
  , scheduleEPayee_suffix                      :: Text -- ^
  , scheduleEPayee_zip                         :: Text -- ^
  , scheduleEPdf_url                           :: Text -- ^
  , scheduleEPrevious_file_number              :: Int -- ^
  , scheduleEReport_type                       :: Text -- ^ Name of report where the underlying data comes from:     - 10D Pre-Election     - 10G Pre-General     - 10P Pre-Primary     - 10R Pre-Run-Off     - 10S Pre-Special     - 12C Pre-Convention     - 12G Pre-General     - 12P Pre-Primary     - 12R Pre-Run-Off     - 12S Pre-Special     - 30D Post-Election     - 30G Post-General     - 30P Post-Primary     - 30R Post-Run-Off     - 30S Post-Special     - 60D Post-Convention     - M1  January Monthly     - M10 October Monthly     - M11 November Monthly     - M12 December Monthly     - M2  February Monthly     - M3  March Monthly     - M4  April Monthly     - M5  May Monthly     - M6  June Monthly     - M7  July Monthly     - M8  August Monthly     - M9  September Monthly     - MY  Mid-Year Report     - Q1  April Quarterly     - Q2  July Quarterly     - Q3  October Quarterly     - TER Termination Report     - YE  Year-End     - 90S Post Inaugural Supplement     - 90D Post Inaugural     - 48  48 Hour Notification     - 24  24 Hour Notification     - M7S July Monthly/Semi-Annual     - MSA Monthly Semi-Annual (MY)     - MYS Monthly Year End/Semi-Annual     - Q2S July Quarterly/Semi-Annual     - QSA Quarterly Semi-Annual (MY)     - QYS Quarterly Year End/Semi-Annual     - QYE Quarterly Semi-Annual (YE)     - QMS Quarterly Mid-Year/ Semi-Annual     - MSY Monthly Semi-Annual (YE)
  , scheduleEReport_year                       :: Int -- ^  Forms with coverage date -      year from the coverage ending date. Forms without coverage date -      year from the receipt date.
  , scheduleESchedule_type                     :: Text -- ^
  , scheduleESchedule_type_full                :: Text -- ^
  , scheduleESub_id                            :: Text -- ^
  , scheduleESupport_oppose_indicator          :: Text -- ^
  , scheduleETransaction_id                    :: Int -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON ScheduleE where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "scheduleE")
instance ToJSON ScheduleE where
  toJSON = genericToJSON (removeFieldLabelPrefix False "scheduleE")

-- |
data ScheduleEByCandidate = ScheduleEByCandidate
  { scheduleEByCandidateCandidate_id             :: Text -- ^
  , scheduleEByCandidateCandidate_name           :: Text -- ^
  , scheduleEByCandidateCommittee_id             :: Text -- ^
  , scheduleEByCandidateCommittee_name           :: Text -- ^
  , scheduleEByCandidateCount                    :: Int -- ^ Number of records making up the total
  , scheduleEByCandidateCycle                    :: Int -- ^  Filter records to only those that were applicable to a given two-year period.The cycle begins with an odd year and is named for its ending, even year.
  , scheduleEByCandidateSupport_oppose_indicator :: Text -- ^ Explains if the money was spent in order to support or oppose a candidate or candidates. (Coded S or O for support or oppose.) This indicator applies to independent expenditures and communication costs.
  , scheduleEByCandidateTotal                    :: Double -- ^
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
  { scheduleEEfileAmendment_indicator           :: Text -- ^
  , scheduleEEfileBack_reference_schedule_name  :: Text -- ^
  , scheduleEEfileBack_reference_transaction_id :: Text -- ^
  , scheduleEEfileBeginning_image_number        :: Text -- ^
  , scheduleEEfileCand_office_district          :: Text -- ^ Two-digit US House distirict of the office the candidate is running for. Presidential, Senate and House at-large candidates will have District 00.
  , scheduleEEfileCand_office_state             :: Text -- ^ US state or territory
  , scheduleEEfileCandidate_first_name          :: Text -- ^
  , scheduleEEfileCandidate_id                  :: Text -- ^
  , scheduleEEfileCandidate_middle_name         :: Text -- ^
  , scheduleEEfileCandidate_name                :: Text -- ^ Name of candidate running for office
  , scheduleEEfileCandidate_office              :: Text -- ^ Federal office candidate runs for: H, S or P
  , scheduleEEfileCandidate_prefix              :: Text -- ^
  , scheduleEEfileCandidate_suffix              :: Text -- ^
  , scheduleEEfileCategory_code                 :: Text -- ^
  , scheduleEEfileCommittee                     :: CommitteeHistory -- ^
  , scheduleEEfileCommittee_id                  :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , scheduleEEfileCsv_url                       :: Text -- ^
  , scheduleEEfileDissemination_date            :: Date -- ^
  , scheduleEEfileEntity_type                   :: Text -- ^
  , scheduleEEfileExpenditure_amount            :: Int -- ^
  , scheduleEEfileExpenditure_date              :: Date -- ^
  , scheduleEEfileExpenditure_description       :: Text -- ^
  , scheduleEEfileFec_url                       :: Text -- ^
  , scheduleEEfileFile_number                   :: Int -- ^
  , scheduleEEfileFiler_first_name              :: Text -- ^
  , scheduleEEfileFiler_last_name               :: Text -- ^
  , scheduleEEfileFiler_middle_name             :: Text -- ^
  , scheduleEEfileFiler_prefix                  :: Text -- ^
  , scheduleEEfileFiler_suffix                  :: Text -- ^
  , scheduleEEfileFiling                        :: EFilings -- ^
  , scheduleEEfileImage_number                  :: Text -- ^ An unique identifier for each page the electronic or paper report.
  , scheduleEEfileIs_notice                     :: Bool -- ^
  , scheduleEEfileLine_number                   :: Text -- ^
  , scheduleEEfileLoad_timestamp                :: Integer -- ^
  , scheduleEEfileMemo_code                     :: Text -- ^
  , scheduleEEfileMemo_text                     :: Text -- ^
  , scheduleEEfileNotary_sign_date              :: Date -- ^
  , scheduleEEfileOffice_total_ytd              :: Float -- ^
  , scheduleEEfilePayee_city                    :: Text -- ^
  , scheduleEEfilePayee_first_name              :: Text -- ^
  , scheduleEEfilePayee_last_name               :: Text -- ^
  , scheduleEEfilePayee_middle_name             :: Text -- ^
  , scheduleEEfilePayee_name                    :: Text -- ^
  , scheduleEEfilePayee_prefix                  :: Text -- ^
  , scheduleEEfilePayee_state                   :: Text -- ^
  , scheduleEEfilePayee_street_1                :: Text -- ^
  , scheduleEEfilePayee_street_2                :: Text -- ^
  , scheduleEEfilePayee_suffix                  :: Text -- ^
  , scheduleEEfilePayee_zip                     :: Text -- ^
  , scheduleEEfilePdf_url                       :: Text -- ^
  , scheduleEEfileRelated_line_number           :: Int -- ^
  , scheduleEEfileReport_type                   :: Text -- ^
  , scheduleEEfileSupport_oppose_indicator      :: Text -- ^
  , scheduleEEfileTransaction_id                :: Text -- ^
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
  { seekInfoCount        :: Int -- ^
  , seekInfoLast_indexes :: Text -- ^
  , seekInfoPages        :: Int -- ^
  , seekInfoPer_page     :: Int -- ^
  } deriving (Show, Eq, Generic)

instance FromJSON SeekInfo where
  parseJSON = genericParseJSON (removeFieldLabelPrefix True "seekInfo")
instance ToJSON SeekInfo where
  toJSON = genericToJSON (removeFieldLabelPrefix False "seekInfo")

-- |
data StateElectionOfficeInfo = StateElectionOfficeInfo
  { stateElectionOfficeInfoAddress_line1          :: Text -- ^
  , stateElectionOfficeInfoAddress_line2          :: Text -- ^
  , stateElectionOfficeInfoCity                   :: Text -- ^
  , stateElectionOfficeInfoEmail                  :: Text -- ^
  , stateElectionOfficeInfoFax_number             :: Text -- ^
  , stateElectionOfficeInfoMailing_address1       :: Text -- ^
  , stateElectionOfficeInfoMailing_address2       :: Text -- ^
  , stateElectionOfficeInfoMailing_city           :: Text -- ^
  , stateElectionOfficeInfoMailing_state          :: Text -- ^
  , stateElectionOfficeInfoMailing_zipcode        :: Text -- ^
  , stateElectionOfficeInfoOffice_name            :: Text -- ^
  , stateElectionOfficeInfoOffice_type            :: Text -- ^
  , stateElectionOfficeInfoPrimary_phone_number   :: Text -- ^
  , stateElectionOfficeInfoSecondary_phone_number :: Text -- ^
  , stateElectionOfficeInfoState                  :: Text -- ^
  , stateElectionOfficeInfoState_full_name        :: Text -- ^
  , stateElectionOfficeInfoWebsite_url1           :: Text -- ^
  , stateElectionOfficeInfoWebsite_url2           :: Text -- ^
  , stateElectionOfficeInfoZip_code               :: Text -- ^
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
  { totalsCommitteeCandidate_ids            :: [Text] -- ^  A unique identifier assigned to each candidate registered with the FEC. If a person runs for several offices, that person will have separate candidate IDs for each office.
  , totalsCommitteeCash_on_hand_end_period  :: Double -- ^
  , totalsCommitteeCity                     :: Text -- ^ City of committee as reported on the Form 1
  , totalsCommitteeCommittee_id             :: Text -- ^  A unique identifier assigned to each committee or filer registered with the FEC. In general committee id's begin with the letter C which is followed by eight digits.
  , totalsCommitteeCommittee_type           :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , totalsCommitteeCommittee_type_full      :: Text -- ^ The one-letter type code of the organization:         - C communication cost         - D delegate         - E electioneering communication         - H House         - I independent expenditor (person or group)         - N PAC - nonqualified         - O independent expenditure-only (super PACs)         - P presidential         - Q PAC - qualified         - S Senate         - U single candidate independent expenditure         - V PAC with non-contribution account, nonqualified         - W PAC with non-contribution account, qualified         - X party, nonqualified         - Y party, qualified         - Z national party non-federal account
  , totalsCommitteeCycle                    :: Int -- ^  A two year election cycle that the committee was active- (after original registration date but before expiration date in FEC Form 1s) The cycle begins with an odd year and is named for its ending, even year.
  , totalsCommitteeCycles                   :: [Int] -- ^  A two year election cycle that the committee was active- (after original registration date but before expiration date in FEC Form 1s) The cycle begins with an odd year and is named for its ending, even year.
  , totalsCommitteeDebts_owed_by_committee  :: Double -- ^
  , totalsCommitteeDesignation              :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , totalsCommitteeDesignation_full         :: Text -- ^ The one-letter designation code of the organization:          - A authorized by a candidate          - J joint fundraising committee          - P principal campaign committee of a candidate          - U unauthorized          - B lobbyist/registrant PAC          - D leadership PAC
  , totalsCommitteeDisbursements            :: Double -- ^
  , totalsCommitteeFiling_frequency         :: Text -- ^ The one-letter      code of the filing frequency:          - A Administratively terminated          - D Debt          - M Monthly filer          - Q Quarterly filer          - T Terminated          - W Waived
  , totalsCommitteeIndependent_expenditures :: Double -- ^
  , totalsCommitteeName                     :: Text -- ^ The name of the committee. If a committee changes its name,     the most recent name will be shown. Committee names are not unique. Use committee_id     for looking up records.
  , totalsCommitteeOrganization_type        :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , totalsCommitteeOrganization_type_full   :: Text -- ^ The one-letter code for the kind for organization:         - C corporation         - L labor organization         - M membership organization         - T trade association         - V cooperative         - W corporation without capital stock
  , totalsCommitteeParty                    :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , totalsCommitteeParty_full               :: Text -- ^ Three-letter code for the party affiliated with a candidate or committee. For example, DEM for Democratic Party and REP for Republican Party.
  , totalsCommitteeReceipts                 :: Double -- ^
  , totalsCommitteeState                    :: Text -- ^ State of the committee's address as filed on the Form 1
  , totalsCommitteeState_full               :: Text -- ^ State of committee as reported on the Form 1
  , totalsCommitteeStreet_1                 :: Text -- ^ Street address of committee as reported on the Form 1
  , totalsCommitteeStreet_2                 :: Text -- ^ Second line of street address of committee as reported on the Form 1
  , totalsCommitteeTreasurer_name           :: Text -- ^ Name of the Committee's treasurer. If multiple treasurers for the committee, the most recent treasurer will be shown.
  , totalsCommitteeZip                      :: Text -- ^ Zip code of committee as reported on the Form 1
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
--      , ("_", "_")
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
