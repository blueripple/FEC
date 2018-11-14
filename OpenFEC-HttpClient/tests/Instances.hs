{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import OpenFEC.Model
import OpenFEC.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

-- * Models
 
instance Arbitrary AuditCandidateSearch where
  arbitrary =
    AuditCandidateSearch
      <$> arbitrary -- auditCandidateSearchId :: Maybe Text
      <*> arbitrary -- auditCandidateSearchName :: Maybe Text
    
instance Arbitrary AuditCandidateSearchList where
  arbitrary =
    AuditCandidateSearchList
      <$> arbitrary -- auditCandidateSearchListResults :: Maybe [AuditCandidateSearch]
    
instance Arbitrary AuditCase where
  arbitrary =
    AuditCase
      <$> arbitrary -- auditCaseAuditCaseId :: Maybe Text
      <*> arbitrary -- auditCaseAuditId :: Maybe Int
      <*> arbitrary -- auditCaseCandidateId :: Maybe Text
      <*> arbitrary -- auditCaseCandidateName :: Maybe Text
      <*> arbitrary -- auditCaseCommitteeDescription :: Maybe Text
      <*> arbitrary -- auditCaseCommitteeDesignation :: Maybe Text
      <*> arbitrary -- auditCaseCommitteeId :: Maybe Text
      <*> arbitrary -- auditCaseCommitteeName :: Maybe Text
      <*> arbitrary -- auditCaseCommitteeType :: Maybe Text
      <*> arbitrary -- auditCaseCycle :: Maybe Int
      <*> arbitrary -- auditCaseFarReleaseDate :: Maybe Date
      <*> arbitrary -- auditCaseLinkToReport :: Maybe Text
      <*> arbitrary -- auditCasePrimaryCategoryList :: Maybe [AuditCaseCategoryRelation]
    
instance Arbitrary AuditCaseCategoryRelation where
  arbitrary =
    AuditCaseCategoryRelation
      <$> arbitrary -- auditCaseCategoryRelationPrimaryCategoryId :: Maybe Text
      <*> arbitrary -- auditCaseCategoryRelationPrimaryCategoryName :: Maybe Text
      <*> arbitrary -- auditCaseCategoryRelationSubCategoryList :: Maybe [AuditCaseSubCategory]
    
instance Arbitrary AuditCaseCategoryRelationPage where
  arbitrary =
    AuditCaseCategoryRelationPage
      <$> arbitrary -- auditCaseCategoryRelationPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- auditCaseCategoryRelationPageResults :: Maybe [AuditCaseCategoryRelation]
    
instance Arbitrary AuditCasePage where
  arbitrary =
    AuditCasePage
      <$> arbitrary -- auditCasePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- auditCasePageResults :: Maybe [AuditCase]
    
instance Arbitrary AuditCaseSubCategory where
  arbitrary =
    AuditCaseSubCategory
      <$> arbitrary -- auditCaseSubCategorySubCategoryId :: Maybe Text
      <*> arbitrary -- auditCaseSubCategorySubCategoryName :: Maybe Text
    
instance Arbitrary AuditCaseSubCategoryPage where
  arbitrary =
    AuditCaseSubCategoryPage
      <$> arbitrary -- auditCaseSubCategoryPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- auditCaseSubCategoryPageResults :: Maybe [AuditCaseSubCategory]
    
instance Arbitrary AuditCategory where
  arbitrary =
    AuditCategory
      <$> arbitrary -- auditCategoryPrimaryCategoryId :: Maybe Text
      <*> arbitrary -- auditCategoryPrimaryCategoryName :: Maybe Text
      <*> arbitrary -- auditCategorySubCategoryList :: Maybe [AuditCategoryRelation]
    
instance Arbitrary AuditCategoryPage where
  arbitrary =
    AuditCategoryPage
      <$> arbitrary -- auditCategoryPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- auditCategoryPageResults :: Maybe [AuditCategory]
    
instance Arbitrary AuditCategoryRelation where
  arbitrary =
    AuditCategoryRelation
      <$> arbitrary -- auditCategoryRelationSubCategoryId :: Maybe Text
      <*> arbitrary -- auditCategoryRelationSubCategoryName :: Maybe Text
    
instance Arbitrary AuditCategoryRelationPage where
  arbitrary =
    AuditCategoryRelationPage
      <$> arbitrary -- auditCategoryRelationPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- auditCategoryRelationPageResults :: Maybe [AuditCategoryRelation]
    
instance Arbitrary AuditCommitteeSearch where
  arbitrary =
    AuditCommitteeSearch
      <$> arbitrary -- auditCommitteeSearchId :: Maybe Text
      <*> arbitrary -- auditCommitteeSearchName :: Maybe Text
    
instance Arbitrary AuditCommitteeSearchList where
  arbitrary =
    AuditCommitteeSearchList
      <$> arbitrary -- auditCommitteeSearchListResults :: Maybe [AuditCommitteeSearch]
    
instance Arbitrary AuditPrimaryCategory where
  arbitrary =
    AuditPrimaryCategory
      <$> arbitrary -- auditPrimaryCategoryPrimaryCategoryId :: Maybe Text
      <*> arbitrary -- auditPrimaryCategoryPrimaryCategoryName :: Maybe Text
    
instance Arbitrary AuditPrimaryCategoryPage where
  arbitrary =
    AuditPrimaryCategoryPage
      <$> arbitrary -- auditPrimaryCategoryPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- auditPrimaryCategoryPageResults :: Maybe [AuditPrimaryCategory]
    
instance Arbitrary BaseF3Filing where
  arbitrary =
    BaseF3Filing
      <$> arbitrary -- baseF3FilingAmendedAddress :: Maybe Text
      <*> arbitrary -- baseF3FilingAmendedBy :: Maybe Int
      <*> arbitrary -- baseF3FilingAmendment :: Maybe Text
      <*> arbitrary -- baseF3FilingAmendmentChain :: Maybe [Int]
      <*> arbitrary -- baseF3FilingBeginningImageNumber :: Maybe Text
      <*> arbitrary -- baseF3FilingCandidateFirstName :: Maybe Text
      <*> arbitrary -- baseF3FilingCandidateId :: Maybe Text
      <*> arbitrary -- baseF3FilingCandidateLastName :: Maybe Text
      <*> arbitrary -- baseF3FilingCandidateMiddleName :: Maybe Text
      <*> arbitrary -- baseF3FilingCandidateName :: Maybe Text
      <*> arbitrary -- baseF3FilingCandidatePrefix :: Maybe Text
      <*> arbitrary -- baseF3FilingCandidateSuffix :: Maybe Text
      <*> arbitrary -- baseF3FilingCashOnHandBeginningPeriod :: Maybe Int
      <*> arbitrary -- baseF3FilingCity :: Maybe Text
      <*> arbitrary -- baseF3FilingCommitteeId :: Maybe Text
      <*> arbitrary -- baseF3FilingCommitteeName :: Maybe Text
      <*> arbitrary -- baseF3FilingCoverageEndDate :: Maybe Date
      <*> arbitrary -- baseF3FilingCoverageStartDate :: Maybe Date
      <*> arbitrary -- baseF3FilingCsvUrl :: Maybe Text
      <*> arbitrary -- baseF3FilingDistrict :: Maybe Int
      <*> arbitrary -- baseF3FilingDocumentDescription :: Maybe Text
      <*> arbitrary -- baseF3FilingElectionDate :: Maybe Date
      <*> arbitrary -- baseF3FilingElectionState :: Maybe Text
      <*> arbitrary -- baseF3FilingF3z1 :: Maybe Int
      <*> arbitrary -- baseF3FilingFecFileId :: Maybe Text
      <*> arbitrary -- baseF3FilingFecUrl :: Maybe Text
      <*> arbitrary -- baseF3FilingFileNumber :: Maybe Int
      <*> arbitrary -- baseF3FilingGeneralElection :: Maybe Text
      <*> arbitrary -- baseF3FilingIsAmended :: Maybe Bool
      <*> arbitrary -- baseF3FilingMostRecent :: Maybe Bool
      <*> arbitrary -- baseF3FilingMostRecentFiling :: Maybe Int
      <*> arbitrary -- baseF3FilingPdfUrl :: Maybe Text
      <*> arbitrary -- baseF3FilingPrefix :: Maybe Text
      <*> arbitrary -- baseF3FilingPrimaryElection :: Maybe Text
      <*> arbitrary -- baseF3FilingReceiptDate :: Maybe Date
      <*> arbitrary -- baseF3FilingReport :: Maybe Text
      <*> arbitrary -- baseF3FilingReportType :: Maybe Text
      <*> arbitrary -- baseF3FilingReportYear :: Maybe Int
      <*> arbitrary -- baseF3FilingRptPgi :: Maybe Text
      <*> arbitrary -- baseF3FilingRunoffElection :: Maybe Text
      <*> arbitrary -- baseF3FilingSignDate :: Maybe Date
      <*> arbitrary -- baseF3FilingSpecialElection :: Maybe Text
      <*> arbitrary -- baseF3FilingState :: Maybe Text
      <*> arbitrary -- baseF3FilingStreet1 :: Maybe Text
      <*> arbitrary -- baseF3FilingStreet2 :: Maybe Text
      <*> arbitrary -- baseF3FilingSuffix :: Maybe Text
      <*> arbitrary -- baseF3FilingSummaryLines :: Maybe Text
      <*> arbitrary -- baseF3FilingTreasurerFirstName :: Maybe Text
      <*> arbitrary -- baseF3FilingTreasurerLastName :: Maybe Text
      <*> arbitrary -- baseF3FilingTreasurerMiddleName :: Maybe Text
      <*> arbitrary -- baseF3FilingTreasurerName :: Maybe Text
      <*> arbitrary -- baseF3FilingZip :: Maybe Text
    
instance Arbitrary BaseF3FilingPage where
  arbitrary =
    BaseF3FilingPage
      <$> arbitrary -- baseF3FilingPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- baseF3FilingPageResults :: Maybe [BaseF3Filing]
    
instance Arbitrary BaseF3PFiling where
  arbitrary =
    BaseF3PFiling
      <$> arbitrary -- baseF3PFilingAmendedBy :: Maybe Int
      <*> arbitrary -- baseF3PFilingAmendment :: Maybe Text
      <*> arbitrary -- baseF3PFilingAmendmentChain :: Maybe [Int]
      <*> arbitrary -- baseF3PFilingBeginningImageNumber :: Maybe Text
      <*> arbitrary -- baseF3PFilingCashOnHandBeginningPeriod :: Maybe Float
      <*> arbitrary -- baseF3PFilingCashOnHandEndPeriod :: Maybe Float
      <*> arbitrary -- baseF3PFilingCity :: Maybe Text
      <*> arbitrary -- baseF3PFilingCommitteeId :: Maybe Text
      <*> arbitrary -- baseF3PFilingCommitteeName :: Maybe Text
      <*> arbitrary -- baseF3PFilingCoverageEndDate :: Maybe Date
      <*> arbitrary -- baseF3PFilingCoverageStartDate :: Maybe Date
      <*> arbitrary -- baseF3PFilingCsvUrl :: Maybe Text
      <*> arbitrary -- baseF3PFilingDebtsOwedByCommittee :: Maybe Float
      <*> arbitrary -- baseF3PFilingDebtsOwedToCommittee :: Maybe Float
      <*> arbitrary -- baseF3PFilingDocumentDescription :: Maybe Text
      <*> arbitrary -- baseF3PFilingElectionDate :: Maybe Date
      <*> arbitrary -- baseF3PFilingElectionState :: Maybe Text
      <*> arbitrary -- baseF3PFilingExpenditureSubjectToLimits :: Maybe Float
      <*> arbitrary -- baseF3PFilingFecFileId :: Maybe Text
      <*> arbitrary -- baseF3PFilingFecUrl :: Maybe Text
      <*> arbitrary -- baseF3PFilingFileNumber :: Maybe Int
      <*> arbitrary -- baseF3PFilingGeneralElection :: Maybe Text
      <*> arbitrary -- baseF3PFilingIsAmended :: Maybe Bool
      <*> arbitrary -- baseF3PFilingMostRecent :: Maybe Bool
      <*> arbitrary -- baseF3PFilingMostRecentFiling :: Maybe Int
      <*> arbitrary -- baseF3PFilingNetContributionsCycleToDate :: Maybe Float
      <*> arbitrary -- baseF3PFilingNetOperatingExpendituresCycleToDate :: Maybe Float
      <*> arbitrary -- baseF3PFilingPdfUrl :: Maybe Text
      <*> arbitrary -- baseF3PFilingPrefix :: Maybe Text
      <*> arbitrary -- baseF3PFilingPrimaryElection :: Maybe Text
      <*> arbitrary -- baseF3PFilingReceiptDate :: Maybe Date
      <*> arbitrary -- baseF3PFilingReport :: Maybe Text
      <*> arbitrary -- baseF3PFilingReportType :: Maybe Text
      <*> arbitrary -- baseF3PFilingReportYear :: Maybe Int
      <*> arbitrary -- baseF3PFilingRptPgi :: Maybe Text
      <*> arbitrary -- baseF3PFilingSignDate :: Maybe Date
      <*> arbitrary -- baseF3PFilingState :: Maybe Text
      <*> arbitrary -- baseF3PFilingStreet1 :: Maybe Text
      <*> arbitrary -- baseF3PFilingStreet2 :: Maybe Text
      <*> arbitrary -- baseF3PFilingSubtotalSummaryPeriod :: Maybe Text
      <*> arbitrary -- baseF3PFilingSuffix :: Maybe Text
      <*> arbitrary -- baseF3PFilingSummaryLines :: Maybe Text
      <*> arbitrary -- baseF3PFilingTreasurerFirstName :: Maybe Text
      <*> arbitrary -- baseF3PFilingTreasurerLastName :: Maybe Text
      <*> arbitrary -- baseF3PFilingTreasurerMiddleName :: Maybe Text
      <*> arbitrary -- baseF3PFilingTreasurerName :: Maybe Text
      <*> arbitrary -- baseF3PFilingZip :: Maybe Text
    
instance Arbitrary BaseF3PFilingPage where
  arbitrary =
    BaseF3PFilingPage
      <$> arbitrary -- baseF3PFilingPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- baseF3PFilingPageResults :: Maybe [BaseF3PFiling]
    
instance Arbitrary BaseF3XFiling where
  arbitrary =
    BaseF3XFiling
      <$> arbitrary -- baseF3XFilingAmendAddress :: Maybe Text
      <*> arbitrary -- baseF3XFilingAmendedBy :: Maybe Int
      <*> arbitrary -- baseF3XFilingAmendment :: Maybe Text
      <*> arbitrary -- baseF3XFilingAmendmentChain :: Maybe [Int]
      <*> arbitrary -- baseF3XFilingBeginningImageNumber :: Maybe Text
      <*> arbitrary -- baseF3XFilingCity :: Maybe Text
      <*> arbitrary -- baseF3XFilingCommitteeId :: Maybe Text
      <*> arbitrary -- baseF3XFilingCommitteeName :: Maybe Text
      <*> arbitrary -- baseF3XFilingCoverageEndDate :: Maybe Date
      <*> arbitrary -- baseF3XFilingCoverageStartDate :: Maybe Date
      <*> arbitrary -- baseF3XFilingCsvUrl :: Maybe Text
      <*> arbitrary -- baseF3XFilingDocumentDescription :: Maybe Text
      <*> arbitrary -- baseF3XFilingElectionDate :: Maybe Date
      <*> arbitrary -- baseF3XFilingElectionState :: Maybe Text
      <*> arbitrary -- baseF3XFilingFecFileId :: Maybe Text
      <*> arbitrary -- baseF3XFilingFecUrl :: Maybe Text
      <*> arbitrary -- baseF3XFilingFileNumber :: Maybe Int
      <*> arbitrary -- baseF3XFilingIsAmended :: Maybe Bool
      <*> arbitrary -- baseF3XFilingMostRecent :: Maybe Bool
      <*> arbitrary -- baseF3XFilingMostRecentFiling :: Maybe Int
      <*> arbitrary -- baseF3XFilingPdfUrl :: Maybe Text
      <*> arbitrary -- baseF3XFilingQualifiedMulticandidateCommittee :: Maybe Text
      <*> arbitrary -- baseF3XFilingReceiptDate :: Maybe Date
      <*> arbitrary -- baseF3XFilingReport :: Maybe Text
      <*> arbitrary -- baseF3XFilingReportType :: Maybe Text
      <*> arbitrary -- baseF3XFilingReportYear :: Maybe Int
      <*> arbitrary -- baseF3XFilingRptPgi :: Maybe Text
      <*> arbitrary -- baseF3XFilingSignDate :: Maybe Date
      <*> arbitrary -- baseF3XFilingState :: Maybe Text
      <*> arbitrary -- baseF3XFilingStreet1 :: Maybe Text
      <*> arbitrary -- baseF3XFilingStreet2 :: Maybe Text
      <*> arbitrary -- baseF3XFilingSummaryLines :: Maybe Text
      <*> arbitrary -- baseF3XFilingZip :: Maybe Text
    
instance Arbitrary BaseF3XFilingPage where
  arbitrary =
    BaseF3XFilingPage
      <$> arbitrary -- baseF3XFilingPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- baseF3XFilingPageResults :: Maybe [BaseF3XFiling]
    
instance Arbitrary CalendarDate where
  arbitrary =
    CalendarDate
      <$> arbitrary -- calendarDateAllDay :: Maybe Bool
      <*> arbitrary -- calendarDateCalendarCategoryId :: Maybe Int
      <*> arbitrary -- calendarDateCategory :: Maybe Text
      <*> arbitrary -- calendarDateDescription :: Maybe Text
      <*> arbitrary -- calendarDateEndDate :: Maybe Text
      <*> arbitrary -- calendarDateEventId :: Maybe Int
      <*> arbitrary -- calendarDateLocation :: Maybe Text
      <*> arbitrary -- calendarDateStartDate :: Maybe Text
      <*> arbitrary -- calendarDateState :: Maybe [Text]
      <*> arbitrary -- calendarDateSummary :: Maybe Text
      <*> arbitrary -- calendarDateUrl :: Maybe Text
    
instance Arbitrary CalendarDatePage where
  arbitrary =
    CalendarDatePage
      <$> arbitrary -- calendarDatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- calendarDatePageResults :: Maybe [CalendarDate]
    
instance Arbitrary Candidate where
  arbitrary =
    Candidate
      <$> arbitrary -- candidateActiveThrough :: Maybe Int
      <*> arbitrary -- candidateCandidateId :: Maybe Text
      <*> arbitrary -- candidateCandidateStatus :: Maybe Text
      <*> arbitrary -- candidateCycles :: Maybe [Int]
      <*> arbitrary -- candidateDistrict :: Maybe Text
      <*> arbitrary -- candidateDistrictNumber :: Maybe Int
      <*> arbitrary -- candidateElectionDistricts :: Maybe [Text]
      <*> arbitrary -- candidateElectionYears :: Maybe [Int]
      <*> arbitrary -- candidateFederalFundsFlag :: Maybe Bool
      <*> arbitrary -- candidateFirstFileDate :: Maybe Date
      <*> arbitrary -- candidateHasRaisedFunds :: Maybe Bool
      <*> arbitrary -- candidateIncumbentChallenge :: Maybe Text
      <*> arbitrary -- candidateIncumbentChallengeFull :: Maybe Text
      <*> arbitrary -- candidateLastF2Date :: Maybe Date
      <*> arbitrary -- candidateLastFileDate :: Maybe Date
      <*> arbitrary -- candidateLoadDate :: Maybe Date
      <*> arbitrary -- candidateName :: Maybe Text
      <*> arbitrary -- candidateOffice :: Maybe Text
      <*> arbitrary -- candidateOfficeFull :: Maybe Text
      <*> arbitrary -- candidateParty :: Maybe Text
      <*> arbitrary -- candidatePartyFull :: Maybe Text
      <*> arbitrary -- candidatePrincipalCommittees :: Maybe [Committee]
      <*> arbitrary -- candidateState :: Maybe Text
    
instance Arbitrary CandidateCommitteeTotalsHouseSenate where
  arbitrary =
    CandidateCommitteeTotalsHouseSenate
      <$> arbitrary -- candidateCommitteeTotalsHouseSenateAllOtherLoans :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateCandidateContribution :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateCandidateId :: Text
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateContributionRefunds :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateCycle :: Int
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateDisbursements :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateExemptLegalAccountingDisbursement :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateFederalFunds :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateFullElection :: Bool
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateFundraisingDisbursements :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateIndividualContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateIndividualItemizedContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateIndividualUnitemizedContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLastBeginningImageNumber :: Maybe Text
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLastCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLastDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLastDebtsOwedToCommittee :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLastNetContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLastNetOperatingExpenditures :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLastReportTypeFull :: Maybe Text
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLastReportYear :: Maybe Int
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLoanRepayments :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLoanRepaymentsCandidateLoans :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLoanRepaymentsOtherLoans :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLoans :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateLoansMadeByCandidate :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateNetContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateNetOperatingExpenditures :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateOffsetsToFundraisingExpenditures :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateOffsetsToLegalAccounting :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateOffsetsToOperatingExpenditures :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateOperatingExpenditures :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateOtherDisbursements :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateOtherPoliticalCommitteeContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateOtherReceipts :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenatePoliticalPartyCommitteeContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateReceipts :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateRefundedIndividualContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateRefundedOtherPoliticalCommitteeContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateRefundedPoliticalPartyCommitteeContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateTotalOffsetsToOperatingExpenditures :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateTransactionCoverageDate :: Maybe DateTime
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateTransfersFromOtherAuthorizedCommittee :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsHouseSenateTransfersToOtherAuthorizedCommittee :: Maybe Double
    
instance Arbitrary CandidateCommitteeTotalsHouseSenatePage where
  arbitrary =
    CandidateCommitteeTotalsHouseSenatePage
      <$> arbitrary -- candidateCommitteeTotalsHouseSenatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- candidateCommitteeTotalsHouseSenatePageResults :: Maybe [CandidateCommitteeTotalsHouseSenate]
    
instance Arbitrary CandidateCommitteeTotalsPresidential where
  arbitrary =
    CandidateCommitteeTotalsPresidential
      <$> arbitrary -- candidateCommitteeTotalsPresidentialCandidateContribution :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialCandidateId :: Text
      <*> arbitrary -- candidateCommitteeTotalsPresidentialContributionRefunds :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- candidateCommitteeTotalsPresidentialCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- candidateCommitteeTotalsPresidentialCycle :: Int
      <*> arbitrary -- candidateCommitteeTotalsPresidentialDisbursements :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialExemptLegalAccountingDisbursement :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialFederalFunds :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialFullElection :: Bool
      <*> arbitrary -- candidateCommitteeTotalsPresidentialFundraisingDisbursements :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialIndividualContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialIndividualItemizedContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialIndividualUnitemizedContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialLastBeginningImageNumber :: Maybe Text
      <*> arbitrary -- candidateCommitteeTotalsPresidentialLastCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialLastDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialLastDebtsOwedToCommittee :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialLastReportTypeFull :: Maybe Text
      <*> arbitrary -- candidateCommitteeTotalsPresidentialLastReportYear :: Maybe Int
      <*> arbitrary -- candidateCommitteeTotalsPresidentialLoanRepaymentsMade :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialLoansReceived :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialLoansReceivedFromCandidate :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialNetContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialNetOperatingExpenditures :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialOffsetsToFundraisingExpenditures :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialOffsetsToLegalAccounting :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialOffsetsToOperatingExpenditures :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialOperatingExpenditures :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialOtherDisbursements :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialOtherLoansReceived :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialOtherPoliticalCommitteeContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialOtherReceipts :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialPoliticalPartyCommitteeContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialReceipts :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialRefundedIndividualContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialRefundedOtherPoliticalCommitteeContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialRefundedPoliticalPartyCommitteeContributions :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialRepaymentsLoansMadeByCandidate :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialRepaymentsOtherLoans :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialTotalOffsetsToOperatingExpenditures :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialTransactionCoverageDate :: Maybe DateTime
      <*> arbitrary -- candidateCommitteeTotalsPresidentialTransfersFromAffiliatedCommittee :: Maybe Double
      <*> arbitrary -- candidateCommitteeTotalsPresidentialTransfersToOtherAuthorizedCommittee :: Maybe Double
    
instance Arbitrary CandidateCommitteeTotalsPresidentialPage where
  arbitrary =
    CandidateCommitteeTotalsPresidentialPage
      <$> arbitrary -- candidateCommitteeTotalsPresidentialPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- candidateCommitteeTotalsPresidentialPageResults :: Maybe [CandidateCommitteeTotalsPresidential]
    
instance Arbitrary CandidateDetail where
  arbitrary =
    CandidateDetail
      <$> arbitrary -- candidateDetailActiveThrough :: Maybe Int
      <*> arbitrary -- candidateDetailAddressCity :: Maybe Text
      <*> arbitrary -- candidateDetailAddressState :: Maybe Text
      <*> arbitrary -- candidateDetailAddressStreet1 :: Maybe Text
      <*> arbitrary -- candidateDetailAddressStreet2 :: Maybe Text
      <*> arbitrary -- candidateDetailAddressZip :: Maybe Text
      <*> arbitrary -- candidateDetailCandidateId :: Maybe Text
      <*> arbitrary -- candidateDetailCandidateInactive :: Maybe Bool
      <*> arbitrary -- candidateDetailCandidateStatus :: Maybe Text
      <*> arbitrary -- candidateDetailCycles :: Maybe [Int]
      <*> arbitrary -- candidateDetailDistrict :: Maybe Text
      <*> arbitrary -- candidateDetailDistrictNumber :: Maybe Int
      <*> arbitrary -- candidateDetailElectionDistricts :: Maybe [Text]
      <*> arbitrary -- candidateDetailElectionYears :: Maybe [Int]
      <*> arbitrary -- candidateDetailFederalFundsFlag :: Maybe Bool
      <*> arbitrary -- candidateDetailFirstFileDate :: Maybe Date
      <*> arbitrary -- candidateDetailFlags :: Maybe Text
      <*> arbitrary -- candidateDetailHasRaisedFunds :: Maybe Bool
      <*> arbitrary -- candidateDetailIncumbentChallenge :: Maybe Text
      <*> arbitrary -- candidateDetailIncumbentChallengeFull :: Maybe Text
      <*> arbitrary -- candidateDetailLastF2Date :: Maybe Date
      <*> arbitrary -- candidateDetailLastFileDate :: Maybe Date
      <*> arbitrary -- candidateDetailLoadDate :: Maybe Date
      <*> arbitrary -- candidateDetailName :: Maybe Text
      <*> arbitrary -- candidateDetailOffice :: Maybe Text
      <*> arbitrary -- candidateDetailOfficeFull :: Maybe Text
      <*> arbitrary -- candidateDetailParty :: Maybe Text
      <*> arbitrary -- candidateDetailPartyFull :: Maybe Text
      <*> arbitrary -- candidateDetailState :: Maybe Text
    
instance Arbitrary CandidateDetailPage where
  arbitrary =
    CandidateDetailPage
      <$> arbitrary -- candidateDetailPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- candidateDetailPageResults :: Maybe [CandidateDetail]
    
instance Arbitrary CandidateFlags where
  arbitrary =
    CandidateFlags
      <$> arbitrary -- candidateFlagsCandidateId :: Text
      <*> arbitrary -- candidateFlagsFederalFundsFlag :: Maybe Bool
      <*> arbitrary -- candidateFlagsHasRaisedFunds :: Maybe Bool
    
instance Arbitrary CandidateFlagsPage where
  arbitrary =
    CandidateFlagsPage
      <$> arbitrary -- candidateFlagsPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- candidateFlagsPageResults :: Maybe [CandidateFlags]
    
instance Arbitrary CandidateHistory where
  arbitrary =
    CandidateHistory
      <$> arbitrary -- candidateHistoryActiveThrough :: Maybe Int
      <*> arbitrary -- candidateHistoryAddressCity :: Maybe Text
      <*> arbitrary -- candidateHistoryAddressState :: Maybe Text
      <*> arbitrary -- candidateHistoryAddressStreet1 :: Maybe Text
      <*> arbitrary -- candidateHistoryAddressStreet2 :: Maybe Text
      <*> arbitrary -- candidateHistoryAddressZip :: Maybe Text
      <*> arbitrary -- candidateHistoryCandidateElectionYear :: Maybe Int
      <*> arbitrary -- candidateHistoryCandidateId :: Text
      <*> arbitrary -- candidateHistoryCandidateInactive :: Maybe Bool
      <*> arbitrary -- candidateHistoryCandidateStatus :: Maybe Text
      <*> arbitrary -- candidateHistoryCycles :: Maybe [Int]
      <*> arbitrary -- candidateHistoryDistrict :: Maybe Text
      <*> arbitrary -- candidateHistoryDistrictNumber :: Maybe Int
      <*> arbitrary -- candidateHistoryElectionDistricts :: Maybe [Text]
      <*> arbitrary -- candidateHistoryElectionYears :: Maybe [Int]
      <*> arbitrary -- candidateHistoryFirstFileDate :: Maybe Date
      <*> arbitrary -- candidateHistoryFlags :: Maybe Text
      <*> arbitrary -- candidateHistoryIncumbentChallenge :: Maybe Text
      <*> arbitrary -- candidateHistoryIncumbentChallengeFull :: Maybe Text
      <*> arbitrary -- candidateHistoryLastF2Date :: Maybe Date
      <*> arbitrary -- candidateHistoryLastFileDate :: Maybe Date
      <*> arbitrary -- candidateHistoryLoadDate :: Maybe Date
      <*> arbitrary -- candidateHistoryName :: Maybe Text
      <*> arbitrary -- candidateHistoryOffice :: Maybe Text
      <*> arbitrary -- candidateHistoryOfficeFull :: Maybe Text
      <*> arbitrary -- candidateHistoryParty :: Maybe Text
      <*> arbitrary -- candidateHistoryPartyFull :: Maybe Text
      <*> arbitrary -- candidateHistoryState :: Maybe Text
      <*> arbitrary -- candidateHistoryTwoYearPeriod :: Int
    
instance Arbitrary CandidateHistoryPage where
  arbitrary =
    CandidateHistoryPage
      <$> arbitrary -- candidateHistoryPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- candidateHistoryPageResults :: Maybe [CandidateHistory]
    
instance Arbitrary CandidateHistoryTotal where
  arbitrary =
    CandidateHistoryTotal
      <$> arbitrary -- candidateHistoryTotalActiveThrough :: Maybe Int
      <*> arbitrary -- candidateHistoryTotalAddressCity :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalAddressState :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalAddressStreet1 :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalAddressStreet2 :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalAddressZip :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalCandidateElectionYear :: Maybe Int
      <*> arbitrary -- candidateHistoryTotalCandidateId :: Text
      <*> arbitrary -- candidateHistoryTotalCandidateInactive :: Maybe Bool
      <*> arbitrary -- candidateHistoryTotalCandidateStatus :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- candidateHistoryTotalCoverageEndDate :: Maybe Date
      <*> arbitrary -- candidateHistoryTotalCoverageStartDate :: Maybe Date
      <*> arbitrary -- candidateHistoryTotalCycle :: Int
      <*> arbitrary -- candidateHistoryTotalCycles :: Maybe [Int]
      <*> arbitrary -- candidateHistoryTotalDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- candidateHistoryTotalDisbursements :: Maybe Double
      <*> arbitrary -- candidateHistoryTotalDistrict :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalDistrictNumber :: Maybe Int
      <*> arbitrary -- candidateHistoryTotalElectionDistricts :: Maybe [Text]
      <*> arbitrary -- candidateHistoryTotalElectionYear :: Maybe Int
      <*> arbitrary -- candidateHistoryTotalElectionYears :: Maybe [Int]
      <*> arbitrary -- candidateHistoryTotalFederalFundsFlag :: Maybe Bool
      <*> arbitrary -- candidateHistoryTotalFirstFileDate :: Maybe Date
      <*> arbitrary -- candidateHistoryTotalFlags :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalHasRaisedFunds :: Maybe Bool
      <*> arbitrary -- candidateHistoryTotalIncumbentChallenge :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalIncumbentChallengeFull :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalIsElection :: Bool
      <*> arbitrary -- candidateHistoryTotalLastF2Date :: Maybe Date
      <*> arbitrary -- candidateHistoryTotalLastFileDate :: Maybe Date
      <*> arbitrary -- candidateHistoryTotalLoadDate :: Maybe Date
      <*> arbitrary -- candidateHistoryTotalName :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalOffice :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalOfficeFull :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalParty :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalPartyFull :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalReceipts :: Maybe Double
      <*> arbitrary -- candidateHistoryTotalState :: Maybe Text
      <*> arbitrary -- candidateHistoryTotalTwoYearPeriod :: Int
    
instance Arbitrary CandidateHistoryTotalPage where
  arbitrary =
    CandidateHistoryTotalPage
      <$> arbitrary -- candidateHistoryTotalPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- candidateHistoryTotalPageResults :: Maybe [CandidateHistoryTotal]
    
instance Arbitrary CandidatePage where
  arbitrary =
    CandidatePage
      <$> arbitrary -- candidatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- candidatePageResults :: Maybe [Candidate]
    
instance Arbitrary CandidateSearch where
  arbitrary =
    CandidateSearch
      <$> arbitrary -- candidateSearchId :: Maybe Text
      <*> arbitrary -- candidateSearchName :: Maybe Text
      <*> arbitrary -- candidateSearchOfficeSought :: Maybe Text
    
instance Arbitrary CandidateSearchList where
  arbitrary =
    CandidateSearchList
      <$> arbitrary -- candidateSearchListResults :: Maybe [CandidateSearch]
    
instance Arbitrary CandidateTotal where
  arbitrary =
    CandidateTotal
      <$> arbitrary -- candidateTotalCandidateId :: Text
      <*> arbitrary -- candidateTotalCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- candidateTotalCoverageEndDate :: Maybe Date
      <*> arbitrary -- candidateTotalCoverageStartDate :: Maybe Date
      <*> arbitrary -- candidateTotalCycle :: Int
      <*> arbitrary -- candidateTotalDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- candidateTotalDisbursements :: Maybe Double
      <*> arbitrary -- candidateTotalElectionYear :: Maybe Int
      <*> arbitrary -- candidateTotalFederalFundsFlag :: Maybe Bool
      <*> arbitrary -- candidateTotalHasRaisedFunds :: Maybe Bool
      <*> arbitrary -- candidateTotalIsElection :: Bool
      <*> arbitrary -- candidateTotalReceipts :: Maybe Double
    
instance Arbitrary CandidateTotalPage where
  arbitrary =
    CandidateTotalPage
      <$> arbitrary -- candidateTotalPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- candidateTotalPageResults :: Maybe [CandidateTotal]
    
instance Arbitrary Committee where
  arbitrary =
    Committee
      <$> arbitrary -- committeeCandidateIds :: Maybe [Text]
      <*> arbitrary -- committeeCommitteeId :: Text
      <*> arbitrary -- committeeCommitteeType :: Maybe Text
      <*> arbitrary -- committeeCommitteeTypeFull :: Maybe Text
      <*> arbitrary -- committeeCycles :: Maybe [Int]
      <*> arbitrary -- committeeDesignation :: Maybe Text
      <*> arbitrary -- committeeDesignationFull :: Maybe Text
      <*> arbitrary -- committeeFilingFrequency :: Maybe Text
      <*> arbitrary -- committeeFirstFileDate :: Maybe Date
      <*> arbitrary -- committeeLastF1Date :: Maybe Date
      <*> arbitrary -- committeeLastFileDate :: Maybe Date
      <*> arbitrary -- committeeName :: Maybe Text
      <*> arbitrary -- committeeOrganizationType :: Maybe Text
      <*> arbitrary -- committeeOrganizationTypeFull :: Maybe Text
      <*> arbitrary -- committeeParty :: Maybe Text
      <*> arbitrary -- committeePartyFull :: Maybe Text
      <*> arbitrary -- committeeState :: Maybe Text
      <*> arbitrary -- committeeTreasurerName :: Maybe Text
    
instance Arbitrary CommitteeDetail where
  arbitrary =
    CommitteeDetail
      <$> arbitrary -- committeeDetailCandidateIds :: Maybe [Text]
      <*> arbitrary -- committeeDetailCity :: Maybe Text
      <*> arbitrary -- committeeDetailCommitteeId :: Text
      <*> arbitrary -- committeeDetailCommitteeType :: Maybe Text
      <*> arbitrary -- committeeDetailCommitteeTypeFull :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianCity :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianName1 :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianName2 :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianNameFull :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianNameMiddle :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianNamePrefix :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianNameSuffix :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianNameTitle :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianPhone :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianState :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianStreet1 :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianStreet2 :: Maybe Text
      <*> arbitrary -- committeeDetailCustodianZip :: Maybe Text
      <*> arbitrary -- committeeDetailCycles :: Maybe [Int]
      <*> arbitrary -- committeeDetailDesignation :: Maybe Text
      <*> arbitrary -- committeeDetailDesignationFull :: Maybe Text
      <*> arbitrary -- committeeDetailEmail :: Maybe Text
      <*> arbitrary -- committeeDetailFax :: Maybe Text
      <*> arbitrary -- committeeDetailFilingFrequency :: Maybe Text
      <*> arbitrary -- committeeDetailFirstFileDate :: Maybe Date
      <*> arbitrary -- committeeDetailFormType :: Maybe Text
      <*> arbitrary -- committeeDetailLastFileDate :: Maybe Date
      <*> arbitrary -- committeeDetailLeadershipPac :: Maybe Text
      <*> arbitrary -- committeeDetailLobbyistRegistrantPac :: Maybe Text
      <*> arbitrary -- committeeDetailName :: Maybe Text
      <*> arbitrary -- committeeDetailOrganizationType :: Maybe Text
      <*> arbitrary -- committeeDetailOrganizationTypeFull :: Maybe Text
      <*> arbitrary -- committeeDetailParty :: Maybe Text
      <*> arbitrary -- committeeDetailPartyFull :: Maybe Text
      <*> arbitrary -- committeeDetailPartyType :: Maybe Text
      <*> arbitrary -- committeeDetailPartyTypeFull :: Maybe Text
      <*> arbitrary -- committeeDetailQualifyingDate :: Maybe Date
      <*> arbitrary -- committeeDetailState :: Maybe Text
      <*> arbitrary -- committeeDetailStateFull :: Maybe Text
      <*> arbitrary -- committeeDetailStreet1 :: Maybe Text
      <*> arbitrary -- committeeDetailStreet2 :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerCity :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerName :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerName1 :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerName2 :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerNameMiddle :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerNamePrefix :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerNameSuffix :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerNameTitle :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerPhone :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerState :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerStreet1 :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerStreet2 :: Maybe Text
      <*> arbitrary -- committeeDetailTreasurerZip :: Maybe Text
      <*> arbitrary -- committeeDetailWebsite :: Maybe Text
      <*> arbitrary -- committeeDetailZip :: Maybe Text
    
instance Arbitrary CommitteeDetailPage where
  arbitrary =
    CommitteeDetailPage
      <$> arbitrary -- committeeDetailPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeeDetailPageResults :: Maybe [CommitteeDetail]
    
instance Arbitrary CommitteeHistory where
  arbitrary =
    CommitteeHistory
      <$> arbitrary -- committeeHistoryCandidateIds :: Maybe [Text]
      <*> arbitrary -- committeeHistoryCity :: Maybe Text
      <*> arbitrary -- committeeHistoryCommitteeId :: Text
      <*> arbitrary -- committeeHistoryCommitteeType :: Maybe Text
      <*> arbitrary -- committeeHistoryCommitteeTypeFull :: Maybe Text
      <*> arbitrary -- committeeHistoryCycle :: Int
      <*> arbitrary -- committeeHistoryCycles :: Maybe [Int]
      <*> arbitrary -- committeeHistoryDesignation :: Maybe Text
      <*> arbitrary -- committeeHistoryDesignationFull :: Maybe Text
      <*> arbitrary -- committeeHistoryFilingFrequency :: Maybe Text
      <*> arbitrary -- committeeHistoryName :: Maybe Text
      <*> arbitrary -- committeeHistoryOrganizationType :: Maybe Text
      <*> arbitrary -- committeeHistoryOrganizationTypeFull :: Maybe Text
      <*> arbitrary -- committeeHistoryParty :: Maybe Text
      <*> arbitrary -- committeeHistoryPartyFull :: Maybe Text
      <*> arbitrary -- committeeHistoryState :: Maybe Text
      <*> arbitrary -- committeeHistoryStateFull :: Maybe Text
      <*> arbitrary -- committeeHistoryStreet1 :: Maybe Text
      <*> arbitrary -- committeeHistoryStreet2 :: Maybe Text
      <*> arbitrary -- committeeHistoryTreasurerName :: Maybe Text
      <*> arbitrary -- committeeHistoryZip :: Maybe Text
    
instance Arbitrary CommitteeHistoryPage where
  arbitrary =
    CommitteeHistoryPage
      <$> arbitrary -- committeeHistoryPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeeHistoryPageResults :: Maybe [CommitteeHistory]
    
instance Arbitrary CommitteePage where
  arbitrary =
    CommitteePage
      <$> arbitrary -- committeePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeePageResults :: Maybe [Committee]
    
instance Arbitrary CommitteeReports where
  arbitrary =
    CommitteeReports
      <$> arbitrary -- committeeReportsAggregateAmountPersonalContributionsGeneral :: Maybe Double
      <*> arbitrary -- committeeReportsAggregateContributionsPersonalFundsPrimary :: Maybe Double
      <*> arbitrary -- committeeReportsAllLoansReceivedPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsAllLoansReceivedYtd :: Maybe Double
      <*> arbitrary -- committeeReportsAllOtherLoansPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsAllOtherLoansYtd :: Maybe Double
      <*> arbitrary -- committeeReportsAllocatedFederalElectionLevinSharePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsAmendmentChain :: Maybe [Double]
      <*> arbitrary -- committeeReportsAmendmentIndicator :: Maybe Text
      <*> arbitrary -- committeeReportsAmendmentIndicatorFull :: Maybe Text
      <*> arbitrary -- committeeReportsBeginningImageNumber :: Maybe Text
      <*> arbitrary -- committeeReportsCalendarYtd :: Maybe Int
      <*> arbitrary -- committeeReportsCandidateContributionPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsCandidateContributionYtd :: Maybe Double
      <*> arbitrary -- committeeReportsCashOnHandBeginningCalendarYtd :: Maybe Double
      <*> arbitrary -- committeeReportsCashOnHandBeginningPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsCashOnHandCloseYtd :: Maybe Double
      <*> arbitrary -- committeeReportsCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsCommitteeId :: Maybe Text
      <*> arbitrary -- committeeReportsCommitteeName :: Maybe Text
      <*> arbitrary -- committeeReportsCommitteeType :: Maybe Text
      <*> arbitrary -- committeeReportsCoordinatedExpendituresByPartyCommitteePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsCoordinatedExpendituresByPartyCommitteeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- committeeReportsCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- committeeReportsCsvUrl :: Maybe Text
      <*> arbitrary -- committeeReportsCycle :: Maybe Int
      <*> arbitrary -- committeeReportsDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- committeeReportsDebtsOwedToCommittee :: Maybe Double
      <*> arbitrary -- committeeReportsDocumentDescription :: Maybe Text
      <*> arbitrary -- committeeReportsEndImageNumber :: Maybe Text
      <*> arbitrary -- committeeReportsExemptLegalAccountingDisbursementPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsExemptLegalAccountingDisbursementYtd :: Maybe Double
      <*> arbitrary -- committeeReportsExpenditureSubjectToLimits :: Maybe Double
      <*> arbitrary -- committeeReportsFecFileId :: Maybe Text
      <*> arbitrary -- committeeReportsFecUrl :: Maybe Text
      <*> arbitrary -- committeeReportsFedCandidateCommitteeContributionRefundsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsFedCandidateCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsFedCandidateCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsFedCandidateContributionRefundsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsFederalFundsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsFederalFundsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsFileNumber :: Maybe Int
      <*> arbitrary -- committeeReportsFundraisingDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsFundraisingDisbursementsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsGrossReceiptAuthorizedCommitteeGeneral :: Maybe Double
      <*> arbitrary -- committeeReportsGrossReceiptAuthorizedCommitteePrimary :: Maybe Double
      <*> arbitrary -- committeeReportsGrossReceiptMinusPersonalContributionGeneral :: Maybe Double
      <*> arbitrary -- committeeReportsGrossReceiptMinusPersonalContributionsPrimary :: Maybe Double
      <*> arbitrary -- committeeReportsHtmlUrl :: Maybe Text
      <*> arbitrary -- committeeReportsIndependentContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsIndependentExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsIndependentExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsIndividualItemizedContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsIndividualItemizedContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsIndividualUnitemizedContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsIndividualUnitemizedContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsIsAmended :: Maybe Bool
      <*> arbitrary -- committeeReportsItemsOnHandLiquidated :: Maybe Double
      <*> arbitrary -- committeeReportsLoanRepaymentsCandidateLoansPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsLoanRepaymentsCandidateLoansYtd :: Maybe Double
      <*> arbitrary -- committeeReportsLoanRepaymentsMadePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsLoanRepaymentsMadeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsLoanRepaymentsOtherLoansPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsLoanRepaymentsOtherLoansYtd :: Maybe Double
      <*> arbitrary -- committeeReportsLoanRepaymentsReceivedPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsLoanRepaymentsReceivedYtd :: Maybe Double
      <*> arbitrary -- committeeReportsLoansMadeByCandidatePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsLoansMadeByCandidateYtd :: Maybe Double
      <*> arbitrary -- committeeReportsLoansMadePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsLoansMadeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsLoansReceivedFromCandidatePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsLoansReceivedFromCandidateYtd :: Maybe Double
      <*> arbitrary -- committeeReportsMeansFiled :: Maybe Text
      <*> arbitrary -- committeeReportsMostRecent :: Maybe Bool
      <*> arbitrary -- committeeReportsMostRecentFileNumber :: Maybe Double
      <*> arbitrary -- committeeReportsNetContributionsCycleToDate :: Maybe Double
      <*> arbitrary -- committeeReportsNetContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsNetContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsNetOperatingExpendituresCycleToDate :: Maybe Double
      <*> arbitrary -- committeeReportsNetOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsNetOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsNonAllocatedFedElectionActivityPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsNonAllocatedFedElectionActivityYtd :: Maybe Double
      <*> arbitrary -- committeeReportsNonfedShareAllocatedDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsOffsetsToFundraisingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsOffsetsToFundraisingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsOffsetsToLegalAccountingPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsOffsetsToLegalAccountingYtd :: Maybe Double
      <*> arbitrary -- committeeReportsOffsetsToOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsOffsetsToOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsOtherDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsOtherDisbursementsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsOtherFedOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsOtherFedOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsOtherFedReceiptsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsOtherFedReceiptsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsOtherLoansReceivedPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsOtherLoansReceivedYtd :: Maybe Double
      <*> arbitrary -- committeeReportsOtherPoliticalCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsOtherPoliticalCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsOtherReceiptsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsOtherReceiptsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPdfUrl :: Maybe Text
      <*> arbitrary -- committeeReportsPoliticalPartyCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPoliticalPartyCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPreviousFileNumber :: Maybe Double
      <*> arbitrary -- committeeReportsReceiptDate :: Maybe Date
      <*> arbitrary -- committeeReportsRefundedIndividualContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsRefundedIndividualContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsRefundedOtherPoliticalCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsRefundedOtherPoliticalCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsRefundedPoliticalPartyCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsRefundedPoliticalPartyCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsRefundsTotalContributionsColTotalYtd :: Maybe Double
      <*> arbitrary -- committeeReportsRepaymentsLoansMadeByCandidatePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsRepaymentsLoansMadeCandidateYtd :: Maybe Double
      <*> arbitrary -- committeeReportsRepaymentsOtherLoansPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsRepaymentsOtherLoansYtd :: Maybe Double
      <*> arbitrary -- committeeReportsReportForm :: Maybe Text
      <*> arbitrary -- committeeReportsReportType :: Maybe Text
      <*> arbitrary -- committeeReportsReportTypeFull :: Maybe Text
      <*> arbitrary -- committeeReportsReportYear :: Maybe Int
      <*> arbitrary -- committeeReportsSharedFedActivityNonfedYtd :: Maybe Double
      <*> arbitrary -- committeeReportsSharedFedActivityPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsSharedFedActivityYtd :: Maybe Double
      <*> arbitrary -- committeeReportsSharedFedOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsSharedFedOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsSharedNonfedOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsSharedNonfedOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsSubtotalPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsSubtotalSummaryPagePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsSubtotalSummaryPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsSubtotalSummaryYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalContributionRefundsColTotalPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalContributionRefundsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalContributionRefundsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalContributionsColumnTotalPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalDisbursementsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalFedDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalFedDisbursementsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalFedElectionActivityPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalFedElectionActivityYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalFedOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalFedOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalFedReceiptsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalFedReceiptsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalIndividualContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalIndividualContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalLoanRepaymentsMadePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalLoanRepaymentsMadeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalLoansReceivedPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalLoansReceivedYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalNonfedTransfersPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalNonfedTransfersYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalOffsetsToOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalOffsetsToOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalReceiptsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTotalReceiptsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTotalYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersFromAffiliatedCommitteePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersFromAffiliatedCommitteeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersFromAffiliatedPartyPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersFromAffiliatedPartyYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersFromNonfedAccountPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersFromNonfedAccountYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersFromNonfedLevinPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersFromNonfedLevinYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersFromOtherAuthorizedCommitteePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersFromOtherAuthorizedCommitteeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersToAffiliatedCommitteePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersToAffilitatedCommitteesYtd :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersToOtherAuthorizedCommitteePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsTransfersToOtherAuthorizedCommitteeYtd :: Maybe Double
    
instance Arbitrary CommitteeReportsHouseSenate where
  arbitrary =
    CommitteeReportsHouseSenate
      <$> arbitrary -- committeeReportsHouseSenateAggregateAmountPersonalContributionsGeneral :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateAggregateContributionsPersonalFundsPrimary :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateAllOtherLoansPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateAllOtherLoansYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateAmendmentChain :: Maybe [Double]
      <*> arbitrary -- committeeReportsHouseSenateAmendmentIndicator :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateAmendmentIndicatorFull :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateBeginningImageNumber :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateCandidateContributionPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateCandidateContributionYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateCashOnHandBeginningPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateCommitteeId :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateCommitteeName :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateCommitteeType :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- committeeReportsHouseSenateCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- committeeReportsHouseSenateCsvUrl :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateCycle :: Maybe Int
      <*> arbitrary -- committeeReportsHouseSenateDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateDebtsOwedToCommittee :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateDocumentDescription :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateEndImageNumber :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateFecFileId :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateFecUrl :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateFileNumber :: Maybe Int
      <*> arbitrary -- committeeReportsHouseSenateGrossReceiptAuthorizedCommitteeGeneral :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateGrossReceiptAuthorizedCommitteePrimary :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateGrossReceiptMinusPersonalContributionGeneral :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateGrossReceiptMinusPersonalContributionsPrimary :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateHtmlUrl :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateIndividualItemizedContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateIndividualItemizedContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateIndividualUnitemizedContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateIndividualUnitemizedContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateIsAmended :: Maybe Bool
      <*> arbitrary -- committeeReportsHouseSenateLoanRepaymentsCandidateLoansPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateLoanRepaymentsCandidateLoansYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateLoanRepaymentsOtherLoansPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateLoanRepaymentsOtherLoansYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateLoansMadeByCandidatePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateLoansMadeByCandidateYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateMeansFiled :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateMostRecent :: Maybe Bool
      <*> arbitrary -- committeeReportsHouseSenateMostRecentFileNumber :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateNetContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateNetContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateNetOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateNetOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateOffsetsToOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateOffsetsToOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateOtherDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateOtherDisbursementsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateOtherPoliticalCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateOtherPoliticalCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateOtherReceiptsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateOtherReceiptsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenatePdfUrl :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenatePoliticalPartyCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenatePoliticalPartyCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenatePreviousFileNumber :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateReceiptDate :: Maybe Date
      <*> arbitrary -- committeeReportsHouseSenateRefundedIndividualContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateRefundedIndividualContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateRefundedOtherPoliticalCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateRefundedOtherPoliticalCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateRefundedPoliticalPartyCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateRefundedPoliticalPartyCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateRefundsTotalContributionsColTotalYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateReportForm :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateReportType :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateReportTypeFull :: Maybe Text
      <*> arbitrary -- committeeReportsHouseSenateReportYear :: Maybe Int
      <*> arbitrary -- committeeReportsHouseSenateSubtotalPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalContributionRefundsColTotalPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalContributionRefundsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalContributionRefundsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalContributionsColumnTotalPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalDisbursementsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalIndividualContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalIndividualContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalLoanRepaymentsMadePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalLoanRepaymentsMadeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalLoansReceivedPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalLoansReceivedYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalOffsetsToOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalOffsetsToOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalReceiptsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTotalReceiptsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTransfersFromOtherAuthorizedCommitteePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTransfersFromOtherAuthorizedCommitteeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTransfersToOtherAuthorizedCommitteePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsHouseSenateTransfersToOtherAuthorizedCommitteeYtd :: Maybe Double
    
instance Arbitrary CommitteeReportsHouseSenatePage where
  arbitrary =
    CommitteeReportsHouseSenatePage
      <$> arbitrary -- committeeReportsHouseSenatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeeReportsHouseSenatePageResults :: Maybe [CommitteeReportsHouseSenate]
    
instance Arbitrary CommitteeReportsIEOnly where
  arbitrary =
    CommitteeReportsIEOnly
      <$> arbitrary -- committeeReportsIEOnlyBeginningImageNumber :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyCommitteeId :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyCommitteeName :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyCommitteeType :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- committeeReportsIEOnlyCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- committeeReportsIEOnlyCsvUrl :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyCycle :: Maybe Int
      <*> arbitrary -- committeeReportsIEOnlyDocumentDescription :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyEndImageNumber :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyFecFileId :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyFecUrl :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyIndependentContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsIEOnlyIndependentExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsIEOnlyIsAmended :: Maybe Bool
      <*> arbitrary -- committeeReportsIEOnlyMeansFiled :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyPdfUrl :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyReceiptDate :: Maybe Date
      <*> arbitrary -- committeeReportsIEOnlyReportForm :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyReportType :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyReportTypeFull :: Maybe Text
      <*> arbitrary -- committeeReportsIEOnlyReportYear :: Maybe Int
    
instance Arbitrary CommitteeReportsIEOnlyPage where
  arbitrary =
    CommitteeReportsIEOnlyPage
      <$> arbitrary -- committeeReportsIEOnlyPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeeReportsIEOnlyPageResults :: Maybe [CommitteeReportsIEOnly]
    
instance Arbitrary CommitteeReportsPacParty where
  arbitrary =
    CommitteeReportsPacParty
      <$> arbitrary -- committeeReportsPacPartyAllLoansReceivedPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyAllLoansReceivedYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyAllocatedFederalElectionLevinSharePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyAmendmentChain :: Maybe [Double]
      <*> arbitrary -- committeeReportsPacPartyAmendmentIndicator :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyAmendmentIndicatorFull :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyBeginningImageNumber :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyCalendarYtd :: Maybe Int
      <*> arbitrary -- committeeReportsPacPartyCashOnHandBeginningCalendarYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyCashOnHandBeginningPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyCashOnHandCloseYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyCommitteeId :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyCommitteeName :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyCommitteeType :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyCoordinatedExpendituresByPartyCommitteePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyCoordinatedExpendituresByPartyCommitteeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- committeeReportsPacPartyCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- committeeReportsPacPartyCsvUrl :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyCycle :: Maybe Int
      <*> arbitrary -- committeeReportsPacPartyDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyDebtsOwedToCommittee :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyDocumentDescription :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyEndImageNumber :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyFecFileId :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyFecUrl :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyFedCandidateCommitteeContributionRefundsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyFedCandidateCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyFedCandidateCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyFedCandidateContributionRefundsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyFileNumber :: Maybe Int
      <*> arbitrary -- committeeReportsPacPartyHtmlUrl :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyIndependentExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyIndependentExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyIndividualItemizedContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyIndividualItemizedContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyIndividualUnitemizedContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyIndividualUnitemizedContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyIsAmended :: Maybe Bool
      <*> arbitrary -- committeeReportsPacPartyLoanRepaymentsMadePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyLoanRepaymentsMadeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyLoanRepaymentsReceivedPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyLoanRepaymentsReceivedYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyLoansMadePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyLoansMadeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyMeansFiled :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyMostRecent :: Maybe Bool
      <*> arbitrary -- committeeReportsPacPartyMostRecentFileNumber :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyNetContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyNetContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyNetOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyNetOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyNonAllocatedFedElectionActivityPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyNonAllocatedFedElectionActivityYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyNonfedShareAllocatedDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyOffsetsToOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyOffsetsToOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyOtherDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyOtherDisbursementsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyOtherFedOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyOtherFedOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyOtherFedReceiptsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyOtherFedReceiptsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyOtherPoliticalCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyOtherPoliticalCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyPdfUrl :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyPoliticalPartyCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyPoliticalPartyCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyPreviousFileNumber :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyReceiptDate :: Maybe Date
      <*> arbitrary -- committeeReportsPacPartyRefundedIndividualContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyRefundedIndividualContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyRefundedOtherPoliticalCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyRefundedOtherPoliticalCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyRefundedPoliticalPartyCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyRefundedPoliticalPartyCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyReportForm :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyReportType :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyReportTypeFull :: Maybe Text
      <*> arbitrary -- committeeReportsPacPartyReportYear :: Maybe Int
      <*> arbitrary -- committeeReportsPacPartySharedFedActivityNonfedYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartySharedFedActivityPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartySharedFedActivityYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartySharedFedOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartySharedFedOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartySharedNonfedOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartySharedNonfedOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartySubtotalSummaryPagePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartySubtotalSummaryYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalContributionRefundsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalContributionRefundsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalDisbursementsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalFedDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalFedDisbursementsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalFedElectionActivityPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalFedElectionActivityYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalFedOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalFedOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalFedReceiptsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalFedReceiptsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalIndividualContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalIndividualContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalNonfedTransfersPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalNonfedTransfersYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalReceiptsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTotalReceiptsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTransfersFromAffiliatedPartyPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTransfersFromAffiliatedPartyYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTransfersFromNonfedAccountPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTransfersFromNonfedAccountYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTransfersFromNonfedLevinPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTransfersFromNonfedLevinYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTransfersToAffiliatedCommitteePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPacPartyTransfersToAffilitatedCommitteesYtd :: Maybe Double
    
instance Arbitrary CommitteeReportsPacPartyPage where
  arbitrary =
    CommitteeReportsPacPartyPage
      <$> arbitrary -- committeeReportsPacPartyPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeeReportsPacPartyPageResults :: Maybe [CommitteeReportsPacParty]
    
instance Arbitrary CommitteeReportsPage where
  arbitrary =
    CommitteeReportsPage
      <$> arbitrary -- committeeReportsPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeeReportsPageResults :: Maybe [CommitteeReports]
    
instance Arbitrary CommitteeReportsPresidential where
  arbitrary =
    CommitteeReportsPresidential
      <$> arbitrary -- committeeReportsPresidentialAmendmentChain :: Maybe [Double]
      <*> arbitrary -- committeeReportsPresidentialAmendmentIndicator :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialAmendmentIndicatorFull :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialBeginningImageNumber :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialCandidateContributionPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialCandidateContributionYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialCashOnHandBeginningPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialCommitteeId :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialCommitteeName :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialCommitteeType :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- committeeReportsPresidentialCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- committeeReportsPresidentialCsvUrl :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialCycle :: Maybe Int
      <*> arbitrary -- committeeReportsPresidentialDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialDebtsOwedToCommittee :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialDocumentDescription :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialEndImageNumber :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialExemptLegalAccountingDisbursementPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialExemptLegalAccountingDisbursementYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialExpenditureSubjectToLimits :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialFecFileId :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialFecUrl :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialFederalFundsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialFederalFundsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialFileNumber :: Maybe Int
      <*> arbitrary -- committeeReportsPresidentialFundraisingDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialFundraisingDisbursementsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialHtmlUrl :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialIndividualItemizedContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialIndividualItemizedContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialIndividualUnitemizedContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialIndividualUnitemizedContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialIsAmended :: Maybe Bool
      <*> arbitrary -- committeeReportsPresidentialItemsOnHandLiquidated :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialLoansReceivedFromCandidatePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialLoansReceivedFromCandidateYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialMeansFiled :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialMostRecent :: Maybe Bool
      <*> arbitrary -- committeeReportsPresidentialMostRecentFileNumber :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialNetContributionsCycleToDate :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialNetOperatingExpendituresCycleToDate :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOffsetsToFundraisingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOffsetsToFundraisingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOffsetsToLegalAccountingPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOffsetsToLegalAccountingYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOffsetsToOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOffsetsToOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOtherDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOtherDisbursementsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOtherLoansReceivedPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOtherLoansReceivedYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOtherPoliticalCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOtherPoliticalCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOtherReceiptsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialOtherReceiptsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialPdfUrl :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialPoliticalPartyCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialPoliticalPartyCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialPreviousFileNumber :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialReceiptDate :: Maybe Date
      <*> arbitrary -- committeeReportsPresidentialRefundedIndividualContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialRefundedIndividualContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialRefundedOtherPoliticalCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialRefundedOtherPoliticalCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialRefundedPoliticalPartyCommitteeContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialRefundedPoliticalPartyCommitteeContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialRepaymentsLoansMadeByCandidatePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialRepaymentsLoansMadeCandidateYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialRepaymentsOtherLoansPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialRepaymentsOtherLoansYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialReportForm :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialReportType :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialReportTypeFull :: Maybe Text
      <*> arbitrary -- committeeReportsPresidentialReportYear :: Maybe Int
      <*> arbitrary -- committeeReportsPresidentialSubtotalSummaryPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalContributionRefundsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalContributionRefundsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalDisbursementsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalDisbursementsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalIndividualContributionsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalIndividualContributionsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalLoanRepaymentsMadePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalLoanRepaymentsMadeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalLoansReceivedPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalLoansReceivedYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalOffsetsToOperatingExpendituresPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalOffsetsToOperatingExpendituresYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalReceiptsPeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalReceiptsYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTotalYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTransfersFromAffiliatedCommitteePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTransfersFromAffiliatedCommitteeYtd :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTransfersToOtherAuthorizedCommitteePeriod :: Maybe Double
      <*> arbitrary -- committeeReportsPresidentialTransfersToOtherAuthorizedCommitteeYtd :: Maybe Double
    
instance Arbitrary CommitteeReportsPresidentialPage where
  arbitrary =
    CommitteeReportsPresidentialPage
      <$> arbitrary -- committeeReportsPresidentialPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeeReportsPresidentialPageResults :: Maybe [CommitteeReportsPresidential]
    
instance Arbitrary CommitteeSearch where
  arbitrary =
    CommitteeSearch
      <$> arbitrary -- committeeSearchId :: Maybe Text
      <*> arbitrary -- committeeSearchName :: Maybe Text
    
instance Arbitrary CommitteeSearchList where
  arbitrary =
    CommitteeSearchList
      <$> arbitrary -- committeeSearchListResults :: Maybe [CommitteeSearch]
    
instance Arbitrary CommitteeTotals where
  arbitrary =
    CommitteeTotals
      <$> arbitrary -- committeeTotalsAllLoansReceived :: Maybe Double
      <*> arbitrary -- committeeTotalsAllOtherLoans :: Maybe Double
      <*> arbitrary -- committeeTotalsAllocatedFederalElectionLevinShare :: Maybe Double
      <*> arbitrary -- committeeTotalsCandidateContribution :: Maybe Double
      <*> arbitrary -- committeeTotalsCashOnHandBeginningPeriod :: Maybe Double
      <*> arbitrary -- committeeTotalsCommitteeDesignation :: Maybe Text
      <*> arbitrary -- committeeTotalsCommitteeDesignationFull :: Maybe Text
      <*> arbitrary -- committeeTotalsCommitteeId :: Maybe Text
      <*> arbitrary -- committeeTotalsCommitteeName :: Maybe Text
      <*> arbitrary -- committeeTotalsCommitteeType :: Maybe Text
      <*> arbitrary -- committeeTotalsCommitteeTypeFull :: Maybe Text
      <*> arbitrary -- committeeTotalsContributionRefunds :: Maybe Double
      <*> arbitrary -- committeeTotalsContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsConventionExp :: Maybe Double
      <*> arbitrary -- committeeTotalsCoordinatedExpendituresByPartyCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- committeeTotalsCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- committeeTotalsCycle :: Int
      <*> arbitrary -- committeeTotalsDisbursements :: Maybe Double
      <*> arbitrary -- committeeTotalsExemptLegalAccountingDisbursement :: Maybe Double
      <*> arbitrary -- committeeTotalsExpPriorYearsSubjectLimits :: Maybe Double
      <*> arbitrary -- committeeTotalsExpSubjectLimits :: Maybe Double
      <*> arbitrary -- committeeTotalsFedCandidateCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsFedCandidateContributionRefunds :: Maybe Double
      <*> arbitrary -- committeeTotalsFedDisbursements :: Maybe Double
      <*> arbitrary -- committeeTotalsFedElectionActivity :: Maybe Double
      <*> arbitrary -- committeeTotalsFedOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsFedReceipts :: Maybe Double
      <*> arbitrary -- committeeTotalsFederalFunds :: Maybe Double
      <*> arbitrary -- committeeTotalsFundraisingDisbursements :: Maybe Double
      <*> arbitrary -- committeeTotalsIndependentExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsIndividualContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsIndividualItemizedContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsIndividualUnitemizedContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsItemizedConventionExp :: Maybe Double
      <*> arbitrary -- committeeTotalsItemizedOtherDisb :: Maybe Double
      <*> arbitrary -- committeeTotalsItemizedOtherIncome :: Maybe Double
      <*> arbitrary -- committeeTotalsItemizedOtherRefunds :: Maybe Double
      <*> arbitrary -- committeeTotalsItemizedRefundsRelatingConventionExp :: Maybe Double
      <*> arbitrary -- committeeTotalsLastBeginningImageNumber :: Maybe Text
      <*> arbitrary -- committeeTotalsLastCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- committeeTotalsLastDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsLastDebtsOwedToCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsLastReportTypeFull :: Maybe Text
      <*> arbitrary -- committeeTotalsLastReportYear :: Maybe Int
      <*> arbitrary -- committeeTotalsLoanRepayments :: Maybe Double
      <*> arbitrary -- committeeTotalsLoanRepaymentsCandidateLoans :: Maybe Double
      <*> arbitrary -- committeeTotalsLoanRepaymentsMade :: Maybe Double
      <*> arbitrary -- committeeTotalsLoanRepaymentsOtherLoans :: Maybe Double
      <*> arbitrary -- committeeTotalsLoanRepaymentsReceived :: Maybe Double
      <*> arbitrary -- committeeTotalsLoans :: Maybe Double
      <*> arbitrary -- committeeTotalsLoansAndLoanRepaymentsMade :: Maybe Double
      <*> arbitrary -- committeeTotalsLoansAndLoanRepaymentsReceived :: Maybe Double
      <*> arbitrary -- committeeTotalsLoansMade :: Maybe Double
      <*> arbitrary -- committeeTotalsLoansMadeByCandidate :: Maybe Double
      <*> arbitrary -- committeeTotalsLoansReceived :: Maybe Double
      <*> arbitrary -- committeeTotalsLoansReceivedFromCandidate :: Maybe Double
      <*> arbitrary -- committeeTotalsNetContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsNetOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsNonAllocatedFedElectionActivity :: Maybe Double
      <*> arbitrary -- committeeTotalsOffsetsToFundraisingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsOffsetsToLegalAccounting :: Maybe Double
      <*> arbitrary -- committeeTotalsOffsetsToOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsOtherDisbursements :: Maybe Double
      <*> arbitrary -- committeeTotalsOtherFedOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsOtherFedReceipts :: Maybe Double
      <*> arbitrary -- committeeTotalsOtherLoansReceived :: Maybe Double
      <*> arbitrary -- committeeTotalsOtherPoliticalCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsOtherReceipts :: Maybe Double
      <*> arbitrary -- committeeTotalsOtherRefunds :: Maybe Double
      <*> arbitrary -- committeeTotalsPartyFull :: Maybe Text
      <*> arbitrary -- committeeTotalsPdfUrl :: Maybe Text
      <*> arbitrary -- committeeTotalsPoliticalPartyCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsReceipts :: Maybe Double
      <*> arbitrary -- committeeTotalsRefundedIndividualContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsRefundedOtherPoliticalCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsRefundedPoliticalPartyCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsRefundsRelatingConventionExp :: Maybe Double
      <*> arbitrary -- committeeTotalsRepaymentsLoansMadeByCandidate :: Maybe Double
      <*> arbitrary -- committeeTotalsRepaymentsOtherLoans :: Maybe Double
      <*> arbitrary -- committeeTotalsReportForm :: Maybe Text
      <*> arbitrary -- committeeTotalsSharedFedActivity :: Maybe Double
      <*> arbitrary -- committeeTotalsSharedFedActivityNonfed :: Maybe Double
      <*> arbitrary -- committeeTotalsSharedFedOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsSharedNonfedOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsTotalExpSubjectLimits :: Maybe Double
      <*> arbitrary -- committeeTotalsTotalIndependentContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsTotalIndependentExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsTotalOffsetsToOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsTotalTransfers :: Maybe Double
      <*> arbitrary -- committeeTotalsTransactionCoverageDate :: Maybe Date
      <*> arbitrary -- committeeTotalsTransfersFromAffiliatedCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsTransfersFromAffiliatedParty :: Maybe Double
      <*> arbitrary -- committeeTotalsTransfersFromNonfedAccount :: Maybe Double
      <*> arbitrary -- committeeTotalsTransfersFromNonfedLevin :: Maybe Double
      <*> arbitrary -- committeeTotalsTransfersFromOtherAuthorizedCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsTransfersToAffiliatedCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsTransfersToOtherAuthorizedCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsUnitemizedConventionExp :: Maybe Double
      <*> arbitrary -- committeeTotalsUnitemizedOtherDisb :: Maybe Double
      <*> arbitrary -- committeeTotalsUnitemizedOtherIncome :: Maybe Double
      <*> arbitrary -- committeeTotalsUnitemizedOtherRefunds :: Maybe Double
      <*> arbitrary -- committeeTotalsUnitemizedRefundsRelatingConventionExp :: Maybe Double
    
instance Arbitrary CommitteeTotalsHouseSenate where
  arbitrary =
    CommitteeTotalsHouseSenate
      <$> arbitrary -- committeeTotalsHouseSenateAllOtherLoans :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateCandidateContribution :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateCashOnHandBeginningPeriod :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateCommitteeDesignation :: Maybe Text
      <*> arbitrary -- committeeTotalsHouseSenateCommitteeDesignationFull :: Maybe Text
      <*> arbitrary -- committeeTotalsHouseSenateCommitteeId :: Maybe Text
      <*> arbitrary -- committeeTotalsHouseSenateCommitteeName :: Maybe Text
      <*> arbitrary -- committeeTotalsHouseSenateCommitteeType :: Maybe Text
      <*> arbitrary -- committeeTotalsHouseSenateCommitteeTypeFull :: Maybe Text
      <*> arbitrary -- committeeTotalsHouseSenateContributionRefunds :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- committeeTotalsHouseSenateCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- committeeTotalsHouseSenateCycle :: Int
      <*> arbitrary -- committeeTotalsHouseSenateDisbursements :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateIndividualContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateIndividualItemizedContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateIndividualUnitemizedContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateLastBeginningImageNumber :: Maybe Text
      <*> arbitrary -- committeeTotalsHouseSenateLastCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateLastDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateLastDebtsOwedToCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateLastReportTypeFull :: Maybe Text
      <*> arbitrary -- committeeTotalsHouseSenateLastReportYear :: Maybe Int
      <*> arbitrary -- committeeTotalsHouseSenateLoanRepayments :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateLoanRepaymentsCandidateLoans :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateLoanRepaymentsOtherLoans :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateLoans :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateLoansMadeByCandidate :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateNetContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateNetOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateOffsetsToOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateOtherDisbursements :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateOtherPoliticalCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateOtherReceipts :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenatePartyFull :: Maybe Text
      <*> arbitrary -- committeeTotalsHouseSenatePdfUrl :: Maybe Text
      <*> arbitrary -- committeeTotalsHouseSenatePoliticalPartyCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateReceipts :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateRefundedIndividualContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateRefundedOtherPoliticalCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateRefundedPoliticalPartyCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateReportForm :: Maybe Text
      <*> arbitrary -- committeeTotalsHouseSenateTransactionCoverageDate :: Maybe Date
      <*> arbitrary -- committeeTotalsHouseSenateTransfersFromOtherAuthorizedCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsHouseSenateTransfersToOtherAuthorizedCommittee :: Maybe Double
    
instance Arbitrary CommitteeTotalsHouseSenatePage where
  arbitrary =
    CommitteeTotalsHouseSenatePage
      <$> arbitrary -- committeeTotalsHouseSenatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeeTotalsHouseSenatePageResults :: Maybe [CommitteeTotalsHouseSenate]
    
instance Arbitrary CommitteeTotalsIEOnly where
  arbitrary =
    CommitteeTotalsIEOnly
      <$> arbitrary -- committeeTotalsIEOnlyCommitteeId :: Maybe Text
      <*> arbitrary -- committeeTotalsIEOnlyCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- committeeTotalsIEOnlyCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- committeeTotalsIEOnlyCycle :: Maybe Int
      <*> arbitrary -- committeeTotalsIEOnlyLastBeginningImageNumber :: Maybe Text
      <*> arbitrary -- committeeTotalsIEOnlyLastCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- committeeTotalsIEOnlyPdfUrl :: Maybe Text
      <*> arbitrary -- committeeTotalsIEOnlyReportForm :: Maybe Text
      <*> arbitrary -- committeeTotalsIEOnlyTotalIndependentContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsIEOnlyTotalIndependentExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsIEOnlyTransactionCoverageDate :: Maybe Date
    
instance Arbitrary CommitteeTotalsIEOnlyPage where
  arbitrary =
    CommitteeTotalsIEOnlyPage
      <$> arbitrary -- committeeTotalsIEOnlyPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeeTotalsIEOnlyPageResults :: Maybe [CommitteeTotalsIEOnly]
    
instance Arbitrary CommitteeTotalsPacParty where
  arbitrary =
    CommitteeTotalsPacParty
      <$> arbitrary -- committeeTotalsPacPartyAllLoansReceived :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyAllocatedFederalElectionLevinShare :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyCashOnHandBeginningPeriod :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyCommitteeDesignation :: Maybe Text
      <*> arbitrary -- committeeTotalsPacPartyCommitteeDesignationFull :: Maybe Text
      <*> arbitrary -- committeeTotalsPacPartyCommitteeId :: Maybe Text
      <*> arbitrary -- committeeTotalsPacPartyCommitteeName :: Maybe Text
      <*> arbitrary -- committeeTotalsPacPartyCommitteeType :: Maybe Text
      <*> arbitrary -- committeeTotalsPacPartyCommitteeTypeFull :: Maybe Text
      <*> arbitrary -- committeeTotalsPacPartyContributionRefunds :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyConventionExp :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyCoordinatedExpendituresByPartyCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- committeeTotalsPacPartyCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- committeeTotalsPacPartyCycle :: Int
      <*> arbitrary -- committeeTotalsPacPartyDisbursements :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyExpPriorYearsSubjectLimits :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyExpSubjectLimits :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyFedCandidateCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyFedCandidateContributionRefunds :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyFedDisbursements :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyFedElectionActivity :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyFedOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyFedReceipts :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyFederalFunds :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyIndependentExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyIndividualContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyIndividualItemizedContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyIndividualUnitemizedContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyItemizedConventionExp :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyItemizedOtherDisb :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyItemizedOtherIncome :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyItemizedOtherRefunds :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyItemizedRefundsRelatingConventionExp :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyLastBeginningImageNumber :: Maybe Text
      <*> arbitrary -- committeeTotalsPacPartyLastCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyLastDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyLastDebtsOwedToCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyLastReportTypeFull :: Maybe Text
      <*> arbitrary -- committeeTotalsPacPartyLastReportYear :: Maybe Int
      <*> arbitrary -- committeeTotalsPacPartyLoanRepaymentsMade :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyLoanRepaymentsReceived :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyLoansAndLoanRepaymentsMade :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyLoansAndLoanRepaymentsReceived :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyLoansMade :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyNetContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyNetOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyNonAllocatedFedElectionActivity :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyOffsetsToOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyOtherDisbursements :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyOtherFedOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyOtherFedReceipts :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyOtherPoliticalCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyOtherRefunds :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyPartyFull :: Maybe Text
      <*> arbitrary -- committeeTotalsPacPartyPdfUrl :: Maybe Text
      <*> arbitrary -- committeeTotalsPacPartyPoliticalPartyCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyReceipts :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyRefundedIndividualContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyRefundedOtherPoliticalCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyRefundedPoliticalPartyCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyRefundsRelatingConventionExp :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyReportForm :: Maybe Text
      <*> arbitrary -- committeeTotalsPacPartySharedFedActivity :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartySharedFedActivityNonfed :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartySharedFedOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartySharedNonfedOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyTotalExpSubjectLimits :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyTotalTransfers :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyTransactionCoverageDate :: Maybe Date
      <*> arbitrary -- committeeTotalsPacPartyTransfersFromAffiliatedParty :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyTransfersFromNonfedAccount :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyTransfersFromNonfedLevin :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyTransfersToAffiliatedCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyUnitemizedConventionExp :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyUnitemizedOtherDisb :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyUnitemizedOtherIncome :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyUnitemizedOtherRefunds :: Maybe Double
      <*> arbitrary -- committeeTotalsPacPartyUnitemizedRefundsRelatingConventionExp :: Maybe Double
    
instance Arbitrary CommitteeTotalsPacPartyPage where
  arbitrary =
    CommitteeTotalsPacPartyPage
      <$> arbitrary -- committeeTotalsPacPartyPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeeTotalsPacPartyPageResults :: Maybe [CommitteeTotalsPacParty]
    
instance Arbitrary CommitteeTotalsPage where
  arbitrary =
    CommitteeTotalsPage
      <$> arbitrary -- committeeTotalsPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeeTotalsPageResults :: Maybe [CommitteeTotals]
    
instance Arbitrary CommitteeTotalsPresidential where
  arbitrary =
    CommitteeTotalsPresidential
      <$> arbitrary -- committeeTotalsPresidentialCandidateContribution :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialCashOnHandBeginningPeriod :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialCommitteeDesignation :: Maybe Text
      <*> arbitrary -- committeeTotalsPresidentialCommitteeDesignationFull :: Maybe Text
      <*> arbitrary -- committeeTotalsPresidentialCommitteeId :: Maybe Text
      <*> arbitrary -- committeeTotalsPresidentialCommitteeName :: Maybe Text
      <*> arbitrary -- committeeTotalsPresidentialCommitteeType :: Maybe Text
      <*> arbitrary -- committeeTotalsPresidentialCommitteeTypeFull :: Maybe Text
      <*> arbitrary -- committeeTotalsPresidentialContributionRefunds :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- committeeTotalsPresidentialCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- committeeTotalsPresidentialCycle :: Int
      <*> arbitrary -- committeeTotalsPresidentialDisbursements :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialExemptLegalAccountingDisbursement :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialFederalFunds :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialFundraisingDisbursements :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialIndividualContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialIndividualItemizedContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialIndividualUnitemizedContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialLastBeginningImageNumber :: Maybe Text
      <*> arbitrary -- committeeTotalsPresidentialLastCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialLastDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialLastDebtsOwedToCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialLastReportTypeFull :: Maybe Text
      <*> arbitrary -- committeeTotalsPresidentialLastReportYear :: Maybe Int
      <*> arbitrary -- committeeTotalsPresidentialLoanRepaymentsMade :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialLoansReceived :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialLoansReceivedFromCandidate :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialNetContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialNetOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialOffsetsToFundraisingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialOffsetsToLegalAccounting :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialOffsetsToOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialOtherDisbursements :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialOtherLoansReceived :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialOtherPoliticalCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialOtherReceipts :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialPartyFull :: Maybe Text
      <*> arbitrary -- committeeTotalsPresidentialPdfUrl :: Maybe Text
      <*> arbitrary -- committeeTotalsPresidentialPoliticalPartyCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialReceipts :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialRefundedIndividualContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialRefundedOtherPoliticalCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialRefundedPoliticalPartyCommitteeContributions :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialRepaymentsLoansMadeByCandidate :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialRepaymentsOtherLoans :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialReportForm :: Maybe Text
      <*> arbitrary -- committeeTotalsPresidentialTotalOffsetsToOperatingExpenditures :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialTransactionCoverageDate :: Maybe Date
      <*> arbitrary -- committeeTotalsPresidentialTransfersFromAffiliatedCommittee :: Maybe Double
      <*> arbitrary -- committeeTotalsPresidentialTransfersToOtherAuthorizedCommittee :: Maybe Double
    
instance Arbitrary CommitteeTotalsPresidentialPage where
  arbitrary =
    CommitteeTotalsPresidentialPage
      <$> arbitrary -- committeeTotalsPresidentialPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- committeeTotalsPresidentialPageResults :: Maybe [CommitteeTotalsPresidential]
    
instance Arbitrary CommunicationCost where
  arbitrary =
    CommunicationCost
      <$> arbitrary -- communicationCostActionCode :: Maybe Text
      <*> arbitrary -- communicationCostActionCodeFull :: Maybe Text
      <*> arbitrary -- communicationCostCandidateFirstName :: Maybe Text
      <*> arbitrary -- communicationCostCandidateId :: Maybe Text
      <*> arbitrary -- communicationCostCandidateLastName :: Maybe Text
      <*> arbitrary -- communicationCostCandidateMiddleName :: Maybe Text
      <*> arbitrary -- communicationCostCandidateName :: Maybe Text
      <*> arbitrary -- communicationCostCandidateOffice :: Maybe Text
      <*> arbitrary -- communicationCostCandidateOfficeDistrict :: Maybe Text
      <*> arbitrary -- communicationCostCandidateOfficeFull :: Maybe Text
      <*> arbitrary -- communicationCostCandidateOfficeState :: Maybe Text
      <*> arbitrary -- communicationCostCommitteeId :: Maybe Text
      <*> arbitrary -- communicationCostCommitteeName :: Maybe Text
      <*> arbitrary -- communicationCostCommunicationClass :: Maybe Text
      <*> arbitrary -- communicationCostCommunicationType :: Maybe Text
      <*> arbitrary -- communicationCostCommunicationTypeFull :: Maybe Text
      <*> arbitrary -- communicationCostCycle :: Maybe Int
      <*> arbitrary -- communicationCostFileNumber :: Maybe Int
      <*> arbitrary -- communicationCostFormTypeCode :: Maybe Text
      <*> arbitrary -- communicationCostImageNumber :: Maybe Text
      <*> arbitrary -- communicationCostOriginalSubId :: Maybe Int
      <*> arbitrary -- communicationCostPdfUrl :: Maybe Text
      <*> arbitrary -- communicationCostPrimaryGeneralIndicator :: Maybe Text
      <*> arbitrary -- communicationCostPrimaryGeneralIndicatorDescription :: Maybe Text
      <*> arbitrary -- communicationCostPurpose :: Maybe Text
      <*> arbitrary -- communicationCostReportType :: Maybe Text
      <*> arbitrary -- communicationCostReportYear :: Maybe Int
      <*> arbitrary -- communicationCostScheduleType :: Maybe Text
      <*> arbitrary -- communicationCostScheduleTypeFull :: Maybe Text
      <*> arbitrary -- communicationCostStateFull :: Maybe Text
      <*> arbitrary -- communicationCostSubId :: Maybe Int
      <*> arbitrary -- communicationCostSupportOpposeIndicator :: Maybe Text
      <*> arbitrary -- communicationCostTranId :: Maybe Text
      <*> arbitrary -- communicationCostTransactionAmount :: Maybe Double
      <*> arbitrary -- communicationCostTransactionDate :: Maybe Date
      <*> arbitrary -- communicationCostTransactionType :: Maybe Text
    
instance Arbitrary CommunicationCostByCandidate where
  arbitrary =
    CommunicationCostByCandidate
      <$> arbitrary -- communicationCostByCandidateCandidateId :: Maybe Text
      <*> arbitrary -- communicationCostByCandidateCandidateName :: Maybe Text
      <*> arbitrary -- communicationCostByCandidateCommitteeId :: Maybe Text
      <*> arbitrary -- communicationCostByCandidateCommitteeName :: Maybe Text
      <*> arbitrary -- communicationCostByCandidateCount :: Maybe Int
      <*> arbitrary -- communicationCostByCandidateCycle :: Int
      <*> arbitrary -- communicationCostByCandidateSupportOpposeIndicator :: Text
      <*> arbitrary -- communicationCostByCandidateTotal :: Maybe Double
    
instance Arbitrary CommunicationCostByCandidatePage where
  arbitrary =
    CommunicationCostByCandidatePage
      <$> arbitrary -- communicationCostByCandidatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- communicationCostByCandidatePageResults :: Maybe [CommunicationCostByCandidate]
    
instance Arbitrary CommunicationCostPage where
  arbitrary =
    CommunicationCostPage
      <$> arbitrary -- communicationCostPagePagination :: Maybe SeekInfo
      <*> arbitrary -- communicationCostPageResults :: Maybe [CommunicationCost]
    
instance Arbitrary EFilings where
  arbitrary =
    EFilings
      <$> arbitrary -- eFilingsAmendedBy :: Maybe Int
      <*> arbitrary -- eFilingsAmendmentChain :: Maybe [Int]
      <*> arbitrary -- eFilingsAmendmentNumber :: Maybe Int
      <*> arbitrary -- eFilingsAmendsFile :: Maybe Int
      <*> arbitrary -- eFilingsBeginningImageNumber :: Maybe Text
      <*> arbitrary -- eFilingsCommitteeId :: Maybe Text
      <*> arbitrary -- eFilingsCommitteeName :: Maybe Text
      <*> arbitrary -- eFilingsCoverageEndDate :: Maybe Date
      <*> arbitrary -- eFilingsCoverageStartDate :: Maybe Date
      <*> arbitrary -- eFilingsCsvUrl :: Maybe Text
      <*> arbitrary -- eFilingsDocumentDescription :: Maybe Text
      <*> arbitrary -- eFilingsEndingImageNumber :: Maybe Text
      <*> arbitrary -- eFilingsFecFileId :: Maybe Text
      <*> arbitrary -- eFilingsFecUrl :: Maybe Text
      <*> arbitrary -- eFilingsFileNumber :: Maybe Int
      <*> arbitrary -- eFilingsFormType :: Maybe Text
      <*> arbitrary -- eFilingsHtmlUrl :: Maybe Text
      <*> arbitrary -- eFilingsIsAmended :: Maybe Bool
      <*> arbitrary -- eFilingsLoadTimestamp :: Maybe DateTime
      <*> arbitrary -- eFilingsMostRecent :: Maybe Bool
      <*> arbitrary -- eFilingsMostRecentFiling :: Maybe Int
      <*> arbitrary -- eFilingsPdfUrl :: Maybe Text
      <*> arbitrary -- eFilingsReceiptDate :: Maybe Date
    
instance Arbitrary EFilingsPage where
  arbitrary =
    EFilingsPage
      <$> arbitrary -- eFilingsPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- eFilingsPageResults :: Maybe [EFilings]
    
instance Arbitrary EfilingsAmendments where
  arbitrary =
    EfilingsAmendments
      <$> arbitrary -- efilingsAmendmentsAmendmentChain :: Maybe [Double]
      <*> arbitrary -- efilingsAmendmentsDepth :: Maybe Double
      <*> arbitrary -- efilingsAmendmentsFileNumber :: Maybe Int
      <*> arbitrary -- efilingsAmendmentsLast :: Maybe Double
      <*> arbitrary -- efilingsAmendmentsLongestChain :: Maybe [Double]
      <*> arbitrary -- efilingsAmendmentsMostRecentFiling :: Maybe Double
      <*> arbitrary -- efilingsAmendmentsPreviousFileNumber :: Maybe Double
    
instance Arbitrary EfilingsAmendmentsPage where
  arbitrary =
    EfilingsAmendmentsPage
      <$> arbitrary -- efilingsAmendmentsPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- efilingsAmendmentsPageResults :: Maybe [EfilingsAmendments]
    
instance Arbitrary Election where
  arbitrary =
    Election
      <$> arbitrary -- electionCandidateElectionYear :: Maybe Int
      <*> arbitrary -- electionCandidateId :: Maybe Text
      <*> arbitrary -- electionCandidateName :: Maybe Text
      <*> arbitrary -- electionCandidatePccId :: Maybe Text
      <*> arbitrary -- electionCandidatePccName :: Maybe Text
      <*> arbitrary -- electionCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- electionCommitteeIds :: Maybe [Text]
      <*> arbitrary -- electionCoverageEndDate :: Maybe Date
      <*> arbitrary -- electionIncumbentChallengeFull :: Maybe Text
      <*> arbitrary -- electionPartyFull :: Maybe Text
      <*> arbitrary -- electionTotalDisbursements :: Maybe Double
      <*> arbitrary -- electionTotalReceipts :: Maybe Double
    
instance Arbitrary ElectionDate where
  arbitrary =
    ElectionDate
      <$> arbitrary -- electionDateActiveElection :: Maybe Bool
      <*> arbitrary -- electionDateCreateDate :: Maybe DateTime
      <*> arbitrary -- electionDateElectionDate :: Maybe Date
      <*> arbitrary -- electionDateElectionDistrict :: Maybe Int
      <*> arbitrary -- electionDateElectionNotes :: Maybe Text
      <*> arbitrary -- electionDateElectionParty :: Maybe Text
      <*> arbitrary -- electionDateElectionState :: Maybe Text
      <*> arbitrary -- electionDateElectionTypeFull :: Maybe Text
      <*> arbitrary -- electionDateElectionTypeId :: Maybe Text
      <*> arbitrary -- electionDateElectionYear :: Maybe Int
      <*> arbitrary -- electionDateOfficeSought :: Maybe Text
      <*> arbitrary -- electionDatePrimaryGeneralDate :: Maybe Date
      <*> arbitrary -- electionDateUpdateDate :: Maybe DateTime
    
instance Arbitrary ElectionDatePage where
  arbitrary =
    ElectionDatePage
      <$> arbitrary -- electionDatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- electionDatePageResults :: Maybe [ElectionDate]
    
instance Arbitrary ElectionPage where
  arbitrary =
    ElectionPage
      <$> arbitrary -- electionPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- electionPageResults :: Maybe [Election]
    
instance Arbitrary ElectionSearch where
  arbitrary =
    ElectionSearch
      <$> arbitrary -- electionSearchCandidateStatus :: Maybe Text
      <*> arbitrary -- electionSearchCycle :: Maybe Int
      <*> arbitrary -- electionSearchDistrict :: Maybe Text
      <*> arbitrary -- electionSearchIncumbentId :: Maybe Text
      <*> arbitrary -- electionSearchIncumbentName :: Maybe Text
      <*> arbitrary -- electionSearchOffice :: Maybe Text
      <*> arbitrary -- electionSearchState :: Maybe Text
    
instance Arbitrary ElectionSearchPage where
  arbitrary =
    ElectionSearchPage
      <$> arbitrary -- electionSearchPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- electionSearchPageResults :: Maybe [ElectionSearch]
    
instance Arbitrary ElectionSummary where
  arbitrary =
    ElectionSummary
      <$> arbitrary -- electionSummaryCount :: Maybe Int
      <*> arbitrary -- electionSummaryDisbursements :: Maybe Double
      <*> arbitrary -- electionSummaryIndependentExpenditures :: Maybe Double
      <*> arbitrary -- electionSummaryReceipts :: Maybe Double
    
instance Arbitrary Electioneering where
  arbitrary =
    Electioneering
      <$> arbitrary -- electioneeringAmendmentIndicator :: Maybe Text
      <*> arbitrary -- electioneeringBeginningImageNumber :: Maybe Text
      <*> arbitrary -- electioneeringCalculatedCandidateShare :: Maybe Double
      <*> arbitrary -- electioneeringCandidateDistrict :: Maybe Text
      <*> arbitrary -- electioneeringCandidateId :: Maybe Text
      <*> arbitrary -- electioneeringCandidateName :: Maybe Text
      <*> arbitrary -- electioneeringCandidateOffice :: Maybe Text
      <*> arbitrary -- electioneeringCandidateState :: Maybe Text
      <*> arbitrary -- electioneeringCommitteeId :: Maybe Text
      <*> arbitrary -- electioneeringCommitteeName :: Maybe Text
      <*> arbitrary -- electioneeringCommunicationDate :: Maybe Date
      <*> arbitrary -- electioneeringDisbursementAmount :: Maybe Double
      <*> arbitrary -- electioneeringDisbursementDate :: Maybe Date
      <*> arbitrary -- electioneeringElectionType :: Maybe Text
      <*> arbitrary -- electioneeringFileNumber :: Maybe Int
      <*> arbitrary -- electioneeringLinkId :: Maybe Int
      <*> arbitrary -- electioneeringNumberOfCandidates :: Maybe Double
      <*> arbitrary -- electioneeringPdfUrl :: Maybe Text
      <*> arbitrary -- electioneeringPublicDistributionDate :: Maybe Date
      <*> arbitrary -- electioneeringPurposeDescription :: Maybe Text
      <*> arbitrary -- electioneeringReceiptDate :: Maybe Date
      <*> arbitrary -- electioneeringReportYear :: Maybe Int
      <*> arbitrary -- electioneeringSbImageNum :: Maybe Text
      <*> arbitrary -- electioneeringSbLinkId :: Maybe Text
      <*> arbitrary -- electioneeringSubId :: Maybe Int
    
instance Arbitrary ElectioneeringByCandidate where
  arbitrary =
    ElectioneeringByCandidate
      <$> arbitrary -- electioneeringByCandidateCandidateId :: Maybe Text
      <*> arbitrary -- electioneeringByCandidateCandidateName :: Maybe Text
      <*> arbitrary -- electioneeringByCandidateCommitteeId :: Maybe Text
      <*> arbitrary -- electioneeringByCandidateCommitteeName :: Maybe Text
      <*> arbitrary -- electioneeringByCandidateCount :: Maybe Int
      <*> arbitrary -- electioneeringByCandidateCycle :: Int
      <*> arbitrary -- electioneeringByCandidateTotal :: Maybe Double
    
instance Arbitrary ElectioneeringByCandidatePage where
  arbitrary =
    ElectioneeringByCandidatePage
      <$> arbitrary -- electioneeringByCandidatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- electioneeringByCandidatePageResults :: Maybe [ElectioneeringByCandidate]
    
instance Arbitrary ElectioneeringPage where
  arbitrary =
    ElectioneeringPage
      <$> arbitrary -- electioneeringPagePagination :: Maybe SeekInfo
      <*> arbitrary -- electioneeringPageResults :: Maybe [Electioneering]
    
instance Arbitrary ElectionsList where
  arbitrary =
    ElectionsList
      <$> arbitrary -- electionsListCycle :: Maybe Int
      <*> arbitrary -- electionsListDistrict :: Maybe Text
      <*> arbitrary -- electionsListOffice :: Maybe Text
      <*> arbitrary -- electionsListState :: Maybe Text
    
instance Arbitrary ElectionsListPage where
  arbitrary =
    ElectionsListPage
      <$> arbitrary -- electionsListPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- electionsListPageResults :: Maybe [ElectionsList]
    
instance Arbitrary EntityReceiptDisbursementTotals where
  arbitrary =
    EntityReceiptDisbursementTotals
      <$> arbitrary -- entityReceiptDisbursementTotalsCumulativeCandidateDisbursements :: Maybe Float
      <*> arbitrary -- entityReceiptDisbursementTotalsCumulativeCandidateReceipts :: Maybe Float
      <*> arbitrary -- entityReceiptDisbursementTotalsCumulativePacDisbursements :: Maybe Float
      <*> arbitrary -- entityReceiptDisbursementTotalsCumulativePacReceipts :: Maybe Float
      <*> arbitrary -- entityReceiptDisbursementTotalsCumulativePartyDisbursements :: Maybe Float
      <*> arbitrary -- entityReceiptDisbursementTotalsCumulativePartyReceipts :: Maybe Float
      <*> arbitrary -- entityReceiptDisbursementTotalsCycle :: Maybe Int
      <*> arbitrary -- entityReceiptDisbursementTotalsEndDate :: Maybe Date
    
instance Arbitrary EntityReceiptDisbursementTotalsPage where
  arbitrary =
    EntityReceiptDisbursementTotalsPage
      <$> arbitrary -- entityReceiptDisbursementTotalsPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- entityReceiptDisbursementTotalsPageResults :: Maybe [EntityReceiptDisbursementTotals]
    
instance Arbitrary Filings where
  arbitrary =
    Filings
      <$> arbitrary -- filingsAmendmentChain :: Maybe [Double]
      <*> arbitrary -- filingsAmendmentIndicator :: Maybe Text
      <*> arbitrary -- filingsAmendmentVersion :: Maybe Int
      <*> arbitrary -- filingsBeginningImageNumber :: Maybe Text
      <*> arbitrary -- filingsCandidateId :: Maybe Text
      <*> arbitrary -- filingsCandidateName :: Maybe Text
      <*> arbitrary -- filingsCashOnHandBeginningPeriod :: Maybe Double
      <*> arbitrary -- filingsCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- filingsCmteTp :: Maybe Text
      <*> arbitrary -- filingsCommitteeId :: Maybe Text
      <*> arbitrary -- filingsCommitteeName :: Maybe Text
      <*> arbitrary -- filingsCoverageEndDate :: Maybe Date
      <*> arbitrary -- filingsCoverageStartDate :: Maybe Date
      <*> arbitrary -- filingsCsvUrl :: Maybe Text
      <*> arbitrary -- filingsCycle :: Maybe Int
      <*> arbitrary -- filingsDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- filingsDebtsOwedToCommittee :: Maybe Double
      <*> arbitrary -- filingsDocumentDescription :: Maybe Text
      <*> arbitrary -- filingsDocumentType :: Maybe Text
      <*> arbitrary -- filingsDocumentTypeFull :: Maybe Text
      <*> arbitrary -- filingsElectionYear :: Maybe Int
      <*> arbitrary -- filingsEndingImageNumber :: Maybe Text
      <*> arbitrary -- filingsFecFileId :: Maybe Text
      <*> arbitrary -- filingsFecUrl :: Maybe Text
      <*> arbitrary -- filingsFileNumber :: Maybe Int
      <*> arbitrary -- filingsFormType :: Maybe Text
      <*> arbitrary -- filingsHousePersonalFunds :: Maybe Double
      <*> arbitrary -- filingsHtmlUrl :: Maybe Text
      <*> arbitrary -- filingsIsAmended :: Maybe Bool
      <*> arbitrary -- filingsMeansFiled :: Maybe Text
      <*> arbitrary -- filingsMostRecent :: Maybe Bool
      <*> arbitrary -- filingsMostRecentFileNumber :: Maybe Int
      <*> arbitrary -- filingsNetDonations :: Maybe Double
      <*> arbitrary -- filingsOffice :: Maybe Text
      <*> arbitrary -- filingsOppositionPersonalFunds :: Maybe Double
      <*> arbitrary -- filingsPages :: Maybe Int
      <*> arbitrary -- filingsParty :: Maybe Text
      <*> arbitrary -- filingsPdfUrl :: Maybe Text
      <*> arbitrary -- filingsPreviousFileNumber :: Maybe Int
      <*> arbitrary -- filingsPrimaryGeneralIndicator :: Maybe Text
      <*> arbitrary -- filingsReceiptDate :: Maybe Date
      <*> arbitrary -- filingsReportType :: Maybe Text
      <*> arbitrary -- filingsReportTypeFull :: Maybe Text
      <*> arbitrary -- filingsReportYear :: Maybe Int
      <*> arbitrary -- filingsRequestType :: Maybe Text
      <*> arbitrary -- filingsSenatePersonalFunds :: Maybe Double
      <*> arbitrary -- filingsState :: Maybe Text
      <*> arbitrary -- filingsSubId :: Maybe Text
      <*> arbitrary -- filingsTotalCommunicationCost :: Maybe Double
      <*> arbitrary -- filingsTotalDisbursements :: Maybe Double
      <*> arbitrary -- filingsTotalIndependentExpenditures :: Maybe Double
      <*> arbitrary -- filingsTotalIndividualContributions :: Maybe Double
      <*> arbitrary -- filingsTotalReceipts :: Maybe Double
      <*> arbitrary -- filingsTreasurerName :: Maybe Text
      <*> arbitrary -- filingsUpdateDate :: Maybe Date
    
instance Arbitrary FilingsPage where
  arbitrary =
    FilingsPage
      <$> arbitrary -- filingsPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- filingsPageResults :: Maybe [Filings]
    
instance Arbitrary InlineResponseDefault where
  arbitrary =
    InlineResponseDefault
      <$> arbitrary -- inlineResponseDefaultPagination :: Maybe OffsetInfo
      <*> arbitrary -- inlineResponseDefaultResults :: Maybe [ElectionDate]
    
instance Arbitrary InlineResponseDefault1 where
  arbitrary =
    InlineResponseDefault1
      <$> arbitrary -- inlineResponseDefault1AdminFines :: Maybe [InlineResponseDefault1AdminFines]
      <*> arbitrary -- inlineResponseDefault1Adrs :: Maybe [InlineResponseDefault1Adrs]
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinions :: Maybe [InlineResponseDefault1AdvisoryOpinions]
      <*> arbitrary -- inlineResponseDefault1Murs :: Maybe [InlineResponseDefault1Murs]
      <*> arbitrary -- inlineResponseDefault1Regulations :: Maybe [InlineResponseDefault1Regulations]
      <*> arbitrary -- inlineResponseDefault1Statutes :: Maybe [InlineResponseDefault1Statutes]
      <*> arbitrary -- inlineResponseDefault1TotalAdminFines :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1TotalAdrs :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1TotalAdvisoryOpinions :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1TotalAll :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1TotalMurs :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1TotalRegulations :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1TotalStatutes :: Maybe Int
    
instance Arbitrary InlineResponseDefault1AdminFines where
  arbitrary =
    InlineResponseDefault1AdminFines
      <$> arbitrary -- inlineResponseDefault1AdminFinesChallengeOutcome :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdminFinesChallengeReceiptDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1AdminFinesCheckAmount :: Maybe Double
      <*> arbitrary -- inlineResponseDefault1AdminFinesCommissionVotes :: Maybe [InlineResponseDefault1CommissionVotes]
      <*> arbitrary -- inlineResponseDefault1AdminFinesCommitteeId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdminFinesDocId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdminFinesDocumentHighlights :: Maybe A.Value
      <*> arbitrary -- inlineResponseDefault1AdminFinesDocuments :: Maybe [InlineResponseDefault1Documents]
      <*> arbitrary -- inlineResponseDefault1AdminFinesFinalDeterminationAmount :: Maybe Double
      <*> arbitrary -- inlineResponseDefault1AdminFinesFinalDeterminationDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1AdminFinesHighlights :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1AdminFinesName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdminFinesNo :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdminFinesPetitionCourtDecisionDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1AdminFinesPetitionCourtFilingDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1AdminFinesReasonToBelieveActionDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1AdminFinesReasonToBelieveFineAmount :: Maybe Double
      <*> arbitrary -- inlineResponseDefault1AdminFinesReportType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdminFinesReportYear :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdminFinesTreasuryReferralAmount :: Maybe Double
      <*> arbitrary -- inlineResponseDefault1AdminFinesTreasuryReferralDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1AdminFinesUrl :: Maybe Text
    
instance Arbitrary InlineResponseDefault1Adrs where
  arbitrary =
    InlineResponseDefault1Adrs
      <$> arbitrary -- inlineResponseDefault1AdrsCloseDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1AdrsCommissionVotes :: Maybe [InlineResponseDefault1CommissionVotes]
      <*> arbitrary -- inlineResponseDefault1AdrsDispositions :: Maybe [InlineResponseDefault1Dispositions]
      <*> arbitrary -- inlineResponseDefault1AdrsDocId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdrsDocumentHighlights :: Maybe A.Value
      <*> arbitrary -- inlineResponseDefault1AdrsDocuments :: Maybe [InlineResponseDefault1Documents]
      <*> arbitrary -- inlineResponseDefault1AdrsElectionCycles :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1AdrsHighlights :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1AdrsName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdrsNo :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdrsOpenDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1AdrsParticipants :: Maybe [InlineResponseDefault1Participants]
      <*> arbitrary -- inlineResponseDefault1AdrsRespondents :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1AdrsSubjects :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1AdrsUrl :: Maybe Text
    
instance Arbitrary InlineResponseDefault1AdvisoryOpinions where
  arbitrary =
    InlineResponseDefault1AdvisoryOpinions
      <$> arbitrary -- inlineResponseDefault1AdvisoryOpinionsAoCitations :: Maybe [InlineResponseDefault1AoCitations]
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsAosCitedBy :: Maybe [InlineResponseDefault1AoCitations]
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsCommenterNames :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsDocumentHighlights :: Maybe A.Value
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsDocuments :: Maybe [InlineResponseDefault1Documents1]
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsEntities :: Maybe [InlineResponseDefault1Entities]
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsHighlights :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsIsPending :: Maybe Bool
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsIssueDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsNo :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsRegulatoryCitations :: Maybe [InlineResponseDefault1RegulatoryCitations]
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsRepresentativeNames :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsRequestDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsRequestorNames :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsRequestorTypes :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsStatus :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsStatutoryCitations :: Maybe [InlineResponseDefault1StatutoryCitations]
      <*> arbitrary -- inlineResponseDefault1AdvisoryOpinionsSummary :: Maybe Text
    
instance Arbitrary InlineResponseDefault1AoCitations where
  arbitrary =
    InlineResponseDefault1AoCitations
      <$> arbitrary -- inlineResponseDefault1AoCitationsName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1AoCitationsNo :: Maybe Text
    
instance Arbitrary InlineResponseDefault1Citations where
  arbitrary =
    InlineResponseDefault1Citations
      <$> arbitrary -- inlineResponseDefault1CitationsText :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1CitationsTitle :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1CitationsType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1CitationsUrl :: Maybe Text
    
instance Arbitrary InlineResponseDefault1CommissionVotes where
  arbitrary =
    InlineResponseDefault1CommissionVotes
      <$> arbitrary -- inlineResponseDefault1CommissionVotesAction :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1CommissionVotesVoteDate :: Maybe Date
    
instance Arbitrary InlineResponseDefault1Dispositions where
  arbitrary =
    InlineResponseDefault1Dispositions
      <$> arbitrary -- inlineResponseDefault1DispositionsCitations :: Maybe [InlineResponseDefault1Citations]
      <*> arbitrary -- inlineResponseDefault1DispositionsDisposition :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1DispositionsPenalty :: Maybe Double
      <*> arbitrary -- inlineResponseDefault1DispositionsRespondent :: Maybe Text
    
instance Arbitrary InlineResponseDefault1Documents where
  arbitrary =
    InlineResponseDefault1Documents
      <$> arbitrary -- inlineResponseDefault1DocumentsCategory :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1DocumentsDescription :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1DocumentsDocumentDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1DocumentsDocumentId :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1DocumentsLength :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1DocumentsUrl :: Maybe Text
    
instance Arbitrary InlineResponseDefault1Documents1 where
  arbitrary =
    InlineResponseDefault1Documents1
      <$> arbitrary -- inlineResponseDefault1Documents1Category :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1Documents1Date :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1Documents1Description :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1Documents1DocumentId :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1Documents1Url :: Maybe Text
    
instance Arbitrary InlineResponseDefault1Entities where
  arbitrary =
    InlineResponseDefault1Entities
      <$> arbitrary -- inlineResponseDefault1EntitiesName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1EntitiesRole :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1EntitiesType :: Maybe Text
    
instance Arbitrary InlineResponseDefault1Murs where
  arbitrary =
    InlineResponseDefault1Murs
      <$> arbitrary -- inlineResponseDefault1MursCloseDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1MursCommissionVotes :: Maybe [InlineResponseDefault1CommissionVotes]
      <*> arbitrary -- inlineResponseDefault1MursDispositions :: Maybe [InlineResponseDefault1Dispositions]
      <*> arbitrary -- inlineResponseDefault1MursDocId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1MursDocumentHighlights :: Maybe A.Value
      <*> arbitrary -- inlineResponseDefault1MursDocuments :: Maybe [InlineResponseDefault1Documents]
      <*> arbitrary -- inlineResponseDefault1MursElectionCycles :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1MursHighlights :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1MursMurType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1MursName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1MursNo :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1MursOpenDate :: Maybe Date
      <*> arbitrary -- inlineResponseDefault1MursParticipants :: Maybe [InlineResponseDefault1Participants]
      <*> arbitrary -- inlineResponseDefault1MursRespondents :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1MursSubjects :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1MursUrl :: Maybe Text
    
instance Arbitrary InlineResponseDefault1Participants where
  arbitrary =
    InlineResponseDefault1Participants
      <$> arbitrary -- inlineResponseDefault1ParticipantsCitations :: Maybe A.Value
      <*> arbitrary -- inlineResponseDefault1ParticipantsName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1ParticipantsRole :: Maybe Text
    
instance Arbitrary InlineResponseDefault1Regulations where
  arbitrary =
    InlineResponseDefault1Regulations
      <$> arbitrary -- inlineResponseDefault1RegulationsDocId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1RegulationsDocumentHighlights :: Maybe A.Value
      <*> arbitrary -- inlineResponseDefault1RegulationsHighlights :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1RegulationsName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1RegulationsNo :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1RegulationsUrl :: Maybe Text
    
instance Arbitrary InlineResponseDefault1RegulatoryCitations where
  arbitrary =
    InlineResponseDefault1RegulatoryCitations
      <$> arbitrary -- inlineResponseDefault1RegulatoryCitationsPart :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1RegulatoryCitationsSection :: Maybe Int
      <*> arbitrary -- inlineResponseDefault1RegulatoryCitationsTitle :: Maybe Int
    
instance Arbitrary InlineResponseDefault1Statutes where
  arbitrary =
    InlineResponseDefault1Statutes
      <$> arbitrary -- inlineResponseDefault1StatutesChapter :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1StatutesDocId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1StatutesDocumentHighlights :: Maybe A.Value
      <*> arbitrary -- inlineResponseDefault1StatutesHighlights :: Maybe [Text]
      <*> arbitrary -- inlineResponseDefault1StatutesName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1StatutesNo :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1StatutesTitle :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1StatutesUrl :: Maybe Text
    
instance Arbitrary InlineResponseDefault1StatutoryCitations where
  arbitrary =
    InlineResponseDefault1StatutoryCitations
      <$> arbitrary -- inlineResponseDefault1StatutoryCitationsSection :: Maybe Text
      <*> arbitrary -- inlineResponseDefault1StatutoryCitationsTitle :: Maybe Int
    
instance Arbitrary InlineResponseDefault2 where
  arbitrary =
    InlineResponseDefault2
      <$> arbitrary -- inlineResponseDefault2Pagination :: Maybe OffsetInfo
      <*> arbitrary -- inlineResponseDefault2Results :: Maybe [ReportDate]
    
instance Arbitrary InlineResponseDefault3 where
  arbitrary =
    InlineResponseDefault3
      <$> arbitrary -- inlineResponseDefault3Pagination :: Maybe OffsetInfo
      <*> arbitrary -- inlineResponseDefault3Results :: Maybe [InlineResponseDefault3Results]
    
instance Arbitrary InlineResponseDefault3Results where
  arbitrary =
    InlineResponseDefault3Results
      <$> arbitrary -- inlineResponseDefault3ResultsActionCode :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsActionCodeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCandidateFirstName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCandidateId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCandidateLastName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCandidateMiddleName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCandidateName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCandidateOffice :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCandidateOfficeDistrict :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCandidateOfficeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCandidateOfficeState :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCandidateOfficeStateFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCandidatePrefix :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCandidateSuffix :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCommittee :: Maybe CommitteeHistory
      <*> arbitrary -- inlineResponseDefault3ResultsCommitteeId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsCycle :: Maybe Int
      <*> arbitrary -- inlineResponseDefault3ResultsDueDateTerms :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsElectionType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsElectionTypeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsEntityType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsEntityTypeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsFecCommitteeId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsFecElectionTypeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsFecElectionTypeYear :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsFileNumber :: Maybe Int
      <*> arbitrary -- inlineResponseDefault3ResultsFilingForm :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsImageNumber :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsIncurredDate :: Maybe DateTime
      <*> arbitrary -- inlineResponseDefault3ResultsInterestRateTerms :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsLineNumber :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsLinkId :: Maybe Int
      <*> arbitrary -- inlineResponseDefault3ResultsLoadDate :: Maybe DateTime
      <*> arbitrary -- inlineResponseDefault3ResultsLoanBalance :: Maybe Float
      <*> arbitrary -- inlineResponseDefault3ResultsLoanSourceCity :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsLoanSourceFirstName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsLoanSourceLastName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsLoanSourceMiddleName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsLoanSourceName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsLoanSourcePrefix :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsLoanSourceState :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsLoanSourceStreet1 :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsLoanSourceStreet2 :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsLoanSourceSuffix :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsLoanSourceZip :: Maybe Int
      <*> arbitrary -- inlineResponseDefault3ResultsMemoCode :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsMemoText :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsOriginalLoanAmount :: Maybe Float
      <*> arbitrary -- inlineResponseDefault3ResultsOriginalSubId :: Maybe Int
      <*> arbitrary -- inlineResponseDefault3ResultsPaymentToDate :: Maybe Float
      <*> arbitrary -- inlineResponseDefault3ResultsPdfUrl :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsPersonallyFunded :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsReportType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsReportYear :: Maybe Int
      <*> arbitrary -- inlineResponseDefault3ResultsScheduleALineNumber :: Maybe Int
      <*> arbitrary -- inlineResponseDefault3ResultsScheduleType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsScheduleTypeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsSecuredInd :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsSubId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault3ResultsTransactionId :: Maybe Text
    
instance Arbitrary InlineResponseDefault4 where
  arbitrary =
    InlineResponseDefault4
      <$> arbitrary -- inlineResponseDefault4Pagination :: Maybe OffsetInfo
      <*> arbitrary -- inlineResponseDefault4Results :: Maybe [InlineResponseDefault4Results]
    
instance Arbitrary InlineResponseDefault4Results where
  arbitrary =
    InlineResponseDefault4Results
      <$> arbitrary -- inlineResponseDefault4ResultsActionCode :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsActionCodeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsAmountIncurredPeriod :: Maybe Float
      <*> arbitrary -- inlineResponseDefault4ResultsCandidateFirstName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCandidateId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCandidateLastName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCandidateOffice :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCandidateOfficeDistrict :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCandidateOfficeState :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCandidateOfficeStateFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCanidateName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCommittee :: Maybe CommitteeHistory
      <*> arbitrary -- inlineResponseDefault4ResultsCommitteeId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCommitteeName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsConduitCommitteeCity :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsConduitCommitteeId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsConduitCommitteeName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsConduitCommitteeState :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsConduitCommitteeStreet1 :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsConduitCommitteeStreet2 :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsConduitCommitteeZip :: Maybe Int
      <*> arbitrary -- inlineResponseDefault4ResultsCreditorDebtorCity :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCreditorDebtorFirstName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCreditorDebtorId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCreditorDebtorLastName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCreditorDebtorMiddleName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCreditorDebtorName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCreditorDebtorPrefix :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCreditorDebtorState :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCreditorDebtorStreet1 :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCreditorDebtorStreet2 :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsCreditorDebtorSuffix :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsElectionCycle :: Maybe Int
      <*> arbitrary -- inlineResponseDefault4ResultsEntityType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsFileNumber :: Maybe Int
      <*> arbitrary -- inlineResponseDefault4ResultsFilingForm :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsImageNumber :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsLineNumber :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsLinkId :: Maybe Int
      <*> arbitrary -- inlineResponseDefault4ResultsLoadDate :: Maybe DateTime
      <*> arbitrary -- inlineResponseDefault4ResultsNatureOfDebt :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsOriginalSubId :: Maybe Int
      <*> arbitrary -- inlineResponseDefault4ResultsOutstandingBalanceBeginningOfPeriod :: Maybe Float
      <*> arbitrary -- inlineResponseDefault4ResultsOutstandingBalanceCloseOfPeriod :: Maybe Float
      <*> arbitrary -- inlineResponseDefault4ResultsPaymentPeriod :: Maybe Float
      <*> arbitrary -- inlineResponseDefault4ResultsPdfUrl :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsReportType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsReportYear :: Maybe Int
      <*> arbitrary -- inlineResponseDefault4ResultsScheduleType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsScheduleTypeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsSubId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault4ResultsTransactionId :: Maybe Text
    
instance Arbitrary InlineResponseDefault5 where
  arbitrary =
    InlineResponseDefault5
      <$> arbitrary -- inlineResponseDefault5Pagination :: Maybe OffsetInfo
      <*> arbitrary -- inlineResponseDefault5Results :: Maybe [InlineResponseDefault5Results]
    
instance Arbitrary InlineResponseDefault5Results where
  arbitrary =
    InlineResponseDefault5Results
      <$> arbitrary -- inlineResponseDefault5ResultsActionCode :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsActionCodeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsAggregateGeneralElectionExpenditure :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsBackReferenceScheduleName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsBackReferenceTransactionId :: Maybe Int
      <*> arbitrary -- inlineResponseDefault5ResultsCandidateFirstName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCandidateId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCandidateLastName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCandidateMiddleName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCandidateName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCandidateOffice :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCandidateOfficeDistrict :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCandidateOfficeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCandidateOfficeState :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCandidateOfficeStateFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCandidatePrefix :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCandidateSuffix :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCatologCode :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCatologCodeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCommittee :: Maybe CommitteeHistory
      <*> arbitrary -- inlineResponseDefault5ResultsCommitteeDesignatedCoordinatedExpenditureIndicator :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCommitteeId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsCommitteeName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsConduitCommitteeCity :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsConduitCommitteeId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsConduitCommitteeName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsConduitCommitteeState :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsConduitCommitteeStreet1 :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsConduitCommitteeStreet2 :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsConduitCommitteeZip :: Maybe Int
      <*> arbitrary -- inlineResponseDefault5ResultsDesignatedCommitteeId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsDesignatedCommitteeName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsElectionCycle :: Maybe Int
      <*> arbitrary -- inlineResponseDefault5ResultsEntityType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsEntityTypeDesc :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsExpenditureAmount :: Maybe Int
      <*> arbitrary -- inlineResponseDefault5ResultsExpenditureDate :: Maybe DateTime
      <*> arbitrary -- inlineResponseDefault5ResultsExpenditurePurposeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsExpenditureType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsExpenditureTypeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsFileNumber :: Maybe Int
      <*> arbitrary -- inlineResponseDefault5ResultsFilingForm :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsImageNumber :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsLineNumber :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsLinkId :: Maybe Int
      <*> arbitrary -- inlineResponseDefault5ResultsLoadDate :: Maybe DateTime
      <*> arbitrary -- inlineResponseDefault5ResultsMemoCode :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsMemoCodeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsMemoText :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsOriginalSubId :: Maybe Int
      <*> arbitrary -- inlineResponseDefault5ResultsPayeeFirstName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsPayeeLastName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsPayeeMiddleName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsPayeeName :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsPdfUrl :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsReportType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsReportYear :: Maybe Int
      <*> arbitrary -- inlineResponseDefault5ResultsScheduleType :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsScheduleTypeFull :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsSubId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsSubordinateCommittee :: Maybe CommitteeHistory
      <*> arbitrary -- inlineResponseDefault5ResultsSubordinateCommitteeId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsTransactionId :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsUnlimitedSpendingFlag :: Maybe Text
      <*> arbitrary -- inlineResponseDefault5ResultsUnlimitedSpendingFlagFull :: Maybe Text
    
instance Arbitrary OffsetInfo where
  arbitrary =
    OffsetInfo
      <$> arbitrary -- offsetInfoCount :: Maybe Int
      <*> arbitrary -- offsetInfoPage :: Maybe Int
      <*> arbitrary -- offsetInfoPages :: Maybe Int
      <*> arbitrary -- offsetInfoPerPage :: Maybe Int
    
instance Arbitrary OperationsLog where
  arbitrary =
    OperationsLog
      <$> arbitrary -- operationsLogAmendmentIndicator :: Maybe Text
      <*> arbitrary -- operationsLogBeginningImageNumber :: Maybe Text
      <*> arbitrary -- operationsLogCandidateCommitteeId :: Maybe Text
      <*> arbitrary -- operationsLogCoverageEndDate :: Maybe DateTime
      <*> arbitrary -- operationsLogCoverageStartDate :: Maybe DateTime
      <*> arbitrary -- operationsLogEndingImageNumber :: Maybe Text
      <*> arbitrary -- operationsLogFormType :: Maybe Text
      <*> arbitrary -- operationsLogReceiptDate :: Maybe DateTime
      <*> arbitrary -- operationsLogReportType :: Maybe Text
      <*> arbitrary -- operationsLogReportYear :: Maybe Int
      <*> arbitrary -- operationsLogStatusNum :: Maybe Int
      <*> arbitrary -- operationsLogSubId :: Maybe Int
      <*> arbitrary -- operationsLogSummaryDataCompleteDate :: Maybe DateTime
      <*> arbitrary -- operationsLogSummaryDataVerificationDate :: Maybe DateTime
      <*> arbitrary -- operationsLogTransactionDataCompleteDate :: Maybe Date
    
instance Arbitrary OperationsLogPage where
  arbitrary =
    OperationsLogPage
      <$> arbitrary -- operationsLogPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- operationsLogPageResults :: Maybe [OperationsLog]
    
instance Arbitrary RadAnalyst where
  arbitrary =
    RadAnalyst
      <$> arbitrary -- radAnalystAnalystEmail :: Maybe Text
      <*> arbitrary -- radAnalystAnalystId :: Maybe Double
      <*> arbitrary -- radAnalystAnalystShortId :: Maybe Double
      <*> arbitrary -- radAnalystAnalystTitle :: Maybe Text
      <*> arbitrary -- radAnalystAssignmentUpdateDate :: Maybe Date
      <*> arbitrary -- radAnalystCommitteeId :: Text
      <*> arbitrary -- radAnalystCommitteeName :: Maybe Text
      <*> arbitrary -- radAnalystFirstName :: Maybe Text
      <*> arbitrary -- radAnalystLastName :: Maybe Text
      <*> arbitrary -- radAnalystRadBranch :: Maybe Text
      <*> arbitrary -- radAnalystTelephoneExt :: Maybe Double
    
instance Arbitrary RadAnalystPage where
  arbitrary =
    RadAnalystPage
      <$> arbitrary -- radAnalystPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- radAnalystPageResults :: Maybe [RadAnalyst]
    
instance Arbitrary ReportDate where
  arbitrary =
    ReportDate
      <$> arbitrary -- reportDateCreateDate :: Maybe Date
      <*> arbitrary -- reportDateDueDate :: Maybe Date
      <*> arbitrary -- reportDateReportType :: Maybe Text
      <*> arbitrary -- reportDateReportTypeFull :: Maybe Text
      <*> arbitrary -- reportDateReportYear :: Maybe Int
      <*> arbitrary -- reportDateUpdateDate :: Maybe Date
    
instance Arbitrary ReportDatePage where
  arbitrary =
    ReportDatePage
      <$> arbitrary -- reportDatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- reportDatePageResults :: Maybe [ReportDate]
    
instance Arbitrary ReportType where
  arbitrary =
    ReportType
      <$> arbitrary -- reportTypeReportType :: Text
      <*> arbitrary -- reportTypeReportTypeFull :: Maybe Text
    
instance Arbitrary ScheduleA where
  arbitrary =
    ScheduleA
      <$> arbitrary -- scheduleAAmendmentIndicator :: Maybe Text
      <*> arbitrary -- scheduleAAmendmentIndicatorDesc :: Maybe Text
      <*> arbitrary -- scheduleABackReferenceScheduleName :: Maybe Text
      <*> arbitrary -- scheduleABackReferenceTransactionId :: Maybe Text
      <*> arbitrary -- scheduleACandidateFirstName :: Maybe Text
      <*> arbitrary -- scheduleACandidateId :: Maybe Text
      <*> arbitrary -- scheduleACandidateLastName :: Maybe Text
      <*> arbitrary -- scheduleACandidateMiddleName :: Maybe Text
      <*> arbitrary -- scheduleACandidateName :: Maybe Text
      <*> arbitrary -- scheduleACandidateOffice :: Maybe Text
      <*> arbitrary -- scheduleACandidateOfficeDistrict :: Maybe Text
      <*> arbitrary -- scheduleACandidateOfficeFull :: Maybe Text
      <*> arbitrary -- scheduleACandidateOfficeState :: Maybe Text
      <*> arbitrary -- scheduleACandidateOfficeStateFull :: Maybe Text
      <*> arbitrary -- scheduleACandidatePrefix :: Maybe Text
      <*> arbitrary -- scheduleACandidateSuffix :: Maybe Text
      <*> arbitrary -- scheduleACommittee :: Maybe CommitteeHistory
      <*> arbitrary -- scheduleACommitteeId :: Maybe Text
      <*> arbitrary -- scheduleACommitteeName :: Maybe Text
      <*> arbitrary -- scheduleAConduitCommitteeCity :: Maybe Text
      <*> arbitrary -- scheduleAConduitCommitteeId :: Maybe Text
      <*> arbitrary -- scheduleAConduitCommitteeName :: Maybe Text
      <*> arbitrary -- scheduleAConduitCommitteeState :: Maybe Text
      <*> arbitrary -- scheduleAConduitCommitteeStreet1 :: Maybe Text
      <*> arbitrary -- scheduleAConduitCommitteeStreet2 :: Maybe Text
      <*> arbitrary -- scheduleAConduitCommitteeZip :: Maybe Int
      <*> arbitrary -- scheduleAContributionReceiptAmount :: Maybe Double
      <*> arbitrary -- scheduleAContributionReceiptDate :: Maybe Date
      <*> arbitrary -- scheduleAContributor :: Maybe CommitteeHistory
      <*> arbitrary -- scheduleAContributorAggregateYtd :: Maybe Double
      <*> arbitrary -- scheduleAContributorCity :: Maybe Text
      <*> arbitrary -- scheduleAContributorEmployer :: Maybe Text
      <*> arbitrary -- scheduleAContributorFirstName :: Maybe Text
      <*> arbitrary -- scheduleAContributorId :: Maybe Text
      <*> arbitrary -- scheduleAContributorLastName :: Maybe Text
      <*> arbitrary -- scheduleAContributorMiddleName :: Maybe Text
      <*> arbitrary -- scheduleAContributorName :: Maybe Text
      <*> arbitrary -- scheduleAContributorOccupation :: Maybe Text
      <*> arbitrary -- scheduleAContributorPrefix :: Maybe Text
      <*> arbitrary -- scheduleAContributorState :: Maybe Text
      <*> arbitrary -- scheduleAContributorStreet1 :: Maybe Text
      <*> arbitrary -- scheduleAContributorStreet2 :: Maybe Text
      <*> arbitrary -- scheduleAContributorSuffix :: Maybe Text
      <*> arbitrary -- scheduleAContributorZip :: Maybe Text
      <*> arbitrary -- scheduleADonorCommitteeName :: Maybe Text
      <*> arbitrary -- scheduleAElectionType :: Maybe Text
      <*> arbitrary -- scheduleAElectionTypeFull :: Maybe Text
      <*> arbitrary -- scheduleAEntityType :: Maybe Text
      <*> arbitrary -- scheduleAEntityTypeDesc :: Maybe Text
      <*> arbitrary -- scheduleAFecElectionTypeDesc :: Maybe Text
      <*> arbitrary -- scheduleAFecElectionYear :: Maybe Text
      <*> arbitrary -- scheduleAFileNumber :: Maybe Int
      <*> arbitrary -- scheduleAFilingForm :: Maybe Text
      <*> arbitrary -- scheduleAImageNumber :: Maybe Text
      <*> arbitrary -- scheduleAIncreasedLimit :: Maybe Text
      <*> arbitrary -- scheduleAIsIndividual :: Maybe Bool
      <*> arbitrary -- scheduleALineNumber :: Maybe Text
      <*> arbitrary -- scheduleALineNumberLabel :: Maybe Text
      <*> arbitrary -- scheduleALinkId :: Maybe Int
      <*> arbitrary -- scheduleALoadDate :: Maybe DateTime
      <*> arbitrary -- scheduleAMemoCode :: Maybe Text
      <*> arbitrary -- scheduleAMemoCodeFull :: Maybe Text
      <*> arbitrary -- scheduleAMemoText :: Maybe Text
      <*> arbitrary -- scheduleAMemoedSubtotal :: Maybe Bool
      <*> arbitrary -- scheduleANationalCommitteeNonfederalAccount :: Maybe Text
      <*> arbitrary -- scheduleAOriginalSubId :: Maybe Text
      <*> arbitrary -- scheduleAPdfUrl :: Maybe Text
      <*> arbitrary -- scheduleAReceiptType :: Maybe Text
      <*> arbitrary -- scheduleAReceiptTypeDesc :: Maybe Text
      <*> arbitrary -- scheduleAReceiptTypeFull :: Maybe Text
      <*> arbitrary -- scheduleAReportType :: Maybe Text
      <*> arbitrary -- scheduleAReportYear :: Maybe Int
      <*> arbitrary -- scheduleAScheduleType :: Maybe Text
      <*> arbitrary -- scheduleAScheduleTypeFull :: Maybe Text
      <*> arbitrary -- scheduleASubId :: Maybe Text
      <*> arbitrary -- scheduleATransactionId :: Maybe Text
      <*> arbitrary -- scheduleATwoYearTransactionPeriod :: Maybe Int
      <*> arbitrary -- scheduleAUnusedContbrId :: Maybe Text
    
instance Arbitrary ScheduleAByEmployer where
  arbitrary =
    ScheduleAByEmployer
      <$> arbitrary -- scheduleAByEmployerCommitteeId :: Text
      <*> arbitrary -- scheduleAByEmployerCount :: Maybe Int
      <*> arbitrary -- scheduleAByEmployerCycle :: Int
      <*> arbitrary -- scheduleAByEmployerEmployer :: Text
      <*> arbitrary -- scheduleAByEmployerTotal :: Maybe Double
    
instance Arbitrary ScheduleAByEmployerPage where
  arbitrary =
    ScheduleAByEmployerPage
      <$> arbitrary -- scheduleAByEmployerPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleAByEmployerPageResults :: Maybe [ScheduleAByEmployer]
    
instance Arbitrary ScheduleAByOccupation where
  arbitrary =
    ScheduleAByOccupation
      <$> arbitrary -- scheduleAByOccupationCommitteeId :: Text
      <*> arbitrary -- scheduleAByOccupationCount :: Maybe Int
      <*> arbitrary -- scheduleAByOccupationCycle :: Int
      <*> arbitrary -- scheduleAByOccupationOccupation :: Text
      <*> arbitrary -- scheduleAByOccupationTotal :: Maybe Double
    
instance Arbitrary ScheduleAByOccupationPage where
  arbitrary =
    ScheduleAByOccupationPage
      <$> arbitrary -- scheduleAByOccupationPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleAByOccupationPageResults :: Maybe [ScheduleAByOccupation]
    
instance Arbitrary ScheduleABySize where
  arbitrary =
    ScheduleABySize
      <$> arbitrary -- scheduleABySizeCommitteeId :: Text
      <*> arbitrary -- scheduleABySizeCount :: Maybe Int
      <*> arbitrary -- scheduleABySizeCycle :: Int
      <*> arbitrary -- scheduleABySizeSize :: Int
      <*> arbitrary -- scheduleABySizeTotal :: Maybe Double
    
instance Arbitrary ScheduleABySizeCandidate where
  arbitrary =
    ScheduleABySizeCandidate
      <$> arbitrary -- scheduleABySizeCandidateCandidateId :: Maybe Text
      <*> arbitrary -- scheduleABySizeCandidateCycle :: Maybe Int
      <*> arbitrary -- scheduleABySizeCandidateSize :: Maybe Int
      <*> arbitrary -- scheduleABySizeCandidateTotal :: Maybe Double
    
instance Arbitrary ScheduleABySizeCandidatePage where
  arbitrary =
    ScheduleABySizeCandidatePage
      <$> arbitrary -- scheduleABySizeCandidatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleABySizeCandidatePageResults :: Maybe [ScheduleABySizeCandidate]
    
instance Arbitrary ScheduleABySizePage where
  arbitrary =
    ScheduleABySizePage
      <$> arbitrary -- scheduleABySizePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleABySizePageResults :: Maybe [ScheduleABySize]
    
instance Arbitrary ScheduleAByState where
  arbitrary =
    ScheduleAByState
      <$> arbitrary -- scheduleAByStateCommitteeId :: Text
      <*> arbitrary -- scheduleAByStateCount :: Maybe Int
      <*> arbitrary -- scheduleAByStateCycle :: Int
      <*> arbitrary -- scheduleAByStateState :: Text
      <*> arbitrary -- scheduleAByStateStateFull :: Text
      <*> arbitrary -- scheduleAByStateTotal :: Maybe Double
    
instance Arbitrary ScheduleAByStateCandidate where
  arbitrary =
    ScheduleAByStateCandidate
      <$> arbitrary -- scheduleAByStateCandidateCandidateId :: Maybe Text
      <*> arbitrary -- scheduleAByStateCandidateCycle :: Maybe Int
      <*> arbitrary -- scheduleAByStateCandidateState :: Maybe Text
      <*> arbitrary -- scheduleAByStateCandidateStateFull :: Maybe Text
      <*> arbitrary -- scheduleAByStateCandidateTotal :: Maybe Double
    
instance Arbitrary ScheduleAByStateCandidatePage where
  arbitrary =
    ScheduleAByStateCandidatePage
      <$> arbitrary -- scheduleAByStateCandidatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleAByStateCandidatePageResults :: Maybe [ScheduleAByStateCandidate]
    
instance Arbitrary ScheduleAByStatePage where
  arbitrary =
    ScheduleAByStatePage
      <$> arbitrary -- scheduleAByStatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleAByStatePageResults :: Maybe [ScheduleAByState]
    
instance Arbitrary ScheduleAByStateRecipientTotals where
  arbitrary =
    ScheduleAByStateRecipientTotals
      <$> arbitrary -- scheduleAByStateRecipientTotalsCommitteeType :: Maybe Text
      <*> arbitrary -- scheduleAByStateRecipientTotalsCommitteeTypeFull :: Maybe Text
      <*> arbitrary -- scheduleAByStateRecipientTotalsCount :: Maybe Int
      <*> arbitrary -- scheduleAByStateRecipientTotalsCycle :: Maybe Int
      <*> arbitrary -- scheduleAByStateRecipientTotalsState :: Maybe Text
      <*> arbitrary -- scheduleAByStateRecipientTotalsStateFull :: Maybe Text
      <*> arbitrary -- scheduleAByStateRecipientTotalsTotal :: Maybe Double
    
instance Arbitrary ScheduleAByStateRecipientTotalsPage where
  arbitrary =
    ScheduleAByStateRecipientTotalsPage
      <$> arbitrary -- scheduleAByStateRecipientTotalsPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleAByStateRecipientTotalsPageResults :: Maybe [ScheduleAByStateRecipientTotals]
    
instance Arbitrary ScheduleAByZip where
  arbitrary =
    ScheduleAByZip
      <$> arbitrary -- scheduleAByZipCommitteeId :: Text
      <*> arbitrary -- scheduleAByZipCount :: Maybe Int
      <*> arbitrary -- scheduleAByZipCycle :: Int
      <*> arbitrary -- scheduleAByZipState :: Maybe Text
      <*> arbitrary -- scheduleAByZipStateFull :: Maybe Text
      <*> arbitrary -- scheduleAByZipTotal :: Maybe Double
      <*> arbitrary -- scheduleAByZipZip :: Text
    
instance Arbitrary ScheduleAByZipPage where
  arbitrary =
    ScheduleAByZipPage
      <$> arbitrary -- scheduleAByZipPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleAByZipPageResults :: Maybe [ScheduleAByZip]
    
instance Arbitrary ScheduleAEfile where
  arbitrary =
    ScheduleAEfile
      <$> arbitrary -- scheduleAEfileAmendmentIndicator :: Maybe Text
      <*> arbitrary -- scheduleAEfileBackReferenceScheduleName :: Maybe Text
      <*> arbitrary -- scheduleAEfileBackReferenceTransactionId :: Maybe Text
      <*> arbitrary -- scheduleAEfileBeginningImageNumber :: Maybe Text
      <*> arbitrary -- scheduleAEfileCommittee :: Maybe CommitteeHistory
      <*> arbitrary -- scheduleAEfileCommitteeId :: Maybe Text
      <*> arbitrary -- scheduleAEfileConduitCommitteeCity :: Maybe Text
      <*> arbitrary -- scheduleAEfileConduitCommitteeId :: Maybe Text
      <*> arbitrary -- scheduleAEfileConduitCommitteeName :: Maybe Text
      <*> arbitrary -- scheduleAEfileConduitCommitteeState :: Maybe Text
      <*> arbitrary -- scheduleAEfileConduitCommitteeStreet1 :: Maybe Text
      <*> arbitrary -- scheduleAEfileConduitCommitteeStreet2 :: Maybe Text
      <*> arbitrary -- scheduleAEfileConduitCommitteeZip :: Maybe Int
      <*> arbitrary -- scheduleAEfileContributionReceiptAmount :: Maybe Double
      <*> arbitrary -- scheduleAEfileContributionReceiptDate :: Maybe Date
      <*> arbitrary -- scheduleAEfileContributorAggregateYtd :: Maybe Double
      <*> arbitrary -- scheduleAEfileContributorCity :: Maybe Text
      <*> arbitrary -- scheduleAEfileContributorEmployer :: Maybe Text
      <*> arbitrary -- scheduleAEfileContributorFirstName :: Maybe Text
      <*> arbitrary -- scheduleAEfileContributorLastName :: Maybe Text
      <*> arbitrary -- scheduleAEfileContributorMiddleName :: Maybe Text
      <*> arbitrary -- scheduleAEfileContributorName :: Maybe Text
      <*> arbitrary -- scheduleAEfileContributorOccupation :: Maybe Text
      <*> arbitrary -- scheduleAEfileContributorPrefix :: Maybe Text
      <*> arbitrary -- scheduleAEfileContributorState :: Maybe Text
      <*> arbitrary -- scheduleAEfileContributorSuffix :: Maybe Text
      <*> arbitrary -- scheduleAEfileContributorZip :: Maybe Text
      <*> arbitrary -- scheduleAEfileCsvUrl :: Maybe Text
      <*> arbitrary -- scheduleAEfileCycle :: Maybe Int
      <*> arbitrary -- scheduleAEfileEntityType :: Maybe Text
      <*> arbitrary -- scheduleAEfileFecElectionTypeDesc :: Maybe Text
      <*> arbitrary -- scheduleAEfileFecUrl :: Maybe Text
      <*> arbitrary -- scheduleAEfileFileNumber :: Int
      <*> arbitrary -- scheduleAEfileFiling :: Maybe EFilings
      <*> arbitrary -- scheduleAEfileImageNumber :: Maybe Text
      <*> arbitrary -- scheduleAEfileLineNumber :: Maybe Text
      <*> arbitrary -- scheduleAEfileLoadTimestamp :: Maybe DateTime
      <*> arbitrary -- scheduleAEfileMemoCode :: Maybe Text
      <*> arbitrary -- scheduleAEfileMemoText :: Maybe Text
      <*> arbitrary -- scheduleAEfilePdfUrl :: Maybe Text
      <*> arbitrary -- scheduleAEfilePgo :: Maybe Text
      <*> arbitrary -- scheduleAEfileRelatedLineNumber :: Int
      <*> arbitrary -- scheduleAEfileReportType :: Maybe Text
      <*> arbitrary -- scheduleAEfileTransactionId :: Maybe Text
    
instance Arbitrary ScheduleAEfilePage where
  arbitrary =
    ScheduleAEfilePage
      <$> arbitrary -- scheduleAEfilePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleAEfilePageResults :: Maybe [ScheduleAEfile]
    
instance Arbitrary ScheduleAPage where
  arbitrary =
    ScheduleAPage
      <$> arbitrary -- scheduleAPagePagination :: Maybe SeekInfo
      <*> arbitrary -- scheduleAPageResults :: Maybe [ScheduleA]
    
instance Arbitrary ScheduleB where
  arbitrary =
    ScheduleB
      <$> arbitrary -- scheduleBAmendmentIndicator :: Maybe Text
      <*> arbitrary -- scheduleBAmendmentIndicatorDesc :: Maybe Text
      <*> arbitrary -- scheduleBBackReferenceScheduleId :: Maybe Text
      <*> arbitrary -- scheduleBBackReferenceTransactionId :: Maybe Text
      <*> arbitrary -- scheduleBBeneficiaryCommitteeName :: Maybe Text
      <*> arbitrary -- scheduleBCandidateFirstName :: Maybe Text
      <*> arbitrary -- scheduleBCandidateId :: Maybe Text
      <*> arbitrary -- scheduleBCandidateLastName :: Maybe Text
      <*> arbitrary -- scheduleBCandidateMiddleName :: Maybe Text
      <*> arbitrary -- scheduleBCandidateName :: Maybe Text
      <*> arbitrary -- scheduleBCandidateOffice :: Maybe Text
      <*> arbitrary -- scheduleBCandidateOfficeDescription :: Maybe Text
      <*> arbitrary -- scheduleBCandidateOfficeDistrict :: Maybe Text
      <*> arbitrary -- scheduleBCandidateOfficeState :: Maybe Text
      <*> arbitrary -- scheduleBCandidateOfficeStateFull :: Maybe Text
      <*> arbitrary -- scheduleBCandidatePrefix :: Maybe Text
      <*> arbitrary -- scheduleBCandidateSuffix :: Maybe Text
      <*> arbitrary -- scheduleBCategoryCode :: Maybe Text
      <*> arbitrary -- scheduleBCategoryCodeFull :: Maybe Text
      <*> arbitrary -- scheduleBCommDt :: Maybe Date
      <*> arbitrary -- scheduleBCommittee :: Maybe CommitteeHistory
      <*> arbitrary -- scheduleBCommitteeId :: Maybe Text
      <*> arbitrary -- scheduleBConduitCommitteeCity :: Maybe Text
      <*> arbitrary -- scheduleBConduitCommitteeName :: Maybe Text
      <*> arbitrary -- scheduleBConduitCommitteeState :: Maybe Text
      <*> arbitrary -- scheduleBConduitCommitteeStreet1 :: Maybe Text
      <*> arbitrary -- scheduleBConduitCommitteeStreet2 :: Maybe Text
      <*> arbitrary -- scheduleBConduitCommitteeZip :: Maybe Int
      <*> arbitrary -- scheduleBDisbursementAmount :: Maybe Double
      <*> arbitrary -- scheduleBDisbursementDate :: Maybe Date
      <*> arbitrary -- scheduleBDisbursementDescription :: Maybe Text
      <*> arbitrary -- scheduleBDisbursementPurposeCategory :: Maybe Text
      <*> arbitrary -- scheduleBDisbursementType :: Maybe Text
      <*> arbitrary -- scheduleBDisbursementTypeDescription :: Maybe Text
      <*> arbitrary -- scheduleBElectionType :: Maybe Text
      <*> arbitrary -- scheduleBElectionTypeFull :: Maybe Text
      <*> arbitrary -- scheduleBEntityType :: Maybe Text
      <*> arbitrary -- scheduleBEntityTypeDesc :: Maybe Text
      <*> arbitrary -- scheduleBFecElectionTypeDesc :: Maybe Text
      <*> arbitrary -- scheduleBFecElectionYear :: Maybe Text
      <*> arbitrary -- scheduleBFileNumber :: Maybe Int
      <*> arbitrary -- scheduleBFilingForm :: Maybe Text
      <*> arbitrary -- scheduleBImageNumber :: Maybe Text
      <*> arbitrary -- scheduleBLineNumber :: Maybe Text
      <*> arbitrary -- scheduleBLineNumberLabel :: Maybe Text
      <*> arbitrary -- scheduleBLinkId :: Maybe Int
      <*> arbitrary -- scheduleBLoadDate :: Maybe DateTime
      <*> arbitrary -- scheduleBMemoCode :: Maybe Text
      <*> arbitrary -- scheduleBMemoCodeFull :: Maybe Text
      <*> arbitrary -- scheduleBMemoText :: Maybe Text
      <*> arbitrary -- scheduleBMemoedSubtotal :: Maybe Bool
      <*> arbitrary -- scheduleBNationalCommitteeNonfederalAccount :: Maybe Text
      <*> arbitrary -- scheduleBOriginalSubId :: Maybe Text
      <*> arbitrary -- scheduleBPayeeEmployer :: Maybe Text
      <*> arbitrary -- scheduleBPayeeFirstName :: Maybe Text
      <*> arbitrary -- scheduleBPayeeLastName :: Maybe Text
      <*> arbitrary -- scheduleBPayeeMiddleName :: Maybe Text
      <*> arbitrary -- scheduleBPayeeOccupation :: Maybe Text
      <*> arbitrary -- scheduleBPayeePrefix :: Maybe Text
      <*> arbitrary -- scheduleBPayeeSuffix :: Maybe Text
      <*> arbitrary -- scheduleBPdfUrl :: Maybe Text
      <*> arbitrary -- scheduleBRecipientCity :: Maybe Text
      <*> arbitrary -- scheduleBRecipientCommittee :: Maybe CommitteeHistory
      <*> arbitrary -- scheduleBRecipientCommitteeId :: Maybe Text
      <*> arbitrary -- scheduleBRecipientName :: Maybe Text
      <*> arbitrary -- scheduleBRecipientState :: Maybe Text
      <*> arbitrary -- scheduleBRecipientZip :: Maybe Text
      <*> arbitrary -- scheduleBRefDispExcessFlg :: Maybe Text
      <*> arbitrary -- scheduleBReportType :: Maybe Text
      <*> arbitrary -- scheduleBReportYear :: Maybe Int
      <*> arbitrary -- scheduleBScheduleType :: Maybe Text
      <*> arbitrary -- scheduleBScheduleTypeFull :: Maybe Text
      <*> arbitrary -- scheduleBSemiAnnualBundledRefund :: Maybe Double
      <*> arbitrary -- scheduleBSpenderCommitteeType :: Maybe Text
      <*> arbitrary -- scheduleBSubId :: Maybe Text
      <*> arbitrary -- scheduleBTransactionId :: Maybe Text
      <*> arbitrary -- scheduleBTwoYearTransactionPeriod :: Maybe Int
      <*> arbitrary -- scheduleBUnusedRecipientCommitteeId :: Maybe Text
    
instance Arbitrary ScheduleBByPurpose where
  arbitrary =
    ScheduleBByPurpose
      <$> arbitrary -- scheduleBByPurposeCommitteeId :: Text
      <*> arbitrary -- scheduleBByPurposeCount :: Maybe Int
      <*> arbitrary -- scheduleBByPurposeCycle :: Int
      <*> arbitrary -- scheduleBByPurposePurpose :: Text
      <*> arbitrary -- scheduleBByPurposeTotal :: Maybe Double
    
instance Arbitrary ScheduleBByPurposePage where
  arbitrary =
    ScheduleBByPurposePage
      <$> arbitrary -- scheduleBByPurposePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleBByPurposePageResults :: Maybe [ScheduleBByPurpose]
    
instance Arbitrary ScheduleBByRecipient where
  arbitrary =
    ScheduleBByRecipient
      <$> arbitrary -- scheduleBByRecipientCommitteeId :: Text
      <*> arbitrary -- scheduleBByRecipientCount :: Maybe Int
      <*> arbitrary -- scheduleBByRecipientCycle :: Int
      <*> arbitrary -- scheduleBByRecipientRecipientName :: Text
      <*> arbitrary -- scheduleBByRecipientTotal :: Maybe Double
    
instance Arbitrary ScheduleBByRecipientID where
  arbitrary =
    ScheduleBByRecipientID
      <$> arbitrary -- scheduleBByRecipientIDCommitteeId :: Text
      <*> arbitrary -- scheduleBByRecipientIDCommitteeName :: Maybe Text
      <*> arbitrary -- scheduleBByRecipientIDCount :: Maybe Int
      <*> arbitrary -- scheduleBByRecipientIDCycle :: Int
      <*> arbitrary -- scheduleBByRecipientIDIdx :: Maybe Int
      <*> arbitrary -- scheduleBByRecipientIDRecipientId :: Text
      <*> arbitrary -- scheduleBByRecipientIDRecipientName :: Maybe Text
      <*> arbitrary -- scheduleBByRecipientIDTotal :: Maybe Double
    
instance Arbitrary ScheduleBByRecipientIDPage where
  arbitrary =
    ScheduleBByRecipientIDPage
      <$> arbitrary -- scheduleBByRecipientIDPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleBByRecipientIDPageResults :: Maybe [ScheduleBByRecipientID]
    
instance Arbitrary ScheduleBByRecipientPage where
  arbitrary =
    ScheduleBByRecipientPage
      <$> arbitrary -- scheduleBByRecipientPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleBByRecipientPageResults :: Maybe [ScheduleBByRecipient]
    
instance Arbitrary ScheduleBEfile where
  arbitrary =
    ScheduleBEfile
      <$> arbitrary -- scheduleBEfileAmendmentIndicator :: Maybe Text
      <*> arbitrary -- scheduleBEfileBackReferenceScheduleName :: Maybe Text
      <*> arbitrary -- scheduleBEfileBackReferenceTransactionId :: Maybe Text
      <*> arbitrary -- scheduleBEfileBeginningImageNumber :: Maybe Text
      <*> arbitrary -- scheduleBEfileBeneficiaryCommitteeName :: Maybe Text
      <*> arbitrary -- scheduleBEfileCandidateOffice :: Maybe Text
      <*> arbitrary -- scheduleBEfileCandidateOfficeDistrict :: Maybe Text
      <*> arbitrary -- scheduleBEfileCommittee :: Maybe CommitteeHistory
      <*> arbitrary -- scheduleBEfileCommitteeId :: Maybe Text
      <*> arbitrary -- scheduleBEfileCsvUrl :: Maybe Text
      <*> arbitrary -- scheduleBEfileDisbursementAmount :: Maybe Double
      <*> arbitrary -- scheduleBEfileDisbursementDate :: Maybe Date
      <*> arbitrary -- scheduleBEfileDisbursementDescription :: Maybe Text
      <*> arbitrary -- scheduleBEfileDisbursementType :: Maybe Text
      <*> arbitrary -- scheduleBEfileEntityType :: Maybe Text
      <*> arbitrary -- scheduleBEfileFecUrl :: Maybe Text
      <*> arbitrary -- scheduleBEfileFileNumber :: Int
      <*> arbitrary -- scheduleBEfileFiling :: Maybe EFilings
      <*> arbitrary -- scheduleBEfileImageNumber :: Maybe Text
      <*> arbitrary -- scheduleBEfileIsNotice :: Maybe Bool
      <*> arbitrary -- scheduleBEfileLineNumber :: Maybe Text
      <*> arbitrary -- scheduleBEfileLoadTimestamp :: Maybe DateTime
      <*> arbitrary -- scheduleBEfileMemoCode :: Maybe Text
      <*> arbitrary -- scheduleBEfileMemoText :: Maybe Text
      <*> arbitrary -- scheduleBEfilePayeeName :: Maybe Text
      <*> arbitrary -- scheduleBEfilePdfUrl :: Maybe Text
      <*> arbitrary -- scheduleBEfileRecipientCity :: Maybe Text
      <*> arbitrary -- scheduleBEfileRecipientName :: Maybe Text
      <*> arbitrary -- scheduleBEfileRecipientPrefix :: Maybe Text
      <*> arbitrary -- scheduleBEfileRecipientState :: Maybe Text
      <*> arbitrary -- scheduleBEfileRecipientSuffix :: Maybe Text
      <*> arbitrary -- scheduleBEfileRecipientZip :: Maybe Text
      <*> arbitrary -- scheduleBEfileRelatedLineNumber :: Int
      <*> arbitrary -- scheduleBEfileReportType :: Maybe Text
      <*> arbitrary -- scheduleBEfileSemiAnnualBundledRefund :: Maybe Int
      <*> arbitrary -- scheduleBEfileTransactionId :: Maybe Text
    
instance Arbitrary ScheduleBEfilePage where
  arbitrary =
    ScheduleBEfilePage
      <$> arbitrary -- scheduleBEfilePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleBEfilePageResults :: Maybe [ScheduleBEfile]
    
instance Arbitrary ScheduleBPage where
  arbitrary =
    ScheduleBPage
      <$> arbitrary -- scheduleBPagePagination :: Maybe SeekInfo
      <*> arbitrary -- scheduleBPageResults :: Maybe [ScheduleB]
    
instance Arbitrary ScheduleE where
  arbitrary =
    ScheduleE
      <$> arbitrary -- scheduleEActionCode :: Maybe Text
      <*> arbitrary -- scheduleEActionCodeFull :: Maybe Text
      <*> arbitrary -- scheduleEAmendmentIndicator :: Maybe Text
      <*> arbitrary -- scheduleEAmendmentNumber :: Maybe Int
      <*> arbitrary -- scheduleEBackReferenceScheduleName :: Maybe Text
      <*> arbitrary -- scheduleEBackReferenceTransactionId :: Maybe Text
      <*> arbitrary -- scheduleECandidate :: Maybe Text
      <*> arbitrary -- scheduleECandidateFirstName :: Maybe Text
      <*> arbitrary -- scheduleECandidateId :: Maybe Text
      <*> arbitrary -- scheduleECandidateLastName :: Maybe Text
      <*> arbitrary -- scheduleECandidateMiddleName :: Maybe Text
      <*> arbitrary -- scheduleECandidateName :: Maybe Text
      <*> arbitrary -- scheduleECandidateOffice :: Maybe Text
      <*> arbitrary -- scheduleECandidateOfficeDistrict :: Maybe Text
      <*> arbitrary -- scheduleECandidateOfficeState :: Maybe Text
      <*> arbitrary -- scheduleECandidateParty :: Maybe Text
      <*> arbitrary -- scheduleECandidatePrefix :: Maybe Text
      <*> arbitrary -- scheduleECandidateSuffix :: Maybe Text
      <*> arbitrary -- scheduleECategoryCode :: Maybe Text
      <*> arbitrary -- scheduleECategoryCodeFull :: Maybe Text
      <*> arbitrary -- scheduleECommittee :: Maybe CommitteeHistory
      <*> arbitrary -- scheduleECommitteeId :: Maybe Text
      <*> arbitrary -- scheduleEConduitCommitteeCity :: Maybe Text
      <*> arbitrary -- scheduleEConduitCommitteeId :: Maybe Text
      <*> arbitrary -- scheduleEConduitCommitteeName :: Maybe Text
      <*> arbitrary -- scheduleEConduitCommitteeState :: Maybe Text
      <*> arbitrary -- scheduleEConduitCommitteeStreet1 :: Maybe Text
      <*> arbitrary -- scheduleEConduitCommitteeStreet2 :: Maybe Text
      <*> arbitrary -- scheduleEConduitCommitteeZip :: Maybe Int
      <*> arbitrary -- scheduleEDisseminationDate :: Maybe Date
      <*> arbitrary -- scheduleEElectionType :: Maybe Text
      <*> arbitrary -- scheduleEElectionTypeFull :: Maybe Text
      <*> arbitrary -- scheduleEExpenditureAmount :: Maybe Double
      <*> arbitrary -- scheduleEExpenditureDate :: Maybe Date
      <*> arbitrary -- scheduleEExpenditureDescription :: Maybe Text
      <*> arbitrary -- scheduleEFileNumber :: Maybe Int
      <*> arbitrary -- scheduleEFilerFirstName :: Maybe Text
      <*> arbitrary -- scheduleEFilerLastName :: Maybe Text
      <*> arbitrary -- scheduleEFilerMiddleName :: Maybe Text
      <*> arbitrary -- scheduleEFilerPrefix :: Maybe Text
      <*> arbitrary -- scheduleEFilerSuffix :: Maybe Text
      <*> arbitrary -- scheduleEFilingForm :: Maybe Text
      <*> arbitrary -- scheduleEImageNumber :: Maybe Text
      <*> arbitrary -- scheduleEIndependentSignDate :: Maybe Date
      <*> arbitrary -- scheduleEIndependentSignName :: Maybe Text
      <*> arbitrary -- scheduleEIsNotice :: Maybe Bool
      <*> arbitrary -- scheduleELineNumber :: Maybe Text
      <*> arbitrary -- scheduleELinkId :: Maybe Int
      <*> arbitrary -- scheduleEMemoCode :: Maybe Text
      <*> arbitrary -- scheduleEMemoCodeFull :: Maybe Text
      <*> arbitrary -- scheduleEMemoText :: Maybe Text
      <*> arbitrary -- scheduleEMemoedSubtotal :: Maybe Bool
      <*> arbitrary -- scheduleENotaryCommissionExpirationDate :: Maybe Date
      <*> arbitrary -- scheduleENotarySignDate :: Maybe Date
      <*> arbitrary -- scheduleENotarySignName :: Maybe Text
      <*> arbitrary -- scheduleEOfficeTotalYtd :: Maybe Double
      <*> arbitrary -- scheduleEOriginalSubId :: Maybe Text
      <*> arbitrary -- scheduleEPayeeCity :: Maybe Text
      <*> arbitrary -- scheduleEPayeeFirstName :: Maybe Text
      <*> arbitrary -- scheduleEPayeeLastName :: Maybe Text
      <*> arbitrary -- scheduleEPayeeMiddleName :: Maybe Text
      <*> arbitrary -- scheduleEPayeeName :: Maybe Text
      <*> arbitrary -- scheduleEPayeePrefix :: Maybe Text
      <*> arbitrary -- scheduleEPayeeState :: Maybe Text
      <*> arbitrary -- scheduleEPayeeStreet1 :: Maybe Text
      <*> arbitrary -- scheduleEPayeeStreet2 :: Maybe Text
      <*> arbitrary -- scheduleEPayeeSuffix :: Maybe Text
      <*> arbitrary -- scheduleEPayeeZip :: Maybe Text
      <*> arbitrary -- scheduleEPdfUrl :: Maybe Text
      <*> arbitrary -- scheduleEPreviousFileNumber :: Maybe Int
      <*> arbitrary -- scheduleEReportType :: Maybe Text
      <*> arbitrary -- scheduleEReportYear :: Maybe Int
      <*> arbitrary -- scheduleEScheduleType :: Maybe Text
      <*> arbitrary -- scheduleEScheduleTypeFull :: Maybe Text
      <*> arbitrary -- scheduleESubId :: Maybe Text
      <*> arbitrary -- scheduleESupportOpposeIndicator :: Maybe Text
      <*> arbitrary -- scheduleETransactionId :: Maybe Int
    
instance Arbitrary ScheduleEByCandidate where
  arbitrary =
    ScheduleEByCandidate
      <$> arbitrary -- scheduleEByCandidateCandidateId :: Maybe Text
      <*> arbitrary -- scheduleEByCandidateCandidateName :: Maybe Text
      <*> arbitrary -- scheduleEByCandidateCommitteeId :: Maybe Text
      <*> arbitrary -- scheduleEByCandidateCommitteeName :: Maybe Text
      <*> arbitrary -- scheduleEByCandidateCount :: Maybe Int
      <*> arbitrary -- scheduleEByCandidateCycle :: Int
      <*> arbitrary -- scheduleEByCandidateSupportOpposeIndicator :: Text
      <*> arbitrary -- scheduleEByCandidateTotal :: Maybe Double
    
instance Arbitrary ScheduleEByCandidatePage where
  arbitrary =
    ScheduleEByCandidatePage
      <$> arbitrary -- scheduleEByCandidatePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleEByCandidatePageResults :: Maybe [ScheduleEByCandidate]
    
instance Arbitrary ScheduleEEfile where
  arbitrary =
    ScheduleEEfile
      <$> arbitrary -- scheduleEEfileAmendmentIndicator :: Maybe Text
      <*> arbitrary -- scheduleEEfileBackReferenceScheduleName :: Maybe Text
      <*> arbitrary -- scheduleEEfileBackReferenceTransactionId :: Maybe Text
      <*> arbitrary -- scheduleEEfileBeginningImageNumber :: Maybe Text
      <*> arbitrary -- scheduleEEfileCandOfficeDistrict :: Maybe Text
      <*> arbitrary -- scheduleEEfileCandOfficeState :: Maybe Text
      <*> arbitrary -- scheduleEEfileCandidateFirstName :: Maybe Text
      <*> arbitrary -- scheduleEEfileCandidateId :: Maybe Text
      <*> arbitrary -- scheduleEEfileCandidateMiddleName :: Maybe Text
      <*> arbitrary -- scheduleEEfileCandidateName :: Maybe Text
      <*> arbitrary -- scheduleEEfileCandidateOffice :: Maybe Text
      <*> arbitrary -- scheduleEEfileCandidatePrefix :: Maybe Text
      <*> arbitrary -- scheduleEEfileCandidateSuffix :: Maybe Text
      <*> arbitrary -- scheduleEEfileCategoryCode :: Maybe Text
      <*> arbitrary -- scheduleEEfileCommittee :: Maybe CommitteeHistory
      <*> arbitrary -- scheduleEEfileCommitteeId :: Maybe Text
      <*> arbitrary -- scheduleEEfileCsvUrl :: Maybe Text
      <*> arbitrary -- scheduleEEfileDisseminationDate :: Maybe Date
      <*> arbitrary -- scheduleEEfileEntityType :: Maybe Text
      <*> arbitrary -- scheduleEEfileExpenditureAmount :: Maybe Int
      <*> arbitrary -- scheduleEEfileExpenditureDate :: Maybe Date
      <*> arbitrary -- scheduleEEfileExpenditureDescription :: Maybe Text
      <*> arbitrary -- scheduleEEfileFecUrl :: Maybe Text
      <*> arbitrary -- scheduleEEfileFileNumber :: Int
      <*> arbitrary -- scheduleEEfileFilerFirstName :: Maybe Text
      <*> arbitrary -- scheduleEEfileFilerLastName :: Maybe Text
      <*> arbitrary -- scheduleEEfileFilerMiddleName :: Maybe Text
      <*> arbitrary -- scheduleEEfileFilerPrefix :: Maybe Text
      <*> arbitrary -- scheduleEEfileFilerSuffix :: Maybe Text
      <*> arbitrary -- scheduleEEfileFiling :: Maybe EFilings
      <*> arbitrary -- scheduleEEfileImageNumber :: Maybe Text
      <*> arbitrary -- scheduleEEfileIsNotice :: Maybe Bool
      <*> arbitrary -- scheduleEEfileLineNumber :: Maybe Text
      <*> arbitrary -- scheduleEEfileLoadTimestamp :: Maybe DateTime
      <*> arbitrary -- scheduleEEfileMemoCode :: Maybe Text
      <*> arbitrary -- scheduleEEfileMemoText :: Maybe Text
      <*> arbitrary -- scheduleEEfileNotarySignDate :: Maybe Date
      <*> arbitrary -- scheduleEEfileOfficeTotalYtd :: Maybe Float
      <*> arbitrary -- scheduleEEfilePayeeCity :: Maybe Text
      <*> arbitrary -- scheduleEEfilePayeeFirstName :: Maybe Text
      <*> arbitrary -- scheduleEEfilePayeeLastName :: Maybe Text
      <*> arbitrary -- scheduleEEfilePayeeMiddleName :: Maybe Text
      <*> arbitrary -- scheduleEEfilePayeeName :: Maybe Text
      <*> arbitrary -- scheduleEEfilePayeePrefix :: Maybe Text
      <*> arbitrary -- scheduleEEfilePayeeState :: Maybe Text
      <*> arbitrary -- scheduleEEfilePayeeStreet1 :: Maybe Text
      <*> arbitrary -- scheduleEEfilePayeeStreet2 :: Maybe Text
      <*> arbitrary -- scheduleEEfilePayeeSuffix :: Maybe Text
      <*> arbitrary -- scheduleEEfilePayeeZip :: Maybe Text
      <*> arbitrary -- scheduleEEfilePdfUrl :: Maybe Text
      <*> arbitrary -- scheduleEEfileRelatedLineNumber :: Int
      <*> arbitrary -- scheduleEEfileReportType :: Maybe Text
      <*> arbitrary -- scheduleEEfileSupportOpposeIndicator :: Maybe Text
      <*> arbitrary -- scheduleEEfileTransactionId :: Maybe Text
    
instance Arbitrary ScheduleEEfilePage where
  arbitrary =
    ScheduleEEfilePage
      <$> arbitrary -- scheduleEEfilePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- scheduleEEfilePageResults :: Maybe [ScheduleEEfile]
    
instance Arbitrary ScheduleEPage where
  arbitrary =
    ScheduleEPage
      <$> arbitrary -- scheduleEPagePagination :: Maybe SeekInfo
      <*> arbitrary -- scheduleEPageResults :: Maybe [ScheduleE]
    
instance Arbitrary SeekInfo where
  arbitrary =
    SeekInfo
      <$> arbitrary -- seekInfoCount :: Maybe Int
      <*> arbitrary -- seekInfoLastIndexes :: Maybe Text
      <*> arbitrary -- seekInfoPages :: Maybe Int
      <*> arbitrary -- seekInfoPerPage :: Maybe Int
    
instance Arbitrary StateElectionOfficeInfo where
  arbitrary =
    StateElectionOfficeInfo
      <$> arbitrary -- stateElectionOfficeInfoAddressLine1 :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoAddressLine2 :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoCity :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoEmail :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoFaxNumber :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoMailingAddress1 :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoMailingAddress2 :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoMailingCity :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoMailingState :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoMailingZipcode :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoOfficeName :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoOfficeType :: Text
      <*> arbitrary -- stateElectionOfficeInfoPrimaryPhoneNumber :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoSecondaryPhoneNumber :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoState :: Text
      <*> arbitrary -- stateElectionOfficeInfoStateFullName :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoWebsiteUrl1 :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoWebsiteUrl2 :: Maybe Text
      <*> arbitrary -- stateElectionOfficeInfoZipCode :: Maybe Text
    
instance Arbitrary StateElectionOfficeInfoPage where
  arbitrary =
    StateElectionOfficeInfoPage
      <$> arbitrary -- stateElectionOfficeInfoPagePagination :: Maybe OffsetInfo
      <*> arbitrary -- stateElectionOfficeInfoPageResults :: Maybe [StateElectionOfficeInfo]
    
instance Arbitrary TotalsCommittee where
  arbitrary =
    TotalsCommittee
      <$> arbitrary -- totalsCommitteeCandidateIds :: Maybe [Text]
      <*> arbitrary -- totalsCommitteeCashOnHandEndPeriod :: Maybe Double
      <*> arbitrary -- totalsCommitteeCity :: Maybe Text
      <*> arbitrary -- totalsCommitteeCommitteeId :: Text
      <*> arbitrary -- totalsCommitteeCommitteeType :: Maybe Text
      <*> arbitrary -- totalsCommitteeCommitteeTypeFull :: Maybe Text
      <*> arbitrary -- totalsCommitteeCycle :: Int
      <*> arbitrary -- totalsCommitteeCycles :: Maybe [Int]
      <*> arbitrary -- totalsCommitteeDebtsOwedByCommittee :: Maybe Double
      <*> arbitrary -- totalsCommitteeDesignation :: Maybe Text
      <*> arbitrary -- totalsCommitteeDesignationFull :: Maybe Text
      <*> arbitrary -- totalsCommitteeDisbursements :: Maybe Double
      <*> arbitrary -- totalsCommitteeFilingFrequency :: Maybe Text
      <*> arbitrary -- totalsCommitteeIndependentExpenditures :: Maybe Double
      <*> arbitrary -- totalsCommitteeName :: Maybe Text
      <*> arbitrary -- totalsCommitteeOrganizationType :: Maybe Text
      <*> arbitrary -- totalsCommitteeOrganizationTypeFull :: Maybe Text
      <*> arbitrary -- totalsCommitteeParty :: Maybe Text
      <*> arbitrary -- totalsCommitteePartyFull :: Maybe Text
      <*> arbitrary -- totalsCommitteeReceipts :: Maybe Double
      <*> arbitrary -- totalsCommitteeState :: Maybe Text
      <*> arbitrary -- totalsCommitteeStateFull :: Maybe Text
      <*> arbitrary -- totalsCommitteeStreet1 :: Maybe Text
      <*> arbitrary -- totalsCommitteeStreet2 :: Maybe Text
      <*> arbitrary -- totalsCommitteeTreasurerName :: Maybe Text
      <*> arbitrary -- totalsCommitteeZip :: Maybe Text
    
instance Arbitrary TotalsCommitteePage where
  arbitrary =
    TotalsCommitteePage
      <$> arbitrary -- totalsCommitteePagePagination :: Maybe OffsetInfo
      <*> arbitrary -- totalsCommitteePageResults :: Maybe [TotalsCommittee]
    



instance Arbitrary E'CommitteeType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'CommitteeType2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'FilerType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner10 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner11 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner12 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner13 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner14 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner2 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner3 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner4 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner5 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner6 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner7 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner8 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Inner9 where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'MurType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Office where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'Renderer where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary E'SupportOppose where
  arbitrary = arbitraryBoundedEnum
