{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import OpenFEC.Model
import OpenFEC.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 5) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy AuditCandidateSearch)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCandidateSearchList)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCase)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCaseCategoryRelation)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCaseCategoryRelationPage)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCasePage)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCaseSubCategory)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCaseSubCategoryPage)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCategory)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCategoryPage)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCategoryRelation)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCategoryRelationPage)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCommitteeSearch)
      propMimeEq MimeJSON (Proxy :: Proxy AuditCommitteeSearchList)
      propMimeEq MimeJSON (Proxy :: Proxy AuditPrimaryCategory)
      propMimeEq MimeJSON (Proxy :: Proxy AuditPrimaryCategoryPage)
      propMimeEq MimeJSON (Proxy :: Proxy BaseF3Filing)
      propMimeEq MimeJSON (Proxy :: Proxy BaseF3FilingPage)
      propMimeEq MimeJSON (Proxy :: Proxy BaseF3PFiling)
      propMimeEq MimeJSON (Proxy :: Proxy BaseF3PFilingPage)
      propMimeEq MimeJSON (Proxy :: Proxy BaseF3XFiling)
      propMimeEq MimeJSON (Proxy :: Proxy BaseF3XFilingPage)
      propMimeEq MimeJSON (Proxy :: Proxy CalendarDate)
      propMimeEq MimeJSON (Proxy :: Proxy CalendarDatePage)
      propMimeEq MimeJSON (Proxy :: Proxy Candidate)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateCommitteeTotalsHouseSenate)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateCommitteeTotalsHouseSenatePage)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateCommitteeTotalsPresidential)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateCommitteeTotalsPresidentialPage)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateDetail)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateDetailPage)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateFlags)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateFlagsPage)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateHistory)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateHistoryPage)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateHistoryTotal)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateHistoryTotalPage)
      propMimeEq MimeJSON (Proxy :: Proxy CandidatePage)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateSearch)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateSearchList)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateTotal)
      propMimeEq MimeJSON (Proxy :: Proxy CandidateTotalPage)
      propMimeEq MimeJSON (Proxy :: Proxy Committee)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeDetail)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeDetailPage)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeHistory)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeHistoryPage)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteePage)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeReports)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeReportsHouseSenate)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeReportsHouseSenatePage)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeReportsIEOnly)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeReportsIEOnlyPage)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeReportsPacParty)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeReportsPacPartyPage)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeReportsPage)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeReportsPresidential)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeReportsPresidentialPage)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeSearch)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeSearchList)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeTotals)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeTotalsHouseSenate)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeTotalsHouseSenatePage)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeTotalsIEOnly)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeTotalsIEOnlyPage)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeTotalsPacParty)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeTotalsPacPartyPage)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeTotalsPage)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeTotalsPresidential)
      propMimeEq MimeJSON (Proxy :: Proxy CommitteeTotalsPresidentialPage)
      propMimeEq MimeJSON (Proxy :: Proxy CommunicationCost)
      propMimeEq MimeJSON (Proxy :: Proxy CommunicationCostByCandidate)
      propMimeEq MimeJSON (Proxy :: Proxy CommunicationCostByCandidatePage)
      propMimeEq MimeJSON (Proxy :: Proxy CommunicationCostPage)
      propMimeEq MimeJSON (Proxy :: Proxy EFilings)
      propMimeEq MimeJSON (Proxy :: Proxy EFilingsPage)
      propMimeEq MimeJSON (Proxy :: Proxy EfilingsAmendments)
      propMimeEq MimeJSON (Proxy :: Proxy EfilingsAmendmentsPage)
      propMimeEq MimeJSON (Proxy :: Proxy Election)
      propMimeEq MimeJSON (Proxy :: Proxy ElectionDate)
      propMimeEq MimeJSON (Proxy :: Proxy ElectionDatePage)
      propMimeEq MimeJSON (Proxy :: Proxy ElectionPage)
      propMimeEq MimeJSON (Proxy :: Proxy ElectionSearch)
      propMimeEq MimeJSON (Proxy :: Proxy ElectionSearchPage)
      propMimeEq MimeJSON (Proxy :: Proxy ElectionSummary)
      propMimeEq MimeJSON (Proxy :: Proxy Electioneering)
      propMimeEq MimeJSON (Proxy :: Proxy ElectioneeringByCandidate)
      propMimeEq MimeJSON (Proxy :: Proxy ElectioneeringByCandidatePage)
      propMimeEq MimeJSON (Proxy :: Proxy ElectioneeringPage)
      propMimeEq MimeJSON (Proxy :: Proxy ElectionsList)
      propMimeEq MimeJSON (Proxy :: Proxy ElectionsListPage)
      propMimeEq MimeJSON (Proxy :: Proxy EntityReceiptDisbursementTotals)
      propMimeEq MimeJSON (Proxy :: Proxy EntityReceiptDisbursementTotalsPage)
      propMimeEq MimeJSON (Proxy :: Proxy Filings)
      propMimeEq MimeJSON (Proxy :: Proxy FilingsPage)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1AdminFines)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1Adrs)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1AdvisoryOpinions)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1AoCitations)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1Citations)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1CommissionVotes)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1Dispositions)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1Documents)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1Documents1)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1Entities)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1Murs)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1Participants)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1Regulations)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1RegulatoryCitations)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1Statutes)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault1StatutoryCitations)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault2)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault3)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault3Results)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault4)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault4Results)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault5)
      propMimeEq MimeJSON (Proxy :: Proxy InlineResponseDefault5Results)
      propMimeEq MimeJSON (Proxy :: Proxy OffsetInfo)
      propMimeEq MimeJSON (Proxy :: Proxy OperationsLog)
      propMimeEq MimeJSON (Proxy :: Proxy OperationsLogPage)
      propMimeEq MimeJSON (Proxy :: Proxy RadAnalyst)
      propMimeEq MimeJSON (Proxy :: Proxy RadAnalystPage)
      propMimeEq MimeJSON (Proxy :: Proxy ReportDate)
      propMimeEq MimeJSON (Proxy :: Proxy ReportDatePage)
      propMimeEq MimeJSON (Proxy :: Proxy ReportType)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleA)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAByEmployer)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAByEmployerPage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAByOccupation)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAByOccupationPage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleABySize)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleABySizeCandidate)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleABySizeCandidatePage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleABySizePage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAByState)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAByStateCandidate)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAByStateCandidatePage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAByStatePage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAByStateRecipientTotals)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAByStateRecipientTotalsPage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAByZip)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAByZipPage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAEfile)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAEfilePage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleAPage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleB)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleBByPurpose)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleBByPurposePage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleBByRecipient)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleBByRecipientID)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleBByRecipientIDPage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleBByRecipientPage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleBEfile)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleBEfilePage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleBPage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleE)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleEByCandidate)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleEByCandidatePage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleEEfile)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleEEfilePage)
      propMimeEq MimeJSON (Proxy :: Proxy ScheduleEPage)
      propMimeEq MimeJSON (Proxy :: Proxy SeekInfo)
      propMimeEq MimeJSON (Proxy :: Proxy StateElectionOfficeInfo)
      propMimeEq MimeJSON (Proxy :: Proxy StateElectionOfficeInfoPage)
      propMimeEq MimeJSON (Proxy :: Proxy TotalsCommittee)
      propMimeEq MimeJSON (Proxy :: Proxy TotalsCommitteePage)
      
