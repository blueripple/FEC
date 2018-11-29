--{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
module OpenFEC.Types where

import           Control.Lens
import           Control.Lens.TH           (makeLenses)
import           Control.Monad             (join, sequence)
import qualified Data.Aeson                as A
import           Data.Aeson.Lens
import qualified Data.Aeson.Types          as A
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as BS
import           Data.Data                 (Data)
import           Data.Foldable             (foldl')
import qualified Data.Foldable             as F
import           Data.Scientific           (FPFormat (Fixed), Scientific,
                                            formatScientific)
import           Data.Text                 (Text, pack)
import           Data.Time.Calendar        (Day)
import           Data.Time.Clock           (UTCTime)
import           Data.Time.Format          (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime       (LocalTime, utc, utcToLocalTime)
import           Data.Vector               (Vector, toList)
import           GHC.Generics              (Generic)
import qualified Text.PrettyPrint.Tabulate as PP
import qualified Text.Tabl                 as TT
import           Web.HttpApiData           (ToHttpApiData (..))

import           OpenFEC.JsonUtils
import qualified OpenFEC.QueryTypes        as FEC

-- All fields are prefixed with entity name followed by field name.
-- If the field name begins with entity name (e.g., "candidate_id" in Candidate) we begin with double underscore.
-- So, field name to json key is straightforward

type CandidateID = Text
type CommitteeID = Text
type Name = Text
type State = Text
type District = Int
type Amount = Scientific

data Candidate = Candidate
  {
    _candidate_id       :: CandidateID
  , _candidate_name     :: Name
  , _candidate_state    :: State
  , _candidate_district :: District
  , _candidate_party    :: FEC.Party
  } deriving (Show, Generic)

instance A.FromJSON Candidate where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON Candidate where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

makeLenses ''Candidate

candidateFromResultJSON :: A.Value -> Either ByteString Candidate
candidateFromResultJSON val = Candidate
  <$> val |#| "candidate_id"
  <*> val |#| "name"
  <*> val |#| "state"
  <*> val |#| "district_number"
  <*> val |#| "party"


candidateHeaders :: [Text]
candidateHeaders = ["ID","Name","State","District","Party"]

candidateAligns = [TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft]

candidateToRow :: Candidate -> [Text]
candidateToRow (Candidate id n s d p) = [id, n, s, pack (show d), pack (show p)]

candidateTable :: (Functor t, Foldable t) => t Candidate -> Text
candidateTable x =
  let cs = F.toList $ candidateToRow <$> x
  in  TT.tabl TT.EnvAscii TT.DecorAll TT.DecorNone candidateAligns (candidateHeaders : cs)

data Committee = Committee
  {
    _committee_id         :: CommitteeID
  , _committee_designaton :: Text
  , _committee_name       :: Text
  , _committee_type_full  :: Text
  , _committee_type       :: Text -- we should make this its own type, prolly
  } deriving (Generic, Show)

instance A.FromJSON Committee where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON Committee where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}


makeLenses ''Committee

committeeFromResultJSON :: A.Value -> Either ByteString Committee
committeeFromResultJSON val = Committee
  <$> val |#| "committee_id"
  <*> val |#| "designation_full"
  <*> val |#| "name"
  <*> val |#| "committee_type_full"
  <*> val |#| "committee_type"

committeeHeaders :: [Text]
committeeHeaders = ["Committee ID","Designation","Name","Type Full", "Type"]

committeeAligns = [TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft]

committeeToRow :: Committee -> [Text]
committeeToRow (Committee id d n tf t) = [id, d, n, tf, t]

committeeTable :: (Functor t, Foldable t) => t Committee -> Text
committeeTable x =
  let cs = F.toList $ committeeToRow <$> x
  in  TT.tabl TT.EnvAscii TT.DecorAll TT.DecorNone committeeAligns (committeeHeaders : cs)

amountToText :: Amount -> Text
amountToText = pack . formatScientific Fixed (Just 2)

dayToText :: Day -> Text
dayToText = pack . formatTime defaultTimeLocale "%F"

localTimeToText :: LocalTime -> Text
localTimeToText = pack . formatTime defaultTimeLocale "%F"

utcTimeToText :: UTCTime -> Text
utcTimeToText = pack . formatTime defaultTimeLocale "%F"

data Report = Report
  {

    _report_total_receipts_period         :: Amount
  , _report_total_receipts_ytd            :: Amount
  , _report_total_contributions_period    :: Amount
  , _report_total_contributions_ytd       :: Amount
  , _report_total_disbursements_period    :: Amount
  , _report_total_disbursements_ytd       :: Amount
  , _report_cash_on_hand_beginning_period :: Amount
  , _report_receipt_date                  :: LocalTime
  } deriving (Generic, Show)

instance A.FromJSON Report where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON Report where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

makeLenses ''Report

reportFromResultJSON :: A.Value -> Either ByteString Report
reportFromResultJSON val = Report
  <$> val |#| "total_receipts_period"
  <*> val |#| "total_receipts_ytd"
  <*> val |#| "total_contributions_period"
  <*> val |#| "total_contributions_ytd"
  <*> val |#| "total_disbursements_period"
  <*> val |#| "total_disbursements_ytd"
  <*> val |#| "cash_on_hand_beginning_period"
  <*> val |#| "receipt_date"

reportHeaders :: [Text]
reportHeaders = ["Receipts", "Receipts (YTD)", "Contributions", "Contributions (YTD)", "Disbursements", "Disbursements (YTD)",
                "Cash On Hand", "Receipt Date"]


reportAligns = [TT.AlignRight, TT.AlignRight, TT.AlignRight, TT.AlignRight, TT.AlignRight, TT.AlignRight,
                TT.AlignRight, TT.AlignRight]

reportToRow :: Report -> [Text]
reportToRow (Report trp try tcp tcy tdp tdy coh rd) =
  [
    amountToText trp
  , amountToText try
  , amountToText tcp
  , amountToText tcy
  , amountToText tdp
  , amountToText tdy
  , amountToText coh
  , localTimeToText rd
  ]

reportTable :: (Functor t, Foldable t) => t Report -> Text
reportTable x =
  let fs = F.toList $ reportToRow <$> x
  in TT.tabl TT.EnvAscii TT.DecorAll TT.DecorNone reportAligns (reportHeaders : fs)

data Disbursement = Disbursement
  {
    _disbursement_date              :: LocalTime
  , _disbursement_amount            :: Amount
  , _disbursement_purpose_Category  :: Text
  , _disbursement_recipient_name    :: Maybe Text
  , _disbursement_committee_id      :: CommitteeID
  , _disbursement_line_number_label :: Text
  } deriving (Generic, Show)


instance A.FromJSON Disbursement where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON Disbursement where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

makeLenses ''Disbursement

disbursementFromResultJSON :: A.Value -> Either ByteString Disbursement
disbursementFromResultJSON val = Disbursement
  <$> tryTwoKeys "disbursement_date" "load_date" (utcToLocalTime utc) val -- val |#| "disbursement_date"
  <*> val |#| "disbursement_amount"
  <*> val |#| "disbursement_purpose_category"
  <*> val |#| "recipient_name"
  <*> val |#| "committee_id"
  <*> val |#| "line_number_label"

disbursementHeaders :: [Text]
disbursementHeaders = ["Date", "Amount", "Purpose", "Recipient", "Committee ID", "Line Number Label"]

disbursementAligns = [TT.AlignLeft, TT.AlignRight, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft]

disbursementToRow :: Disbursement -> [Text]
disbursementToRow (Disbursement d a pc rn ci lnl)  = [localTimeToText d, amountToText a, pc, maybe "N/A" id rn, ci, lnl]

disbursementTable :: (Functor t, Foldable t) => t Disbursement -> Text
disbursementTable x =
  let ds = F.toList $ disbursementToRow <$> x
  in TT.tabl TT.EnvAscii TT.DecorAll TT.DecorNone disbursementAligns (disbursementHeaders : ds)

data SpendingIntention = Support | Oppose deriving (Generic, Show, Eq, Data)
instance PP.CellValueFormatter SpendingIntention

instance A.ToJSON SpendingIntention where
  toJSON Support = A.String "S"
  toJSON Oppose  = A.String "O"

instance A.FromJSON SpendingIntention where
  parseJSON o = A.withText "SpendingIntention" f o where
    f t = case t of
      "S" -> return Support
      "O" -> return Oppose
      _   -> A.typeMismatch "SpendingIntention" o

data IndExpenditure = IndExpenditure
  {
    _indExpenditure_date                     :: LocalTime
  , _indExpenditure_amount                   :: Amount
  , _indExpenditure_amount_from_ytd          :: Amount
  , _indExpenditure_support_oppose_indicator :: SpendingIntention
  , _indExpenditure_office_total_ytd         :: Amount
  , _indExpenditure_category_code_full       :: Maybe Text
  , _indExpenditure_description              :: Maybe Text
  , _indExpenditure_candidate_id             :: CandidateID
  , _indExpenditure_committee_id             :: CommitteeID
  , _indExpenditure_committee_name           :: Text
  } deriving (Generic, Show, Eq)

instance PP.CellValueFormatter LocalTime
instance PP.CellValueFormatter Scientific
instance PP.CellValueFormatter Text

instance A.FromJSON IndExpenditure where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON IndExpenditure where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance PP.Tabulate IndExpenditure PP.DoNotExpandWhenNested

makeLenses ''IndExpenditure

indExpenditureFromResultJSON :: A.Value -> Either ByteString IndExpenditure
indExpenditureFromResultJSON val = IndExpenditure
  <$> val |#| "expenditure_date"
  <*> val |#| "expenditure_amount"
  <*> pure (0 :: Amount)
  <*> val |#| "support_oppose_indicator"
  <*> val |#| "office_total_ytd"
  <*> val |#| "category_code_full"
  <*> val |#| "expenditure_description"
  <*> tryKeys ["candidate","candidate_id"] val
  <*> val |#| "committee_id"
  <*> tryKeys ["committee","name"]  val

{-
indExpenditureHeaders :: [Text]
indExpenditureHeaders = ["Date", "Amount", "Intention", "YTD", "Category", "Description", "Candidate ID", "Committee ID", "Committee Name"]

indExpenditureAligns = [TT.AlignLeft, TT.AlignRight, TT.AlignLeft, TT.AlignRight, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft]

indExpenditureToRow :: IndExpenditure -> [Text]
indExpenditureToRow (IndExpenditure d a so ytd ccf ed cais coi cn)  =
  [localTimeToText d, amountToText a, pack (show so), amountToText ytd, maybe "N/A" id ccf, maybe "N/A" id ed, cais, coi, cn]

indExpenditureTable :: (Functor t, Foldable t) => t IndExpenditure -> Text
indExpenditureTable x =
  let ds = F.toList $ indExpenditureToRow <$> x
  in TT.tabl TT.EnvAscii TT.DecorAll TT.DecorNone indExpenditureAligns (indExpenditureHeaders : ds)
-}

data PartyExpenditure = PartyExpenditure
  {
    _partyExpenditure_date           :: UTCTime
  , _partyExpenditure_amount         :: Amount
  , _partyExpenditure_purpose_full   :: Text
  , _partyExpenditure_committee_id   :: CommitteeID
  , _partyExpenditure_committee_name :: Text
  } deriving (Generic, Show)

instance A.FromJSON PartyExpenditure where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON PartyExpenditure where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

makeLenses ''PartyExpenditure

partyExpenditureFromResultJSON :: A.Value -> Either ByteString PartyExpenditure
partyExpenditureFromResultJSON val = PartyExpenditure
  <$> val |#| "expenditure_date"
  <*> val |#| "expenditure_amount"
  <*> val |#| "expenditure_purpose_full"
  <*> tryKeys ["committee","committee_id"] val -- val |#| "committee_id"
  <*> tryKeys ["committee","name"] val -- val |#| "committee_name"

partyExpenditureHeaders :: [Text]
partyExpenditureHeaders = ["Date", "Amount", "Purpose", "Committee ID", "Committee Name"]

partyExpenditureAligns = [TT.AlignLeft, TT.AlignRight, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft]

partyExpenditureToRow :: PartyExpenditure -> [Text]
partyExpenditureToRow (PartyExpenditure d a pf ci cn)  = [utcTimeToText d, amountToText a, pf, ci, cn]

partyExpenditureTable :: (Functor t, Foldable t) => t PartyExpenditure -> Text
partyExpenditureTable x =
  let ds = F.toList $ partyExpenditureToRow <$> x
  in TT.tabl TT.EnvAscii TT.DecorAll TT.DecorNone partyExpenditureAligns (partyExpenditureHeaders : ds)
