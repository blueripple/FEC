--{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
--{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module OpenFEC.Types
  (
    module OpenFEC.Types
  , module OpenFEC.Beam.Types
  ) where

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
import           Data.Text                 (Text, pack, unpack)
import           Data.Time.Calendar        (Day)
import           Data.Time.Clock           (UTCTime)
import           Data.Time.Format          (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime       (LocalTime, utc, utcToLocalTime)
import           Data.Vector               (Vector, toList)
import           GHC.Generics              (Generic)
import qualified Text.PrettyPrint.Tabulate as PP
import           Text.Read                 (readMaybe)
import           Web.HttpApiData           (ToHttpApiData (..))


import           OpenFEC.JsonUtils
import qualified OpenFEC.Pagination        as FEC

import           OpenFEC.Beam.Types
-- All fields are prefixed with entity name followed by field name.
-- If the field name begins with entity name (e.g., "candidate_id" in Candidate) we begin with double underscore.
-- So, field name to json key is straightforward

--type CandidateID = Text
--type CommitteeID = Text
--type Name = Text
--type State = Text
--type District = Int
--type Amount = Scientific
type ElectionYear = Int
type ElectionCycle = Int


officeToText :: Office -> Text
officeToText House     = "H"
officeToText Senate    = "S"
officeToText President = "P"

instance A.ToJSON Office where
  toJSON = A.String . officeToText

instance A.FromJSON Office where
  parseJSON o = A.withText "Office" f o where
    f t = case t of
      "H" -> return House
      "S" -> return Senate
      "P" -> return President
      _   -> A.typeMismatch "Office" o

partyToText :: Party -> Text
partyToText Democrat        = "DEM"
partyToText Republican      = "REP"
partyToText Independent     = "IND"
partyToText WorkingFamilies = "WFP"
partyToText Conservative    = "CON"
partyToText Green           = "GRE"
partyToText Libertarian     = "LIB"
partyToText Other           = "OTH"
partyToText Unknown         = "UNK"

instance A.ToJSON Party where
  toJSON = A.String . partyToText

-- this one is used in Parsing from FEC data.  Maybe we can build a custom Prism? "Prism' Text Party" ?
instance A.FromJSON Party where
  parseJSON o = A.withText "Party" f o where
    f t = case t of
      "DEM" -> return Democrat
      "REP" -> return Republican
      "IND" -> return Independent
      "WFP" -> return WorkingFamilies
      "CON" -> return Conservative
      "GRE" -> return Green
      "LIB" -> return Libertarian
      "OTH" -> return Other
      "UNK" -> return Unknown
      _     -> return Other --A.typeMismatch "Party" o


instance A.FromJSON Candidate where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON Candidate where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}


--makeLenses ''Candidate

candidateFromResultJSON :: A.Value -> Either ByteString Candidate
candidateFromResultJSON val = Candidate
  <$> val |#| "candidate_id"
  <*> val |#| "name"
  <*> val |#| "office"
  <*> val |#| "state"
  <*> val |#| "district_number"
  <*> val |#| "party"


instance A.FromJSON Committee where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON Committee where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

--makeLenses ''Committee

committeeFromResultJSON :: A.Value -> Either ByteString Committee
committeeFromResultJSON val = Committee
  <$> val |#| "committee_id"
  <*> val |#| "designation_full"
  <*> val |#| "name"
  <*> val |#| "committee_type_full"
  <*> val |#| "committee_type"

committeeWithCandidatesFromResultJSON :: A.Value -> Either ByteString (Committee, [CandidateID])
committeeWithCandidatesFromResultJSON val = (,)
  <$> committeeFromResultJSON val
  <*> val |#| "candidate_ids"

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

--makeLenses ''Report

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


instance A.FromJSON Disbursement where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON Disbursement where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

--makeLenses ''Disbursement
sub_idFromText :: Either ByteString Text -> Either ByteString Int
sub_idFromText t =
  let f = either Left (maybe (Left "Error converting sub_id Text to Int") Right)
  in f $ readMaybe . unpack <$> t

disbursementFromResultJSON :: A.Value -> Either ByteString Disbursement
disbursementFromResultJSON val = Disbursement
  <$> tryTwoKeys "disbursement_date" "load_date" (utcToLocalTime utc) val -- val |#| "disbursement_date"
  <*> val |#| "disbursement_amount"
  <*> val |#| "disbursement_purpose_category"
  <*> val |#| "recipient_name"
  <*> val |#| "committee_id"
  <*> val |#| "line_number_label"
  <*> pure 0 -- sub_idFromText (val |#| "sub_id")

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


instance PP.CellValueFormatter LocalTime
instance PP.CellValueFormatter Scientific
instance PP.CellValueFormatter Text


instance A.FromJSON IndExpenditure where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON IndExpenditure where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}


instance PP.Tabulate IndExpenditure PP.DoNotExpandWhenNested

--makeLenses ''IndExpenditure

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
  <*> pure 0 -- sub_idFromText (val |#| "sub_id")


instance A.FromJSON PartyExpenditure where
  parseJSON = A.genericParseJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

instance A.ToJSON PartyExpenditure where
  toJSON = A.genericToJSON A.defaultOptions {A.fieldLabelModifier = drop 1}

--makeLenses ''PartyExpenditure

partyExpenditureFromResultJSON :: A.Value -> Either ByteString PartyExpenditure
partyExpenditureFromResultJSON val = PartyExpenditure
  <$> fmap (utcToLocalTime utc) (val |#| "expenditure_date")
  <*> val |#| "expenditure_amount"
  <*> val |#| "expenditure_purpose_full"
  <*> tryKeys ["committee","committee_id"] val -- val |#| "committee_id"
  <*> tryKeys ["committee","name"] val -- val |#| "committee_name"
  <*> pure 0 --sub_idFromText (val |#| "sub_id")
