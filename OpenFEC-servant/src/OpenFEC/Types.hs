{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}

module OpenFEC.Types where

import GHC.Generics (Generic)
import           Control.Lens       ((^?))
import Control.Lens.TH (makeLenses)
import qualified Data.Aeson         as A
import           Data.Aeson.Lens
import qualified Text.Tabl as TT
import           Data.Text          (Text, pack)
import qualified Data.Foldable as F
import Data.Scientific (Scientific, formatScientific, Fixed)
import Data.Time.Calendar (Day)
import Data.Time.Format (formatTime, defaultTimeLocale)
  
import qualified OpenFEC.QueryTypes as QT

type CandidateID = Text
type Name = Text
type State = Text
type District = Integer

data Candidate = Candidate
  {
    _candidate_id       :: CandidateID
  , _candidate_name     :: Name
  , _candidate_state    :: State
  , _candidate_district :: District
  , _candidate_party    :: QT.Party
  } deriving (Show, Generic)

-- we don't really want to and from JSON of this thing so much as from the candidate query result to this

makeLenses ''Candidate

candidateFromResultJSON :: A.Value -> Maybe Candidate
candidateFromResultJSON val =
  let idM = val ^? key "candidate_id" . _String
      nameM = val ^? key "name" . _String
      stateM = val ^? key "state" . _String
      districtM = val ^? key "district_number" . _Integer
      partyM = val ^? key "party" . _JSON -- this is...magic
  in Candidate <$> idM <*> nameM <*> stateM <*> districtM <*> partyM

candidateHeaders :: [Text]
candidateHeaders = ["ID","Name","State","District","Party"]

candidateAligns = [TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft]

candidateToRow :: Candidate -> [Text]
candidateToRow (Candidate id n s d p) = [id, n, s, pack (show d), pack (show p)] 

candidateTable :: (Functor t, Foldable t) => t Candidate -> Text
candidateTable x =
  let cs = F.toList $ candidateToRow <$> x
  in  TT.tabl TT.EnvAscii TT.DecorAll TT.DecorNone candidateAligns (candidateHeaders : cs) 


data Filing = Filing
  {
    _filing_cash_on_hand_beginning :: Scientific
  , _filing_cash_on_hand_end :: Scientific
  , _filing_committee_type :: Text
  , _filing_committee_id :: Text
  , _filing_document_type :: Text
  , _filing_receipt_date :: Day
  , _filing_report_type :: Text
  , _filing_total_communication_cost :: Scientific
  , _filing_total_disbursements :: Scientific
  , _filing_total_independent_expenditures :: Scientific
  , _filing_total_individual_contributions :: Scientific
  , _filing_total_receipts :: Scientific
  , _filing_update_date :: Day
  }

makeLenses ''Filing

sciAsMoney :: Scientific -> Text
sciAsMoney = formatScientific Fixed (Just 2) 

dayToText :: Day -> Text
dayToText = T.pack . formatTime defaultTimeLocale "%F" 

filingFromResultJSON :: A.Value -> Maybe Filing
filinfFromResultJSON val =
  let cohb = val ^? key "cash_on_hand_beginning" . _Number
      cohe = val ^? key "cash_on_hand_end" . _Number
      ct = val ^? "committee_type" . _String
      ci = val ^? "committee_id" . _String
      dt = val ^? "document_type" . _String
      rd = val ^? "receipt_date" . _AsJSON
      rt = val ^? "report_type" . _String
      ttc = val ^? "total_communication_cost" . _Number
      td = val ^? "total_disbursements" . _Number
      tie = val ^? "total_independent_expenditures" . _Number
      tic = val ^? "total_individual_contributions" . _Number
      tr = val ^? "total_receipts" . _Number
      ud = val ^? "update_date" . _AsJSON
  in Filing <$> cohb <*> cohe <*> ct <*> ci <*> dt <*> rd <*> rt <*> ttc <*> td <*> tie <*> tic <*> tr <*> ud 

filingHeaders :: [Text]
filingHeaders = ["$ Beginning", "$ End", "committee type", "committee id", "doc type", "receipt date",
                "report type", "$ communications", "$ disbursements", "$ ind expenditures" ,
                "$ individual contributions", "$ receipts", "update"]

                
filingAligns = [TT.AlignRight, TT.AlignRight, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft, TT.AlignLeft,
                TT.AlignLeft, TT.AlignRight, TT.AlignRight, TT.AlignRight, TT.AlignRight, TT.AlignRight,
                TT.AlignLeft]

filingToRow :: Filing -> [Text]
filingToRow (Filing cohb cohe ct ci dt rd rt ttc td tie tic tr ud) =
  [
    sciAsMoney cohb
  , sciAsMoney cohe
  , ct
  , ci
  , dt
  , dayToText rd
  , rt
  , sciAsMoney ttc
  , sciAsMoney td
  , sciAsMoney tie
  , sciAsMoney tic
  , sciAsMoney tr
  , dayToText ud
  ]

  
   
filingTable :: (Functor t, Foldable t) => t Filing -> Text
filingTable x =
  let fs = F.toList $ filingToRow <$> x
  in TT.tabl TT.EnvAscii TT.DecorAll TT.DecorNone filingAligns (filingHeaders : fs) 
