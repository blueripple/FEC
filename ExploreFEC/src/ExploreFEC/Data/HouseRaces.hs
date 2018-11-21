{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module ExploreFEC.Data.HouseRaces where

import           Servant.API
import           Servant.Client

import           Data.Text          (Text)

import qualified OpenFEC.API        as FEC
import qualified OpenFEC.QueryTypes as QT
