{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module ExploreFEC.Data.HouseRaces where

--import           Servant.API
import           Servant.Client

import qualified Data.Text      as T

import qualified OpenFEC.API    as FEC
import qualified OpenFEC.Types  as FEC

