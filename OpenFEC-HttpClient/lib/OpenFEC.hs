{-
   OpenFEC

   This API allows you to explore the way candidates and committees fund their campaigns.    The FEC API is a RESTful web service supporting full-text and field-specific searches on FEC data. [Bulk downloads](https://www.fec.gov/data/advanced/?tab=bulk-data) are available on the current site. Information is tied to the underlying forms by file ID and image ID. Data is updated nightly.    There is a lot of data, but a good place to start is to use search to find interesting candidates and committees. Then, you can use their IDs to find report or line item details with the other endpoints. If you are interested in individual donors, check out contributor information in schedule_a.    Get an [API key here](https://api.data.gov/signup/). That will enable you to place up to 1,000 calls an hour. Each call is limited to 100 results per page. You can email questions, comments or a request to get a key for 120 calls per minute to [APIinfo@fec.gov](mailto:apiinfo@fec.gov). You can also ask questions and discuss the data in the [FEC data Google Group](https://groups.google.com/forum/#!forum/fec-data). API changes will also be added to this group in advance of the change.       The model definitions and schema are available at [/swagger](/swagger/). This is useful for making wrappers and exploring the data.    A few restrictions limit the way you can use FEC data. For example, you can’t use contributor lists for commercial purposes or to solicit donations. [Learn more here](https://transition.fec.gov/pages/brochures/saleuse.shtml).    [View our source code](https://github.com/fecgov/openFEC). We welcome issues and pull requests!

   OpenAPI spec version: 2.0
   OpenFEC API version: 1.0
   Generated by Swagger Codegen (https://github.com/swagger-api/swagger-codegen.git)
-}

{-|
Module : OpenFEC
-}

module OpenFEC
  (  module OpenFEC.API
  , module OpenFEC.Client
  , module OpenFEC.Core
  , module OpenFEC.Logging
  , module OpenFEC.MimeTypes
  , module OpenFEC.Model
  , module OpenFEC.ModelLens
  ) where

import OpenFEC.API
import OpenFEC.Client
import OpenFEC.Core
import OpenFEC.Logging
import OpenFEC.MimeTypes
import OpenFEC.Model
import OpenFEC.ModelLens