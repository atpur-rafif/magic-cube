module Util (encodeOptions) where

import Data.Aeson
  ( Options (omitNothingFields, sumEncoding),
    SumEncoding (TaggedObject, contentsFieldName, tagFieldName),
    defaultOptions,
  )

encodeOptions :: Options
encodeOptions =
  defaultOptions
    { omitNothingFields = True,
      sumEncoding =
        TaggedObject
          { tagFieldName = "type",
            contentsFieldName = "properties"
          }
    }
