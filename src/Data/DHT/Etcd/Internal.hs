module Data.DHT.Etcd.Internal where

import Data.String
import Data.Aeson

-- | extract is an internal utility function for parsing JSON data from
--   etcd responses (hence its ridiculously restrictive type)
extract :: (FromJSON a) => String -> IO (Maybe a)
extract = return . decode . fromString
