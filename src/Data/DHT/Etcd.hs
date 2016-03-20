module Data.DHT.Etcd
( Etcd
, Key
, etcd
, getFlexiRaw
, getFlexi
, putFlexi
, putFlexiRaw
, getRaw
, get
, putRaw
, put
, EtcdValue(..)
, EtcdPrevValue(..)
, EtcdModifiedIndex(..) ) where

import Data.DHT.Etcd.Etcd
import Data.DHT.Etcd.Parsers
import Data.DHT.Etcd.Basic
import Data.DHT.Etcd.Internal

import Data.Aeson
import Data.String

-- | getRaw queries an Etcd instance for the value stored at the specified key.
--   it does no parsing, and returns the entire body sent as a result of the HTTP
--   GET request as a String
getRaw :: Etcd -> Key -> IO String
getRaw etcd key = getFlexiRaw etcd key

-- | get queries an etcd instance for the value stored at a key. The return value
--   is obtained by trying to parse with a FromJSON instance (either inferred, or
--   specified explicitly.
get :: (FromJSON a) => Etcd -> Key -> IO (Maybe a)
get etcd key = getRaw etcd key >>= extract


-- | putRaw places a value at a key, and returns the unparsed response from
--   etcd as a String.
putRaw :: Etcd -> Key -> String -> IO String
putRaw etcd key value = putFlexiRaw etcd key ("value=" ++ value)

-- | put stores a value at a key on an instance of Etcd. Its return type is
--   obtained by parsing the JSON to an inferred or specified type.
put :: (FromJSON a) => Etcd -> Key -> String -> IO (Maybe a)
put etcd key value = do
  putRaw etcd key value >>= extract
