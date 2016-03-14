{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
module Data.DHT.Etcd where

import Network.HTTP
import Network.URI (parseURI)
import Data.Aeson
import Data.String

import Control.Applicative

data Etcd = Etcd { restAddress :: String }
type Key = String

data EtcdValue = EtcdValue { etcdValue :: String } deriving (Show)
instance FromJSON EtcdValue where
  parseJSON (Object v) = EtcdValue <$> (v .: "node" >>= (.: "value"))

data EtcdPrevValue = EtcdPrevValue { etcdPrevValue :: String } deriving (Show)
instance FromJSON EtcdPrevValue where
  parseJSON (Object v) = EtcdPrevValue <$> (v .: "prevNode" >>= (.: "value"))

-- | extract is an internal utility function for parsing JSON data from
--   etcd responses (hence its ridiculously restrictive type)
extract :: (FromJSON a) => String -> IO (Maybe a)
extract = return . decode . fromString

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


-- | getFlexiRaw allows for more specific get requests
getFlexiRaw :: Etcd -> String -> IO String
getFlexiRaw Etcd{..} urlext = simpleHTTP (getRequest $ "http://" ++ restAddress ++ "/v2/keys" ++ urlext) >>= getResponseBody

getFlexi :: (FromJSON a) => Etcd -> String -> IO (Maybe a)
getFlexi etcd key = getFlexiRaw etcd key >>= extract

-- | putRequest builds a put request for storing a particular value at a key
putRequest :: String -> String -> Maybe (Request String)
putRequest place what = let uri = parseURI place
                            empty = fmap (mkRequest PUT) uri in
                        case empty of
                          Just req -> return $ setRequestBody req ("application/x-www-form-urlencoded", what)
                          Nothing -> Nothing


-- | putRaw places a value at a key, and returns the unparsed response from
--   etcd as a String.
putRaw :: Etcd -> Key -> String -> IO String
putRaw etcd key value = putFlexiRaw etcd key ("value=" ++ value)

-- | put stores a value at a key on an instance of Etcd. Its return type is
--   obtained by parsing the JSON to an inferred or specified type.
put :: (FromJSON a) => Etcd -> Key -> String -> IO (Maybe a)
put etcd key value = do
  putRaw etcd key value >>= extract


-- | etcd has a range of different commands, mostly based around PUT requests.
--   putFlexRaw allows the body of a PUT request to be specified, as well as any
--   extensions to the URL (for example used in compare and swap operations). The
--   extension must be specified explicitly, from the root etcd address. For example
--   to put the value "bar" at the key "/foo", use
--
--       putFlexiRaw etcd "/foo" "value=bar"
--
--   This is exposed to make the library easy to extend. Other functions should be used
--   in place of this if they are available, but if they are not, this is the do
--   everything PUT request.
putFlexiRaw :: Etcd -> String -> String -> IO String
putFlexiRaw Etcd{..} urlext body = do
  let url = "http://" ++ restAddress ++ "/v2/keys" ++ urlext
      mreq = putRequest url $ body
  case mreq of
    Just req -> simpleHTTP req >>= getResponseBody
    Nothing -> return "Couldn't form a valid put request"

-- | putFlexi is like putFlexiRaw but parses the return value in some way.
putFlexi :: (FromJSON a) => Etcd -> String -> String -> IO (Maybe a)
putFlexi etcd urlext body = do
  putFlexiRaw etcd urlext body >>= extract
