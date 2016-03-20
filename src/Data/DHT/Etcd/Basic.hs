{-# LANGUAGE RecordWildCards #-}
module Data.DHT.Etcd.Basic where

import Data.DHT.Etcd.Etcd
import Data.DHT.Etcd.Internal

import Network.HTTP
import Network.URI (parseURI)
import Data.Aeson

-- | getFlexiRaw allows for more specific get requests
getFlexiRaw :: Etcd -> String -> IO String
getFlexiRaw Etcd{..} urlext = simpleHTTP (getRequest $ "http://" ++ restAddress ++ "/v2/keys" ++ urlext) >>= getResponseBody

getFlexi :: (FromJSON a) => Etcd -> String -> IO (Maybe a)
getFlexi etcd key = getFlexiRaw etcd key >>= extract

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

-- | putRequest builds a put request for storing a particular value at a key
putRequest :: String -> String -> Maybe (Request String)
putRequest place what = let uri = parseURI place
                            empty = fmap (mkRequest PUT) uri in
                        case empty of
                          Just req -> return $ setRequestBody req ("application/x-www-form-urlencoded", what)
                          Nothing -> Nothing
