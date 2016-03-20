{-# LANGUAGE OverloadedStrings #-}
module Data.DHT.Etcd.Parsers where

import Data.Aeson
import Control.Applicative

data EtcdValue = EtcdValue { etcdValue :: String } deriving (Show)
instance FromJSON EtcdValue where
  parseJSON (Object v) = EtcdValue <$> (v .: "node" >>= (.: "value"))
  parseJSON _ = empty

data EtcdPrevValue = EtcdPrevValue { etcdPrevValue :: String } deriving (Show)
instance FromJSON EtcdPrevValue where
  parseJSON (Object v) = EtcdPrevValue <$> (v .: "prevNode" >>= (.: "value"))
  parseJSON _ = empty

data EtcdModifiedIndex = EtcdModifiedIndex { etcdModifiedIndex :: Integer } deriving (Show)
instance FromJSON EtcdModifiedIndex where
  parseJSON (Object v) = EtcdModifiedIndex <$> (v .: "node" >>= (.: "modifiedIndex"))
  parseJSON _ = empty
