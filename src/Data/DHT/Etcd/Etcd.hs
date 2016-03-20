module Data.DHT.Etcd.Etcd where

data Etcd = Etcd { restAddress :: String }
type Key = String

-- | etcd takes an address and returns an etcd object
etcd :: String -> Etcd
etcd = Etcd
