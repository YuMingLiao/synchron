{-# LANGUAGE DeriveFunctor #-}

type Config = Config' String
data Config' a = Config
  { field1 :: a
  , field2 :: a
  , field3 :: a
  } deriving (Functor, Show)

configHeads :: Config -> Config' Char
configHeads = fmap head


what's the difference between barbie and https://stackoverflow.com/questions/24922375/map-identity-functor-over-record? 

