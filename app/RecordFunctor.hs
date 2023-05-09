{-# LANGUAGE DeriveFunctor #-}

type Config = Config' String
data Config' a = Config
  { field1 :: a
  , field2 :: a
  , field3 :: a
  } deriving (Functor, Show)

configHeads :: Config -> Config' Char
configHeads = fmap head
