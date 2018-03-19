module Transaction
    where

data Transaction = Transaction {
      inputs :: [String]
    , outputs :: [String]
}

