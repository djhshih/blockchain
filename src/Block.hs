{-# LANGUAGE DeriveGeneric #-}

module Block
    where


import Data.Word
import Crypto.Hash (Digest, SHA256)
import qualified Crypto.Hash as H
import Data.ByteArray (convert)

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder

import Data.Serialize (Serialize)
import qualified Data.Serialize as S

import GHC.Generics (Generic)

import Data.Char (chr, ord)


type Hash = Digest SHA256

-- max integer value of hash: 256^32 - 1
-- max integer value of target: 2^32 - 1
difficultyStepSize = (256^32 - 1) `quot` (2^32 - 1)

data Block = Block {
    -- version (4)
      version ::  Word32
    -- hash of previous block (32)
    , previous :: ByteString
    -- root hash of the hash tree of transactions (32)
    , root :: ByteString
    -- block timestamp (4)
    , time :: Word32
    -- target threshold (4)
    , target :: Word32
    -- arbitrary number (4)
    , nounce :: Word32
    } deriving (Generic)

instance Serialize Block

instance Show Block where
    show b = "block v" ++ show (version b) ++ 
             " @" ++ show (time b) ++
             " {\n" ++
             "  previous: " ++ show (byteStringToInteger $ previous b) ++ "\n" ++
             "  target: " ++ show (expandTarget $ target b) ++ "\n" ++
             "  nounce: " ++ show (nounce b) ++ "\n" ++
             "  root: " ++ showHash (root b) ++
             "\n}" 

-- Create a genesis block.
new :: Block
new = Block {
      version = 1
    , previous = B.empty
    , root = B.empty
    , time = 0
    , target = 2^32 - 1
    , nounce = 0
    }

-- Hash a block.
hash :: Block -> ByteString
hash block = convert ((H.hash serialized) :: Hash)
  where
    serialized = S.encode block

-- encode hash in base16 for display
showHash :: ByteString -> String
showHash = (map (chr . fromIntegral)) . L.unpack . toLazyByteString . byteStringHex

-- expand target value from original space to full 32-byte space
expandTarget :: Word32 -> Integer
expandTarget x = difficultyStepSize * (fromIntegral x)

-- convert bytestring to an integer using the numeric value of the bytes
byteStringToInteger :: ByteString -> Integer
byteStringToInteger xs = f (B.unpack xs) 0
  where
    f :: [Word8] -> Integer -> Integer
    f [] b = b
    f (x:[]) b = b + fromIntegral x
    f (x:xs) b = f xs ((b + fromIntegral x) * 256)

-- Add a block to the front of a block chain
cons :: Block -> [Block] -> [Block]
cons block chain
    | null chain           =  consValid block chain
    | previous block == h  =  consValid block chain
    | otherwise            =  consValid block {previous = h} chain
      where
        h = hash $ head chain

-- Add a valid block
-- increment nounce until hash of block meets target requirement,
-- and then add block to the chain
consValid :: Block -> [Block] -> [Block]
consValid block chain =
    let 
        h = hash block
        itarget = (expandTarget . target) block
    in if byteStringToInteger h < itarget then
        block : chain
    else 
        consValid block {nounce = nounce block + 1} chain

