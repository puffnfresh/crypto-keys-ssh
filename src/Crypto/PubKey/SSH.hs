module Crypto.PubKey.SSH (
  SshPubKeyError (..)
, getSshRsa
, decodeSshPubKey
) where

import           Control.Monad               (void)
import           Data.Bifunctor              (first)
import           Data.Binary.Get             (Get, getByteString, getWord32be,
                                              runGet)
import qualified Data.ByteString             as BS
import           Data.ByteString.Base64.Lazy (decode)
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Maybe                  (listToMaybe)

data SshPubKeyError
  = SshNotEnoughWords
  | SshInvalidBase64 String
  deriving (Show, Eq, Ord)

integerSize :: Integer -> Int
integerSize =
  (`div` 8) . length . takeWhile (/= 0) . iterate (`div` 2)

getInteger :: Get Integer
getInteger = do
  size <- getWord32be
  bs <- getByteString (fromIntegral size)
  pure (sum (zipWith (\c b -> fromIntegral c * 0x100 ^ b) (BS.unpack bs) [size - 1, size - 2..0]))

getSshRsa :: Get (Integer, Integer)
getSshRsa = do
  size <- getWord32be
  void (getByteString (fromIntegral size))
  e <- getInteger
  n <- getInteger
  pure (e, n)

decodeSshPubKey :: (Int -> Integer -> Integer -> a) -> LBS.ByteString -> Either SshPubKeyError a
decodeSshPubKey f bs = do
  bs' <- maybe (Left SshNotEnoughWords) Right (listToMaybe (drop 1 (LBS.words bs)))
  sshKey <- first SshInvalidBase64 (decode bs')
  let (e, n) = runGet getSshRsa sshKey
  pure (f (integerSize n) n e)
