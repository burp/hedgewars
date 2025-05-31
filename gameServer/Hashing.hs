{-
 * Hedgewars, a free turn based strategy game
 * This module provides IP hashing functionality for the server.
-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Hashing (hashAddress) where

import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Crypto.Hash (Digest, SHA256, hash)
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import Network.Socket (SockAddr, getNameInfo, NameInfoFlag(NI_NUMERICHOST))
-- Control.Monad.IO.Class (liftIO) is not used with try
import Control.Exception (try, SomeException)


-- | Extracts the IP address string from a SockAddr.
-- Similar to sockAddr2String in Utils.hs but returns IO Text for direct use.
sockAddrToIpText :: SockAddr -> IO (Maybe T.Text)
sockAddrToIpText sockAddr = do
    result <- try (getNameInfo [NI_NUMERICHOST] True False sockAddr)
    case result of
        Left (_ :: SomeException) -> return Nothing -- Handle cases where getNameInfo might fail
        Right (Just host, _) -> return $ Just (T.pack host)
        Right (Nothing, _) -> return Nothing

-- | Hashes an IP address from a SockAddr using SHA256 with a provided salt.
-- The result is a hex-encoded Text.
hashAddress :: B.ByteString -> SockAddr -> IO (Maybe T.Text)
hashAddress salt sockAddr = do
    maybeIpStr <- sockAddrToIpText sockAddr
    case maybeIpStr of
        Nothing -> return Nothing -- Could not extract IP from SockAddr
        Just ipStr ->
            -- Convert salt and IP to ByteString (ensure salt is ByteString)
            let ipBytes = TE.encodeUtf8 ipStr -- IP as UTF-8 ByteString
                -- Salt is already ByteString
                saltedIpBytes = salt `B.append` ipBytes
                -- Compute SHA256 hash
                hashDigest :: Digest SHA256
                hashDigest = hash saltedIpBytes
                -- Convert hash to hex-encoded ByteString, then to Text
                hexHash = T.toLower $ TE.decodeUtf8 $ convertToBase Base16 hashDigest
            in return $ Just hexHash
