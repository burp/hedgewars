{-
 * Hedgewars, a free turn based strategy game
 * Copyright (c) 2004-2015 Andrey Korotaev <unC0Rr@gmail.com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; version 2 of the License
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 \-}

{-# LANGUAGE RankNTypes #-}
module ConfigFile where

import Data.Maybe
import Data.TConfig
import qualified Data.ByteString.Char8 as B
import qualified Codec.Binary.Base64.String as B64 -- For Base64 encoding/decoding
import System.Entropy (getEntropy) -- For generating random salt
import Control.Monad (liftM) -- For older GHC versions, often used with Applicative/Monad
-------------------
import CoreTypes

cfgFileName :: String
cfgFileName = "hedgewars-server.ini"


readServerConfig :: ServerInfo -> IO ServerInfo
readServerConfig serverInfo' = do
    cfg <- readConfig cfgFileName

    -- Read serverWideSalt
    let loadedSaltStr = getValue "serverWideSalt" cfg
    finalSalt <- case loadedSaltStr of
        Just s | not (null s) ->
            case B64.decode s of
                Left err -> do
                    putStrLn $ "Warning: Failed to decode serverWideSalt from config: " ++ err ++ ". Generating a new one."
                    getEntropy 32
                Right decodedSaltBytes -> return (B.pack decodedSaltBytes)
        _ -> do
            putStrLn "serverWideSalt not found in config or is empty. Generating a new one."
            getEntropy 32

    let si = serverInfo'{
        dbHost = value "dbHost" cfg (dbHost serverInfo')
        , dbName = value "dbName" cfg (dbName serverInfo')
        , dbLogin = value "dbLogin" cfg (dbLogin serverInfo')
        , dbPassword = value "dbPassword" cfg (dbPassword serverInfo')
        , serverMessage = value "sv_message" cfg (serverMessage serverInfo')
        , serverMessageForOldVersions = value "sv_messageOld" cfg (serverMessageForOldVersions serverInfo')
        , bans = fromMaybe (bans serverInfo') (getValue "bans" cfg >>= maybeRead) -- Using fromMaybe for safer parsing
        , latestReleaseVersion = fromMaybe (latestReleaseVersion serverInfo') (getValue "sv_latestProto" cfg >>= maybeRead) -- Using fromMaybe
        , serverConfig = Just cfg
        , serverWideSalt = finalSalt -- Store the loaded or generated salt
    }
    return si
    where
        -- Modified value helper to take a default ByteString from serverInfo'
        value n c defBS = B.pack $ fromMaybe (B.unpack defBS) (getValue n c)
        -- fromJust2 is no longer used directly for main fields, using fromMaybe now
        -- fromJust2 n Nothing = error $ "Missing config entry " ++ n -- Kept if used by other parts not modified
        -- fromJust2 _ (Just a) = a
        maybeRead :: Read a => String -> Maybe a
        maybeRead s = case reads s of
                          [(x, "")] -> Just x
                          _         -> Nothing


writeServerConfig :: ServerInfo -> IO ()
writeServerConfig ServerInfo{serverConfig = Nothing} = return ()
writeServerConfig ServerInfo{
    dbHost = dh,
    dbName = dn,
    dbLogin = dl,
    dbPassword = dp,
    serverMessage = sm,
    serverMessageForOldVersions = smo,
    bans = b,
    latestReleaseVersion = ver,
    serverWideSalt = swSalt, -- Get the salt
    serverConfig = Just cfg}
        =
    writeConfig cfgFileName $ foldl1 (.) entries cfg
    where
        -- Encode salt to Base64 String before saving
        encodedSaltStr = B64.encode $ B.unpack swSalt
        entries =
            repConfig "sv_latestProto" (show ver)
            : repConfig "bans" (show b)
            : repConfig "serverWideSalt" encodedSaltStr -- Add salt to entries
            : map (\(n, v) -> repConfig n (B.unpack v)) [
            ("dbHost", dh)
            , ("dbName", dn)
            , ("dbLogin", dl)
            , ("dbPassword", dp)
            , ("sv_message", sm)
            , ("sv_messageOld", smo)
            ]
