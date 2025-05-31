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

{-# LANGUAGE ScopedTypeVariables, OverloadedStrings #-}
module NetRoutines where

import Network.Socket
import Control.Concurrent.Chan
import Data.Time
import Control.Monad
import Data.Unique
import qualified Codec.Binary.Base64 as Base64 -- This was correct
import qualified Control.Exception as E
import System.Entropy
-----------------------------
import CoreTypes
import Utils
import Hashing (hashAddress) -- For IP hashing
import qualified Data.Text as T -- For ipHash type
import qualified Data.ByteString.Char8 as B -- For B.ByteString type of salt


acceptLoop :: Socket -> B.ByteString -> Chan CoreMessage -> IO () -- Added B.ByteString for serverSaltGlobal
acceptLoop servSock serverSaltGlobal coreMessageChan = E.bracket openHandle closeHandle (forever . f) -- Renamed 'chan'
    where
    f ch = E.try (Network.Socket.accept servSock) >>= \v -> case v of
      Left (e :: E.IOException) -> return ()
      Right (sock, sockAddr) -> do
        clientHost <- sockAddr2String sockAddr
        currentTime <- getCurrentTime
        sendChan' <- newChan
        uid <- newUnique
        clientSpecificSalt <- liftM Base64.encode $ hGetEntropy ch 18 -- Renamed for clarity

        -- Hash the IP address
        maybeHashedIp <- hashAddress serverSaltGlobal sockAddr -- Use the passed server-wide salt

        let newClient = ClientInfo {
                clUID = uid,
                sendChan = sendChan',
                clientSocket = sock,
                host = clientHost,             -- Store raw IP for now, server-internal use only
                connectTime = currentTime,
                nick = "",                     -- Empty string
                webPassword = "",              -- Empty string
                serverSalt = clientSpecificSalt, -- This is the client-specific one from 'salt'
                logonPassed = False,
                isVisible = False,
                clientProto = 0,
                pingsQueue = 0,
                isMaster = False,
                isReady = False,
                isInGame = False,
                isAdministrator = False,
                hasSuperPower = False,
                isChecker = False,
                isContributor = False,
                isKickedFromServer = False,
                isJoinedMidGame = False,
                hasAskedList = False,
                clientClan = Nothing,
                checkInfo = Nothing,
                eiLobbyChat = newEventsInfo,
                eiEM = newEventsInfo,
                eiJoin = newEventsInfo,
                teamsInGame = 0,
                teamIndexes = [],
                pendingActions = [],
                ipHash = maybeHashedIp         -- Populate new field
            }

        writeChan coreMessageChan $ Accept newClient -- Use renamed coreMessageChan
        return ()
