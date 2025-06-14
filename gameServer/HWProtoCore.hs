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

{-# LANGUAGE OverloadedStrings #-}
module HWProtoCore where

import Control.Monad
import Control.Monad.Reader
import Data.Maybe
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T -- Added for T.Text
import qualified Data.Text.Encoding as TE -- Added for TE.encodeUtf8
--------------------------------------
import CoreTypes
import HWProtoNEState
import HWProtoLobbyState
import HWProtoInRoomState
import HWProtoChecker
import HandlerUtils
import RoomsAndClients
import Utils
import Consts

handleCmd, handleCmd_loggedin, handleCmd_lobbyOnly, handleCmd_roomOnly :: CmdHandler


handleCmd ["PING"] = answerClient ["PONG"]


handleCmd ("QUIT" : xs) = return [ByeClient msg]
    where
        -- "bye" is a special string (do not translate!) when the user quits manually,
        -- otherwise there will be an additional server message
        msg = if not $ null xs then (head xs) else "bye"


handleCmd ["PONG"] = do
    cl <- thisClient
    if pingsQueue cl == 0 then
        return [ProtocolError "Protocol violation"]
        else
        return [ModifyClient (\c -> c{pingsQueue = pingsQueue c - 1})]

handleCmd cmd = do
    (ci, irnc) <- ask
    let cl = irnc `client` ci
    if logonPassed cl then
        if isChecker cl then
            handleCmd_checker cmd
            else
            handleCmd_loggedin cmd
        else
        handleCmd_NotEntered cmd

handleCmd_lobbyOnly cmd = do
    (ci, rnc) <- ask
    if (clientRoom rnc ci) == lobbyId then
        handleCmd cmd
    else
        return [Warning $ loc "This command is only available in the lobby."]

handleCmd_roomOnly cmd = do
    (ci, rnc) <- ask
    if (clientRoom rnc ci) == lobbyId then
        return [Warning $ loc "This command is only available in rooms."]
    else
        handleCmd cmd

-- Chat command handling
unknownCmdWarningText :: B.ByteString
unknownCmdWarningText = loc "Unknown command or invalid parameters. Say '/help' in chat for a list of commands."

handleCmd_loggedin ["CMD"] = return [Warning unknownCmdWarningText]

handleCmd_loggedin ["CMD", parameters] = uncurry h $ extractParameters parameters
    where
        -- room-only commands
        h "DELEGATE" n | not $ B.null n = handleCmd_roomOnly ["DELEGATE", n]
        h "SAVEROOM" n | not $ B.null n = handleCmd_roomOnly ["SAVEROOM", n]
        h "LOADROOM" n | not $ B.null n = handleCmd_roomOnly ["LOADROOM", n]
        h "SAVE" n | not $ B.null n = let (sn, ln) = B.break (== ' ') n in if B.null ln then return [Warning unknownCmdWarningText] else handleCmd_roomOnly ["SAVE", sn, B.tail ln]
        h "DELETE" n | not $ B.null n = handleCmd_roomOnly ["DELETE", n]
        h "FIX" _ = handleCmd_roomOnly ["FIX"]
        h "UNFIX" _ = handleCmd_roomOnly ["UNFIX"]
        h "GREETING" msg = handleCmd_roomOnly ["GREETING", msg]
        h "CALLVOTE" msg | B.null msg = handleCmd_roomOnly ["CALLVOTE"]
                         | otherwise = let (c, p) = extractParameters msg in
                                           if B.null p then handleCmd_roomOnly ["CALLVOTE", c] else handleCmd_roomOnly ["CALLVOTE", c, p]
        h "VOTE" msg | not $ B.null msg = handleCmd_roomOnly ["VOTE", upperCase msg]
                     | otherwise = handleCmd_roomOnly ["VOTE", ""]
        h "FORCE" msg | not $ B.null msg = handleCmd_roomOnly ["VOTE", upperCase msg, "FORCE"]
                      | otherwise = handleCmd_roomOnly ["VOTE", "", "FORCE"]
        h "MAXTEAMS" n | not $ B.null n = handleCmd_roomOnly ["MAXTEAMS", n]
                       | otherwise = handleCmd_roomOnly ["MAXTEAMS"]

        -- lobby-only commands
        h "STATS" _ = handleCmd_lobbyOnly ["STATS"]
        h "RESTART_SERVER" p = handleCmd_lobbyOnly ["RESTART_SERVER", upperCase p]

        -- room and lobby commands
        h "QUIT" _ = handleCmd ["QUIT"]
        h "RND" p = handleCmd ("RND" : B.words p)
        h "GLOBAL" p = serverAdminOnly $ do
            rnc <- liftM snd ask
            let chans = map (sendChan . client rnc) $ allClients rnc
            return [AnswerClients chans ["CHAT", nickGlobal, p]]
        h "WATCH" f = return [QueryReplay f]
        h "INFO" n | not $ B.null n = handleCmd ["INFO", n]
        h "HELP" _ = handleCmd ["HELP"]
        h "REGISTERED_ONLY" _ = serverAdminOnly $ do
            rnc <- liftM snd ask
            let chans = map (sendChan . client rnc) $ allClients rnc
            return
                [ModifyServerInfo(\s -> s{isRegisteredUsersOnly = not $ isRegisteredUsersOnly s})
                , ShowRegisteredOnlyState chans
                ]
        h "SUPER_POWER" _ = serverAdminOnly $ do
            cl <- thisClient
            return
                [ModifyClient (\c -> c{hasSuperPower = True})
                , AnswerClients [sendChan cl] ["CHAT", nickServer, loc "Super power activated."]
                ]
        h _ _ = return [Warning unknownCmdWarningText]


        extractParameters p = let (a, b) = B.break (== ' ') p in (upperCase a, B.dropWhile (== ' ') b)

handleCmd_loggedin ["INFO", asknick] = do
    (_, rnc) <- ask -- We need rnc to get client and room info
    -- isAdminAsking is removed as IP Hash will always be sent (or empty if unavailable)
    -- _ <- liftM isAdministrator thisClient -- If thisClient was only for isAdminAsking, it might be removable or used for other purposes.

    maybeTargetClientId <- clientByNick asknick
    if isNothing maybeTargetClientId then
        answerClient [ "CHAT", nickServer, loc "Player is not online." ]
    else do
        let targetClientId = fromJust maybeTargetClientId
        let targetClientInfo = rnc `client` targetClientId -- targetClientInfo is the ClientInfo record

        let targetRoomId = clientRoom rnc targetClientId
        let targetClientRoomInfo = room rnc targetRoomId

        let roomMasterSign = if isMaster targetClientInfo then "+" else ""
        let adminSign = if isAdministrator targetClientInfo then "@" else ""
        let roomIdentifierText = if targetRoomId /= lobbyId
                           then B.concat [adminSign, roomMasterSign, loc "room", " ", name targetClientRoomInfo]
                           else adminSign `B.append` loc "lobby"

        let roomGameStatusText = if isJust $ gameInfo targetClientRoomInfo then
                if teamsInGame targetClientInfo > 0 then (loc "(playing)") else (loc "(spectating)")
                else
                B.empty

        -- Prepare the IP Hash string to be sent
        -- ipHash field in targetClientInfo is (Maybe T.Text)
        let finalIpHashDisplayStr = case ipHash targetClientInfo of
                                  Just hashText -> TE.encodeUtf8 hashText -- Convert Text to ByteString
                                  Nothing -> B.empty       -- Send empty if hash is not available

        answerClient [
            "INFO",
            nick targetClientInfo,
            B.concat ["[", finalIpHashDisplayStr, "]"], -- Send the IP Hash (or empty) within brackets
            protoNumber2ver $ clientProto targetClientInfo,
            B.concat ["[", roomIdentifierText, "]", roomGameStatusText]
            ]


handleCmd_loggedin cmd = do
    (ci, rnc) <- ask
    if clientRoom rnc ci == lobbyId then
        handleCmd_lobby cmd
        else
        handleCmd_inRoom cmd
