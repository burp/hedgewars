-- Library for miscellaneous utilitiy functions and global helper variables

HedgewarsScriptLoad("/Scripts/Locale.lua")

--[[ FUNCTIONS ]]
-- Check if a gear is inside a box
function gearIsInBox(gear, x, y, w, h)
    local gx, gy = GetGearPosition(gear)
    if gx >= x and gy >= y and gx <= x + w and gy <= y + h then
        return true
    end
    return false
end

-- Check if a gear is inside a circle
function gearIsInCircle(gear, x, y, r, useRadius)
    local gx, gy = GetGearPosition(gear)
    if useRadius then
        r = r + GetGearRadius(gear)
    end
    if r ^ 2 >= (x - gx) ^ 2 + (y - gy) ^ 2 then
        return true
    end
    return false
end

local function drawFullMap(erase, flush)
	for x = 200,4000,600 do
		for y = 100,2000,150 do
			AddPoint(x, y, 63, erase)
		end
	end
	if flush ~= false then
		FlushPoints()
	end
end

local function challengeRecordToString(recordType, value)
	if recordType == "TimeRecord" then
		return string.format(loc("Team's best time: %.3fs"), value/1000)
	elseif recordType == "TimeRecordHigh" then
		return string.format(loc("Team's longest time: %.3fs"), value/1000)
	elseif recordType == "Highscore" then
		return string.format(loc("Team highscore: %d"), value)
	elseif recordType == "Lowscore" then
		return string.format(loc("Team lowscore: %d"), value)
	elseif recordType == "AccuracyRecord" then
		return string.format(loc("Team's top accuracy: %d%"), value)
	end
end

function getReadableChallengeRecord(recordType)
	local record = tonumber(GetMissionVar(recordType))
	if type(record) ~= "number" then
		return ""
	else
		return challengeRecordToString(recordType, record)
	end
end

function updateChallengeRecord(recordType, value, stat)
	local oldRecord = tonumber(GetMissionVar(recordType))
	local newRecord = false
	if stat == nil then
		stat = recordType ~= "AccuracyRecord"
	end
	if type(oldRecord) ~= "number" then
		newRecord = true
	else
		local recordBeaten = false
		if recordType == "Lowscore" or recordType == "TimeRecord" then
			if value < oldRecord then
				recordBeaten = true
				newRecord = true
			end
		else
			if value > oldRecord then
				recordBeaten = true
				newRecord = true
			end
		end
		if stat then
			if recordBeaten then
				SendStat(siCustomAchievement, loc("You have beaten the team record, congratulations!"))
			else
				SendStat(siCustomAchievement, challengeRecordToString(recordType, oldRecord))
			end
		end
	end
	if newRecord then
		SaveMissionVar(recordType, value)
	end
end

-- Completely fill the map with land. Requires MapGen=mgDrawn.
-- If flush is false, FlushPoints() is not called.
function fillMap(flush)
	drawFullMap(false, flush)
end

-- Completely erase all land from drawn maps. Requires MapGen=mgDrawn.
-- If flush is false, FlushPoints() is not called.
function eraseMap(flush)
	drawFullMap(true, flush)
end

-- Approximative but desync-safe version of square root. This function follows the Babylonian method.
function integerSqrt(num)
	local temp = num
	while (temp*temp - div(temp, 2) > num)
	do
		temp = div((temp + div(num, temp)), 2)
	end
	return math.abs(temp)
end

-- Integer square root of (x^2, y^2), works without desyncs. Is approximative.
-- This is the same as the length of the hypotenuse of a triangle with legs x, y.
function integerHypotenuse(x, y)
	-- To fix overflows
	if(((math.abs(x)^2) + (math.abs(y)^2)) > 2^26)
	then
		local bitr = 2^13
		return integerSqrt((div(math.abs(x), bitr)^2) + (div(math.abs(y), bitr)^2)) * bitr
	else
		return integerSqrt((math.abs(x)^2) + (math.abs(y) ^ 2))
	end
end

-- Insert parameters %1 to %9 into an engine string and returns the result.
-- * text: engine string with parameters (from GetEngineString)
-- * ...: Arguments to insert into the string. The number of arguments MUST match
--        the number of available arguments of the engine string
--
-- Example: formatEngineString(GetEngineString("TMsgStrId", sidWinner), "My Team")
-- to create a string showing the winning team.
function formatEngineString(text, ...)
    local input = text
    for i=1, 9 do
       text = string.gsub(text, "%%"..i, "%%s")
    end
    text = string.format(text, ...)
    return text
end

--[[ GLOBAL VARIABLES ]]

-- Shared common land color values for land sprites.
-- These are useful if you want to make the land type visible.
-- To be used as tint argument of PlaceSprite.
U_LAND_TINT_NORMAL = 0xFFFFFFFF			-- tint for normal land
U_LAND_TINT_INDESTRUCTIBLE = 0x960000FF		-- tint for indestructible land
U_LAND_TINT_ICE = 0x00FAFAFA			-- tint for icy land
U_LAND_TINT_BOUNCY = 0x00FA00FF			-- tint for bouncy land
