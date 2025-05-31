(*
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
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 *)

{$INCLUDE "options.inc"}

unit uLandGraphics;
interface
uses uFloat, uConsts, uTypes, Math, uRenderUtils;

type
    fillType = (nullPixel, backgroundPixel, ebcPixel, icePixel, addNotHHObj, removeNotHHObj, addHH, removeHH, setCurrentHog, removeCurrentHog);

type TRangeArray = array[0..31] of record
                                   Left, Right: LongInt;
                                   end;
     PRangeArray = ^TRangeArray;
TLandCircleProcedure = procedure (landX, landY, pixelX, pixelY: Longint);

function  addBgColor(OldColor, NewColor: LongWord): LongWord;
function  SweepDirty: boolean;
function  Despeckle(X, Y: LongInt): Boolean;
procedure Smooth(X, Y: LongInt);
function  CheckLandValue(X, Y: LongInt; LandFlag: Word): boolean;
function  DrawExplosion(X, Y, Radius: LongInt): Longword;
procedure DrawHLinesExplosions(ar: PRangeArray; Radius: LongInt; y, dY: LongInt; Count: Byte);
procedure DrawTunnel(X, Y, dX, dY: hwFloat; ticks, HalfWidth: LongInt);
function FillRoundInLand(X, Y, Radius: LongInt; Value: Longword): Longword;
function FillRoundInLandFT(X, Y, Radius: LongInt; fill: fillType): Longword;
procedure ChangeRoundInLand(X, Y, Radius: LongInt; doSet, isCurrent, isHH: boolean);
function  LandBackPixel(x, y: LongInt): LongWord;
procedure DrawLine(X1, Y1, X2, Y2: LongInt; Color: Longword);
function  DrawThickLine(X1, Y1, X2, Y2, radius: LongInt; color: Longword): Longword;
procedure DumpLandToLog(x, y, r: LongInt);
procedure DrawIceBreak(x, y, iceRadius, iceHeight: Longint);
function TryPlaceOnLandSimple(cpX, cpY: LongInt; Obj: TSprite; Frame: LongInt; doPlace, indestructible: boolean): boolean;
function TryPlaceOnLand(cpX, cpY: LongInt; Obj: TSprite; Frame: LongInt; doPlace: boolean; LandFlags: Word): boolean;
function ForcePlaceOnLand(cpX, cpY: LongInt; Obj: TSprite; Frame: LongInt; LandFlags: Word; Tint: LongWord; Behind, flipHoriz, flipVert: boolean): boolean;
function TryPlaceOnLand(cpX, cpY: LongInt; Obj: TSprite; Frame: LongInt; doPlace, outOfMap, force, behind, flipHoriz, flipVert: boolean; LandFlags: Word; Tint: LongWord): boolean;
procedure EraseLandRectRaw(X, Y, width, height: LongWord);
procedure EraseLand(cpX, cpY: LongInt; Obj: TSprite; Frame: LongInt; LandFlags: Word; eraseOnLFMatch, onlyEraseLF, flipHoriz, flipVert: boolean);
function GetPlaceCollisionTex(cpX, cpY: LongInt; Obj: TSprite; Frame: LongInt): PTexture;

implementation
uses SDLh, uLandTexture, uTextures, uVariables, uUtils, uDebug, uScript, uLandUtils;


procedure calculatePixelsCoordinates(landX, landY: Longint; var pixelX, pixelY: Longint);
begin
if (cReducedQuality and rqBlurryLand) = 0 then
    begin
    pixelX := landX;
    pixelY := landY;
    end
else
    begin
    pixelX := LandX div 2;
    pixelY := LandY div 2;
    end;
end;

function drawPixelBG(landX, landY, pixelX, pixelY: Longint): Longword;
begin
drawPixelBG := 0;
if (LandGet(LandY, landX) and lfIndestructible) = 0 then
    begin
        if ((LandGet(landY, landX) and lfBasic) <> 0) and (((LandPixelGet(pixelY, pixelX) and AMask) shr AShift) = 255) and (not disableLandBack) then
        begin
            LandPixelSet(pixelY, pixelX, LandBackPixel(landX, landY));
            inc(drawPixelBG);
        end
        else if ((LandGet(landY, landX) and lfObject) <> 0) or (((LandPixelGet(pixelY, pixelX) and AMask) shr AShift) < 255) then
            LandPixelSet(pixelY, pixelX, ExplosionBorderColorNoA)
    end;
end;

procedure drawPixelEBC(landX, landY, pixelX, pixelY: Longint);
begin
if (LandGet(landY, landX) and lfIndestructible = 0) and
    (((LandGet(landY, landX) and lfBasic) <> 0) or ((LandGet(landY, landX) and lfObject) <> 0)) then
    begin
    LandPixelSet(pixelY, pixelX, ExplosionBorderColor);
    LandSet(landY, landX, (LandGet(landY, landX) or lfDamaged) and (not lfIce));
    LandDirty[landY div 32, landX div 32]:= 1;
    end;
end;

function isLandscapeEdge(weight:Longint):boolean;
begin
isLandscapeEdge := (weight < 8) and (weight >= 2);
end;

function getPixelWeight(x, y:Longint): Longint;
var
    i, j, r: Longint;
begin
r := 0;
for i := x - 1 to x + 1 do
    for j := y - 1 to y + 1 do
    begin
    if (i < 0) or
       (i > LAND_WIDTH - 1) or
       (j < 0) or
       (j > LAND_HEIGHT -1) then
       exit(9);

    if LandGet(j, i) and lfLandMask and (not lfIce) = 0 then
       inc(r)
    end;

    getPixelWeight:= r
end;


procedure fillPixelFromIceSprite(pixelX, pixelY:Longint);
var
    iceSurface: PSDL_Surface;
    icePixels: PLongwordArray;
    w: LongWord;
begin
    if cOnlyStats then exit;
    // So. 3 parameters here. Ice colour, Ice opacity, and a bias on the greyscaled pixel towards lightness
    iceSurface:= SpritesData[sprIceTexture].Surface;
    icePixels := iceSurface^.pixels;
    w:= LandPixelGet(pixelY, pixelX);
    if w > 0 then
        begin
        w:= round(((w shr RShift and $FF) * RGB_LUMINANCE_RED +
              (w shr BShift and $FF) * RGB_LUMINANCE_GREEN +
              (w shr GShift and $FF) * RGB_LUMINANCE_BLUE));
        if w < 128 then w:= w+128;
        if w > 255 then w:= 255;
        w:= (w shl RShift) or (w shl BShift) or (w shl GShift) or (LandPixelGet(pixelY, pixelX) and AMask);
        LandPixelSet(pixelY, pixelX, addBgColor(w, IceColor));
        LandPixelSet(pixelY, pixelX, addBgColor(LandPixelGet(pixelY, pixelX), icePixels^[iceSurface^.w * (pixelY mod iceSurface^.h) + (pixelX mod iceSurface^.w)]))
        end
    else
        begin
        LandPixelSet(pixelY, pixelX, IceColor and (not AMask) or $E8 shl AShift);
        LandPixelSet(pixelY, pixelX, addBgColor(LandPixelGet(pixelY, pixelX), icePixels^[iceSurface^.w * (pixelY mod iceSurface^.h) + (pixelX mod iceSurface^.w)]));
        // silly workaround to avoid having to make background erasure a tadb it smarter about sea ice
        if LandPixelGet(pixelY, pixelX) and AMask shr AShift = 255 then
            LandPixelSet(pixelY, pixelX, LandPixelGet(pixelY, pixelX) and (not AMask) or 254 shl AShift);
        end;
end;


procedure DrawPixelIce(landX, landY, pixelX, pixelY: Longint);
begin
if ((LandGet(landY, landX) and lfIce) <> 0) then exit;
if (pixelX < LeftX) or (pixelX > RightX) or (pixelY < TopY) then exit;
if isLandscapeEdge(getPixelWeight(landX, landY)) then
    begin
    if (LandPixelGet(pixelY, pixelX) and AMask < 255) and (LandPixelGet(pixelY, pixelX) and AMask > 0) then
        LandPixelSet(pixelY, pixelX, (IceEdgeColor and (not AMask)) or (LandPixelGet(pixelY, pixelX) and AMask))
    else if (LandPixelGet(pixelY, pixelX) and AMask < 255) or (LandGet(landY, landX) > 255) then
        LandPixelSet(pixelY, pixelX, IceEdgeColor)
    end
else if LandGet(landY, landX) > 255 then
    begin
        fillPixelFromIceSprite(pixelX, pixelY);
    end;
if LandGet(landY, landX) > 255 then LandSet(landY, landX, LandGet(landY, landX) or lfIce and (not lfDamaged));
end;


function FillLandCircleLineFT(y, fromPix, toPix: LongInt; fill : fillType): Longword;
var px, py, i: LongInt;
begin
//get rid of compiler warning
    px := 0;
    py := 0;
    FillLandCircleLineFT := 0;
    case fill of
    backgroundPixel:
        for i:= fromPix to toPix do
            begin
            calculatePixelsCoordinates(i, y, px, py);
            inc(FillLandCircleLineFT, drawPixelBG(i, y, px, py));
            end;
    ebcPixel:
        for i:= fromPix to toPix do
            begin
            calculatePixelsCoordinates(i, y, px, py);
            drawPixelEBC(i, y, px, py);
            end;
    nullPixel:
        for i:= fromPix to toPix do
            begin
            calculatePixelsCoordinates(i, y, px, py);
            if ((LandGet(y, i) and lfIndestructible) = 0) and (not disableLandBack or (LandGet(y, i) > 255))  then
                LandPixelSet(py, px, ExplosionBorderColorNoA);
            end;
    icePixel:
        for i:= fromPix to toPix do
            begin
            calculatePixelsCoordinates(i, y, px, py);
            DrawPixelIce(i, y, px, py);
            end;
    addNotHHObj:
        for i:= fromPix to toPix do
            begin
            if LandGet(y, i) and lfNotHHObjMask shr lfNotHHObjShift < lfNotHHObjSize then
                LandSet(y, i, (LandGet(y, i) and (not lfNotHHObjMask)) or ((LandGet(y, i) and lfNotHHObjMask shr lfNotHHObjShift + 1) shl lfNotHHObjShift));
            end;
    removeNotHHObj:
        for i:= fromPix to toPix do
            begin
            if LandGet(y, i) and lfNotHHObjMask <> 0 then
                LandSet(y, i, (LandGet(y, i) and (not lfNotHHObjMask)) or ((LandGet(y, i) and lfNotHHObjMask shr lfNotHHObjShift - 1) shl lfNotHHObjShift));
            end;
    addHH:
        for i:= fromPix to toPix do
            begin
            if LandGet(y, i) and lfHHMask < lfHHMask then
                LandSet(y, i, LandGet(y, i) + 1)
            end;
    removeHH:
        for i:= fromPix to toPix do
            begin
            if LandGet(y, i) and lfHHMask > 0 then
                LandSet(y, i, LandGet(y, i) - 1);
            end;
    setCurrentHog:
        for i:= fromPix to toPix do
            begin
            LandSet(y, i, LandGet(y, i) or lfCurHogCrate)
            end;
    removeCurrentHog:
        for i:= fromPix to toPix do
            begin
            LandSet(y, i, LandGet(y, i) and lfNotCurHogCrate);
            end;
    end;
end;

function FillLandCircleSegmentFT(x, y, dx, dy: LongInt; fill : fillType): Longword;
begin
    FillLandCircleSegmentFT := 0;
if ((y + dy) and LAND_HEIGHT_MASK) = 0 then
    inc(FillLandCircleSegmentFT, FillLandCircleLineFT(y + dy, Max(x - dx, 0), Min(x + dx, LAND_WIDTH - 1), fill));
if ((y - dy) and LAND_HEIGHT_MASK) = 0 then
    inc(FillLandCircleSegmentFT, FillLandCircleLineFT(y - dy, Max(x - dx, 0), Min(x + dx, LAND_WIDTH - 1), fill));
if ((y + dx) and LAND_HEIGHT_MASK) = 0 then
    inc(FillLandCircleSegmentFT, FillLandCircleLineFT(y + dx, Max(x - dy, 0), Min(x + dy, LAND_WIDTH - 1), fill));
if ((y - dx) and LAND_HEIGHT_MASK) = 0 then
    inc(FillLandCircleSegmentFT, FillLandCircleLineFT(y - dx, Max(x - dy, 0), Min(x + dy, LAND_WIDTH - 1), fill));
end;

function FillRoundInLandFT(X, Y, Radius: LongInt; fill: fillType): Longword;
var dx, dy, d: LongInt;
begin
dx:= 0;
dy:= Radius;
d:= 3 - 2 * Radius;
FillRoundInLandFT := 0;
while (dx < dy) do
    begin
    inc(FillRoundInLandFT, FillLandCircleSegmentFT(x, y, dx, dy, fill));
    if (d < 0) then
        d:= d + 4 * dx + 6
    else
        begin
        d:= d + 4 * (dx - dy) + 10;
        dec(dy)
        end;
    inc(dx)
    end;
if (dx = dy) then
    inc (FillRoundInLandFT, FillLandCircleSegmentFT(x, y, dx, dy, fill));
end;


function addBgColor(OldColor, NewColor: LongWord): LongWord;
// Factor ranges from 0 to 100% NewColor
var
    oRed, oBlue, oGreen, oAlpha, nRed, nBlue, nGreen, nAlpha: byte;
begin
    oAlpha := (OldColor shr AShift);
    nAlpha := (NewColor shr AShift);
    // shortcircuit
    if (oAlpha = 0) or (nAlpha = $FF) then
        begin
        addBgColor:= NewColor;
        exit
        end;
    // Get colors
    oRed   := (OldColor shr RShift);
    oGreen := (OldColor shr GShift);
    oBlue  := (OldColor shr BShift);

    nRed   := (NewColor shr RShift);
    nGreen := (NewColor shr GShift);
    nBlue  := (NewColor shr BShift);

    // Mix colors
    nRed   := min(255,((nRed*nAlpha) div 255) + ((oRed*oAlpha*byte(255-nAlpha)) div 65025));
    nGreen := min(255,((nGreen*nAlpha) div 255) + ((oGreen*oAlpha*byte(255-nAlpha)) div 65025));
    nBlue  := min(255,((nBlue*nAlpha) div 255) + ((oBlue*oAlpha*byte(255-nAlpha)) div 65025));
    nAlpha := min(255, oAlpha + nAlpha);

    addBgColor := (nAlpha shl AShift) or (nRed shl RShift) or (nGreen shl GShift) or (nBlue shl BShift);
end;

function FillCircleLines(x, y, dx, dy: LongInt; Value: Longword): Longword;
var i: LongInt;
begin
    FillCircleLines:= 0;

    if ((y + dy) and LAND_HEIGHT_MASK) = 0 then
        for i:= Max(x - dx, 0) to Min(x + dx, LAND_WIDTH - 1) do
            if (LandGet(y + dy, i) and lfIndestructible) = 0 then
            begin
                if LandGet(y + dy, i) <> Value then inc(FillCircleLines);
                LandSet(y + dy, i, Value);
            end;
    if ((y - dy) and LAND_HEIGHT_MASK) = 0 then
        for i:= Max(x - dx, 0) to Min(x + dx, LAND_WIDTH - 1) do
            if (LandGet(y - dy, i) and lfIndestructible) = 0 then
            begin
                if LandGet(y - dy, i) <> Value then inc(FillCircleLines);
                LandSet(y - dy, i, Value);
            end;
    if ((y + dx) and LAND_HEIGHT_MASK) = 0 then
        for i:= Max(x - dy, 0) to Min(x + dy, LAND_WIDTH - 1) do
            if (LandGet(y + dx, i) and lfIndestructible) = 0 then
            begin
                if LandGet(y + dx, i) <> Value then inc(FillCircleLines);
                LandSet(y + dx, i, Value);
            end;
    if ((y - dx) and LAND_HEIGHT_MASK) = 0 then
        for i:= Max(x - dy, 0) to Min(x + dy, LAND_WIDTH - 1) do
            if (LandGet(y - dx, i) and lfIndestructible) = 0 then
            begin
                if LandGet(y - dx, i) <> Value then inc(FillCircleLines);
                LandSet(y - dx, i, Value);
            end;
end;

function FillRoundInLand(X, Y, Radius: LongInt; Value: Longword): Longword;
var dx, dy, d: LongInt;
begin
FillRoundInLand:= 0;
dx:= 0;
dy:= Radius;
d:= 3 - 2 * Radius;
while (dx < dy) do
    begin
    inc(FillRoundInLand, FillCircleLines(x, y, dx, dy, Value));
    if (d < 0) then
        d:= d + 4 * dx + 6
    else
        begin
        d:= d + 4 * (dx - dy) + 10;
        dec(dy)
        end;
    inc(dx)
    end;
if (dx = dy) then
    inc(FillRoundInLand, FillCircleLines(x, y, dx, dy, Value));
end;

procedure ChangeRoundInLand(X, Y, Radius: LongInt; doSet, isCurrent, isHH: boolean);
begin
if not doSet and isCurrent then
    FillRoundInLandFT(X, Y, Radius, removeCurrentHog)
else if (not doSet) and (not IsCurrent) and isHH then
    FillRoundInLandFT(X, Y, Radius, removeHH)
else if (not doSet) and (not IsCurrent) and (not isHH) then
    FillRoundInLandFT(X, Y, Radius, removeNotHHObj)
else if doSet and IsCurrent then
    FillRoundInLandFT(X, Y, Radius, setCurrentHog)
else if doSet and (not IsCurrent) and isHH then
    FillRoundInLandFT(X, Y, Radius, addHH)
else if doSet and (not IsCurrent) and (not isHH) then
    FillRoundInLandFT(X, Y, Radius, addNotHHObj);
end;

procedure DrawIceBreak(x, y, iceRadius, iceHeight: Longint);
var
    i, j, iceL, iceR, IceT, iceB: LongInt;
    landRect: TSDL_Rect;
begin
// figure out bottom/left/right/top coords of ice to draw

// determine absolute limits first
iceT:= 0;
iceB:= min(cWaterLine, LAND_HEIGHT - 1);

iceL:= 0;
iceR:= LAND_WIDTH - 1;

if WorldEdge <> weNone then
    begin
    iceL:= max(leftX,  iceL);
    iceR:= min(rightX, iceR);
    end;

// adjust based on location but without violating absolute limits
if y >= cWaterLine then
    begin
    iceL:= max(x - iceRadius, iceL);
    iceR:= min(x + iceRadius, iceR);
    iceT:= max(cWaterLine - iceHeight, iceT);
    end
else {if WorldEdge = weSea then}
    begin
    iceT:= max(y - iceRadius, iceT);
    iceB:= min(y + iceRadius, iceB);
    if x <= leftX then
        iceR:= min(leftX + iceHeight, iceR)
    else {if x >= rightX then}
        iceL:= max(rightX - iceHeight, iceL);
    end;

// don't continue if all ice is outside land array
if (iceL > iceR) or (iceT > iceB) then
    exit();

for i := iceL to iceR do
    begin
    for j := iceT to iceB do
        begin
        if LandGet(j, i) = 0 then
            begin
            LandSet(j, i, lfIce);
            if (cReducedQuality and rqBlurryLand) = 0 then
                fillPixelFromIceSprite(i, j)
            else
                fillPixelFromIceSprite(i div 2, j div 2);
            end;
        end;
    end;

landRect.x := iceL;
landRect.y := iceT;
landRect.w := iceR - IceL + 1;
landRect.h := iceB - iceT + 1;

UpdateLandTexture(landRect.x, landRect.w, landRect.y, landRect.h, true);
end;

function DrawExplosion(X, Y, Radius: LongInt): Longword;
var
    tx, ty, dx, dy: Longint;
begin
    DrawExplosion := FillRoundInLandFT(x, y, Radius, backgroundPixel);
    if Radius > 20 then
        FillRoundInLandFT(x, y, Radius - 15, nullPixel);
    FillRoundInLand(X, Y, Radius, 0);
    FillRoundInLandFT(x, y, Radius + 4, ebcPixel);
    tx:= Max(X - Radius - 5, 0);
    dx:= Min(X + Radius + 5, LAND_WIDTH) - tx;
    ty:= Max(Y - Radius - 5, 0);
    dy:= Min(Y + Radius + 5, LAND_HEIGHT) - ty;
    UpdateLandTexture(tx, dx, ty, dy, false);
end;

procedure DrawHLinesExplosions(ar: PRangeArray; Radius: LongInt; y, dY: LongInt; Count: Byte);
var tx, ty, by, bx,  i: LongInt;
begin
for i:= 0 to Pred(Count) do
    begin
    for ty:= Max(y - Radius, 0) to Min(y + Radius, TopY) do
        for tx:= Max(LeftX, ar^[i].Left - Radius) to Min(RightX, ar^[i].Right + Radius) do
            begin
            if (LandGet(ty, tx) and lfIndestructible) = 0 then
                begin
                if (cReducedQuality and rqBlurryLand) = 0 then
                    begin
                    by:= ty; bx:= tx;
                    end
                else
                    begin
                    by:= ty div 2; bx:= tx div 2;
                    end;
                if ((LandGet(ty, tx) and lfBasic) <> 0) and (((LandPixelGet(by,bx) and AMask) shr AShift) = 255) and (not disableLandBack) then
                    LandPixelSet(by, bx, LandBackPixel(tx, ty))
                else if ((LandGet(ty, tx) and lfObject) <> 0) or (((LandPixelGet(by,bx) and AMask) shr AShift) < 255) then
                    LandPixelSet(by, bx, LandPixelGet(by, bx) and (not AMASK))
                end
            end;
    inc(y, dY)
    end;

inc(Radius, 4);
dec(y, Count * dY);

for i:= 0 to Pred(Count) do
    begin
    for ty:= Max(y - Radius, 0) to Min(y + Radius, TopY) do
        for tx:= Max(LeftX, ar^[i].Left - Radius) to Min(RightX, ar^[i].Right + Radius) do
            if ((LandGet(ty, tx) and lfBasic) <> 0) or ((LandGet(ty, tx) and lfObject) <> 0) then
                begin
                 if (cReducedQuality and rqBlurryLand) = 0 then
                    LandPixelSet(ty, tx, ExplosionBorderColor)
                else
                    LandPixelSet(ty div 2, tx div 2, ExplosionBorderColor);

                LandSet(ty, tx, (LandGet(ty, tx) or lfDamaged) and (not lfIce));
                LandDirty[ty div 32, tx div 32]:= 1;
                end;
    inc(y, dY)
    end;


UpdateLandTexture(0, LAND_WIDTH, 0, LAND_HEIGHT, false)
end;



procedure DrawExplosionBorder(X, Y, dx, dy:hwFloat;  despeckle : Boolean);
var
    t, tx, ty :Longint;
begin
for t:= 0 to 7 do
    begin
    X:= X + dX;
    Y:= Y + dY;
    tx:= hwRound(X);
    ty:= hwRound(Y);
    if ((ty and LAND_HEIGHT_MASK) = 0) and ((tx and LAND_WIDTH_MASK) = 0) and (((LandGet(ty, tx) and lfBasic) <> 0)
    or ((LandGet(ty, tx) and lfObject) <> 0)) then
        begin
        LandSet(ty, tx, (LandGet(ty, tx) or lfDamaged) and (not lfIce));
        if despeckle then
            LandDirty[ty div 32, tx div 32]:= 1;
        if (cReducedQuality and rqBlurryLand) = 0 then
            LandPixelSet(ty, tx, ExplosionBorderColor)
        else
            LandPixelSet(ty div 2, tx div 2, ExplosionBorderColor)
        end
    end;
end;

type TWrapNeeded = (wnNone, wnLeft, wnRight);

//
//  - (dX, dY) - direction, vector of length = 0.5
//
function DrawTunnel_real(X, Y, dX, dY: hwFloat; ticks, HalfWidth: LongInt): TWrapNeeded;
var nx, ny, dX8, dY8: hwFloat;
    i, t, tx, ty, by, bx, stX, stY, ddy, ddx: Longint;
    despeckle : Boolean;
begin  // (-dY, dX) is (dX, dY) rotated by PI/2
DrawTunnel_real:= wnNone;

stY:= hwRound(Y);
stX:= hwRound(X);

despeckle:= HalfWidth > 1;

nx:= X + dY * (HalfWidth + 8);
ny:= Y - dX * (HalfWidth + 8);

dX8:= dX * 8;
dY8:= dY * 8;
for i:= 0 to 7 do
    begin
    X:= nx - dX8;
    Y:= ny - dY8;
    for t:= -8 to ticks + 8 do
    begin
    X:= X + dX;
    Y:= Y + dY;
    tx:= hwRound(X);
    ty:= hwRound(Y);
    if ((ty and LAND_HEIGHT_MASK) = 0)
    and ((tx and LAND_WIDTH_MASK) = 0)
    and (((LandGet(ty, tx) and lfBasic) <> 0) or ((LandGet(ty, tx) and lfObject) <> 0)) then
        begin
        LandSet(ty, tx, LandGet(ty, tx) and (not lfIce));
        if despeckle then
            begin
            LandSet(ty, tx, LandGet(ty, tx) or lfDamaged);
            LandDirty[ty div 32, tx div 32]:= 1
            end;
        if (cReducedQuality and rqBlurryLand) = 0 then
            LandPixelSet(ty, tx, ExplosionBorderColor)
        else
            LandPixelSet(ty div 2, tx div 2, ExplosionBorderColor)
        end
    end;
    nx:= nx - dY;
    ny:= ny + dX;
    end;

for i:= -HalfWidth to HalfWidth do
    begin
    X:= nx - dX8;
    Y:= ny - dY8;
    DrawExplosionBorder(X, Y, dx, dy, despeckle);
    X:= nx;
    Y:= ny;
    for t:= 0 to ticks do
        begin
        X:= X + dX;
        Y:= Y + dY;
        tx:= hwRound(X);
        ty:= hwRound(Y);
        if ((ty and LAND_HEIGHT_MASK) = 0) and ((tx and LAND_WIDTH_MASK) = 0) and ((LandGet(ty, tx) and lfIndestructible) = 0) then
            begin
            if (cReducedQuality and rqBlurryLand) = 0 then
                begin
                by:= ty; bx:= tx;
                end
            else
                begin
                by:= ty div 2; bx:= tx div 2;
                end;
            if ((LandGet(ty, tx) and lfBasic) <> 0) and (((LandPixelGet(by,bx) and AMask) shr AShift) = 255) and (not disableLandBack) then
                LandPixelSet(by, bx, LandBackPixel(tx, ty))
            else if ((LandGet(ty, tx) and lfObject) <> 0) or (((LandPixelGet(by,bx) and AMask) shr AShift) < 255) then
                LandPixelSet(by, bx, LandPixelGet(by, bx) and (not AMASK));
            LandSet(ty, tx, 0);
            end
        end;
    DrawExplosionBorder(X, Y, dx, dy, despeckle);
    nx:= nx - dY;
    ny:= ny + dX;
    end;

for i:= 0 to 7 do
    begin
    X:= nx - dX8;
    Y:= ny - dY8;
    for t:= -8 to ticks + 8 do
    begin
    X:= X + dX;
    Y:= Y + dY;
    tx:= hwRound(X);
    ty:= hwRound(Y);
    if ((ty and LAND_HEIGHT_MASK) = 0) and ((tx and LAND_WIDTH_MASK) = 0) and (((LandGet(ty, tx) and lfBasic) <> 0)
    or ((LandGet(ty, tx) and lfObject) <> 0)) then
        begin
        LandSet(ty, tx, (LandGet(ty, tx) or lfDamaged) and (not lfIce));
        if despeckle then
            LandDirty[ty div 32, tx div 32]:= 1;
        if (cReducedQuality and rqBlurryLand) = 0 then
            LandPixelSet(ty, tx, ExplosionBorderColor)
        else
            LandPixelSet(ty div 2, tx div 2, ExplosionBorderColor)
        end
    end;
    nx:= nx - dY;
    ny:= ny + dX;
    end;

tx:= stX - HalfWidth * 2 - 4 - abs(hwRound(dX * ticks));
ddx:= stX + HalfWidth * 2 + 4 + abs(hwRound(dX * ticks));

if WorldEdge = weWrap then
    begin
    if (tx < leftX) or (ddx < leftX) then
        DrawTunnel_real:= wnLeft
    else if (tx > rightX) or (ddx > rightX) then
        DrawTunnel_real:= wnRight;
    end;

tx:= Max(tx, 0);
ty:= Max(stY - HalfWidth * 2 - 4 - abs(hwRound(dY * ticks)), 0);
ddx:= Min(ddx, LAND_WIDTH) - tx;
ddy:= Min(stY + HalfWidth * 2 + 4 + abs(hwRound(dY * ticks)), LAND_HEIGHT) - ty;

UpdateLandTexture(tx, ddx, ty, ddy, false)
end;

procedure DrawTunnel(X, Y, dX, dY: hwFloat; ticks, HalfWidth: LongInt);
var wn: TWrapNeeded;
begin
wn:= DrawTunnel_real(X, Y, dX, dY, ticks, HalfWidth);
if wn <> wnNone then
    begin
    if wn = wnLeft then
        DrawTunnel_real(X + int2hwFloat(playWidth), Y, dX, dY, ticks, HalfWidth)
    else
        DrawTunnel_real(X - int2hwFloat(playWidth), Y, dX, dY, ticks, HalfWidth);
    end;
end;

function TryPlaceOnLandSimple(cpX, cpY: LongInt; Obj: TSprite; Frame: LongInt; doPlace, indestructible: boolean): boolean;
var lf: Word;
begin
if indestructible then
    lf:= lfIndestructible
else
    lf:= 0;
TryPlaceOnLandSimple:= TryPlaceOnLand(cpX, cpY, Obj, Frame, doPlace, false, false, false, false, false, lf, $FFFFFFFF);
end;

function TryPlaceOnLand(cpX, cpY: LongInt; Obj: TSprite; Frame: LongInt; doPlace: boolean; LandFlags: Word): boolean;
begin
TryPlaceOnLand:= TryPlaceOnLand(cpX, cpY, Obj, Frame, doPlace, false, false, false, false, false, LandFlags, $FFFFFFFF);
end;

function ForcePlaceOnLand(cpX, cpY: LongInt; Obj: TSprite; Frame: LongInt; LandFlags: Word; Tint: LongWord; Behind, flipHoriz, flipVert: boolean): boolean;
begin
    ForcePlaceOnLand:= TryPlaceOnLand(cpX, cpY, Obj, Frame, true, false, true, behind, flipHoriz, flipVert, LandFlags, Tint)
end;
function TryPlaceOnLand(cpX, cpY: LongInt; Obj: TSprite; Frame: LongInt; doPlace, outOfMap, force, behind, flipHoriz, flipVert: boolean; LandFlags: Word; Tint: LongWord): boolean;
var X, Y, bpp, h, w, row, col, gx, gy, numFramesFirstCol: LongInt;
    p: PByteArray;
    Image: PSDL_Surface;
    pixel: LongWord;
begin
TryPlaceOnLand:= false;
numFramesFirstCol:= SpritesData[Obj].imageHeight div SpritesData[Obj].Height;

if outOfMap then doPlace:= false; // just using for a check

if checkFails(SpritesData[Obj].Surface <> nil, 'Assert SpritesData[Obj].Surface failed', true) then exit;

Image:= SpritesData[Obj].Surface;
w:= SpritesData[Obj].Width;
h:= SpritesData[Obj].Height;
if flipVert then flipSurface(Image, true);
if flipHoriz then flipSurface(Image, false);
row:= Frame mod numFramesFirstCol;
col:= Frame div numFramesFirstCol;

if SDL_MustLock(Image) then
    if SDLCheck(SDL_LockSurface(Image) >= 0, 'TryPlaceOnLand', true) then exit;

bpp:= Image^.format^.BytesPerPixel;
if checkFails(bpp = 4, 'It should be 32 bpp sprite', true) then
begin
    if SDL_MustLock(Image) then
        SDL_UnlockSurface(Image);
    exit
end;
// Check that sprite fits free space
p:= PByteArray(@(PByteArray(Image^.pixels)^[ Image^.pitch * row * h + col * w * 4 ]));
case bpp of
    4: for y:= 0 to Pred(h) do
        begin
        for x:= 0 to Pred(w) do
            if ((PLongword(@(p^[x * 4]))^) and AMask) <> 0 then
                if (outOfMap and
                   ((cpY + y) < LAND_HEIGHT) and ((cpY + y) >= 0) and
                   ((cpX + x) < LAND_WIDTH) and ((cpX + x) >= 0) and
                   ((not force) and (LandGet(cpY + y, cpX + x) <> 0))) or

                   (not outOfMap and
                       (((cpY + y) <= topY) or ((cpY + y) >= LAND_HEIGHT) or
                       ((cpX + x) <= leftX) or ((cpX + x) >= rightX) or
                       ((not force) and (LandGet(cpY + y, cpX + x) <> 0)))) then
                   begin
                   if SDL_MustLock(Image) then
                       SDL_UnlockSurface(Image);
                   exit
                   end;
        p:= PByteArray(@(p^[Image^.pitch]))
        end
    end;

TryPlaceOnLand:= true;
if not doPlace then
    begin
    if SDL_MustLock(Image) then
        SDL_UnlockSurface(Image);
    exit
    end;

// Checked, now place
p:= PByteArray(@(PByteArray(Image^.pixels)^[ Image^.pitch * row * h + col * w * 4 ]));
case bpp of
    4: for y:= 0 to Pred(h) do
        begin
        for x:= 0 to Pred(w) do
            if ((PLongword(@(p^[x * 4]))^) and AMask) <> 0 then
                begin
                if (cReducedQuality and rqBlurryLand) = 0 then
                    begin
                    gX:= cpX + x;
                    gY:= cpY + y;
                    end
                else
                    begin
                    gX:= (cpX + x) div 2;
                    gY:= (cpY + y) div 2;
                    end;
                if (not behind) or (LandGet(cpY + y, cpX + x) and lfLandMask = 0) then
                    begin
                    if (LandFlags and lfBasic <> 0) or
                       ((LandPixelGet(gY, gX) and AMask shr AShift > 128) and  // This test assumes lfBasic and lfObject differ only graphically
                         (LandFlags and (lfObject or lfIce) = 0)) then
                         LandSet(cpY + y, cpX + x, lfBasic or LandFlags)
                    else if (LandFlags and lfIce = 0) then
						 LandSet(cpY + y, cpX + x, lfObject or LandFlags)
					else LandSet(cpY + y, cpX + x, LandFlags)
                    end;
                if (not behind) or (LandPixelGet(gY, gX) = 0) then
                    begin
                    if tint = $FFFFFFFF then
                        LandPixelSet(gY, gX, PLongword(@(p^[x * 4]))^)
                    else
                        begin
                        pixel:= PLongword(@(p^[x * 4]))^;
                        LandPixelSet(gY, gX,
                           ceil((pixel shr RShift and $FF) * ((tint shr 24) / 255)) shl RShift or
                           ceil((pixel shr GShift and $FF) * ((tint shr 16 and $ff) / 255)) shl GShift or
                           ceil((pixel shr BShift and $FF) * ((tint shr  8 and $ff) / 255)) shl BShift or
                           ceil((pixel shr AShift and $FF) * ((tint and $ff) / 255)) shl AShift);
                        end
                    end
                end;
        p:= PByteArray(@(p^[Image^.pitch]));
        end;
    end;
if SDL_MustLock(Image) then
    SDL_UnlockSurface(Image);

if flipVert then flipSurface(Image, true);
if flipHoriz then flipSurface(Image, false);

x:= Max(cpX, leftX);
w:= Min(cpX + Image^.w, LAND_WIDTH) - x;
y:= Max(cpY, topY);
h:= Min(cpY + Image^.h, LAND_HEIGHT) - y;
UpdateLandTexture(x, w, y, h, true);

ScriptCall('onSpritePlacement', ord(Obj), cpX + w div 2, cpY + h div 2);
if Obj = sprAmGirder then
    ScriptCall('onGirderPlacement', frame, cpX + w div 2, cpY + h div 2)
else if Obj = sprAmRubber then
    ScriptCall('onRubberPlacement', frame, cpX + w div 2, cpY + h div 2);

end;

procedure EraseLandRectRaw(X, Y, width, height: LongWord);
var tx, ty: LongWord;
begin
for ty:= 0 to height - 1 do
    for tx:= 0 to width - 1 do
        begin
        LandPixelSet(ty, tx, 0);
        LandSet(Y + ty, X + tx, 0);
        end;
end;

procedure EraseLand(cpX, cpY: LongInt; Obj: TSprite; Frame: LongInt; LandFlags: Word; eraseOnLFMatch, onlyEraseLF, flipHoriz, flipVert: boolean);
var X, Y, bpp, h, w, row, col, gx, gy, numFramesFirstCol: LongInt;
    p: PByteArray;
    Image: PSDL_Surface;
begin
numFramesFirstCol:= SpritesData[Obj].imageHeight div SpritesData[Obj].Height;

if checkFails(SpritesData[Obj].Surface <> nil, 'Assert SpritesData[Obj].Surface failed', true) then exit;

Image:= SpritesData[Obj].Surface;
w:= SpritesData[Obj].Width;
h:= SpritesData[Obj].Height;
if flipVert then flipSurface(Image, true);
if flipHoriz then flipSurface(Image, false);
row:= Frame mod numFramesFirstCol;
col:= Frame div numFramesFirstCol;

if SDL_MustLock(Image) then
    if SDLCheck(SDL_LockSurface(Image) >= 0, 'EraseLand', true) then exit;

bpp:= Image^.format^.BytesPerPixel;
if checkFails(bpp = 4, 'It should be 32 bpp sprite', true) then
begin
    if SDL_MustLock(Image) then
        SDL_UnlockSurface(Image);
    exit
end;
// Check that sprite fits free space
p:= PByteArray(@(PByteArray(Image^.pixels)^[ Image^.pitch * row * h + col * w * 4 ]));

    for y:= 0 to Pred(h) do
        begin
        for x:= 0 to Pred(w) do
            if ((PLongword(@(p^[x * 4]))^) and AMask) <> 0 then
                if ((cpY + y) <= topY) or ((cpY + y) >= LAND_HEIGHT) or
                   ((cpX + x) <= leftX) or ((cpX + x) >= rightX) then
                   begin
                   if SDL_MustLock(Image) then
                       SDL_UnlockSurface(Image);
                   exit
                   end;
        p:= PByteArray(@(p^[Image^.pitch]))
        end;

// Checked, now place
p:= PByteArray(@(PByteArray(Image^.pixels)^[ Image^.pitch * row * h + col * w * 4 ]));
    for y:= 0 to Pred(h) do
        begin
        for x:= 0 to Pred(w) do
            if ((PLongword(@(p^[x * 4]))^) and AMask) <> 0 then
                   begin
                if (cReducedQuality and rqBlurryLand) = 0 then
                    begin
                    gX:= cpX + x;
                    gY:= cpY + y;
                    end
                else
                    begin
                    gX:= (cpX + x) div 2;
                    gY:= (cpY + y) div 2;
                    end;
                if (not eraseOnLFMatch or (LandGet(cpY + y, cpX + x) and LandFlags <> 0)) and
                    ((PLongword(@(p^[x * 4]))^) and AMask <> 0) then
                    begin
                    if not onlyEraseLF then
                        begin
                        LandPixelSet(gY, gX, 0);
                        LandSet(cpY + y, cpX + x, 0)
                        end
                    else LandSet(cpY + y, cpX + x, LandGet(cpY + y, cpX + x) and (not LandFlags))
                    end
                end;
        p:= PByteArray(@(p^[Image^.pitch]));
        end;
if SDL_MustLock(Image) then
    SDL_UnlockSurface(Image);

if flipVert then flipSurface(Image, true);
if flipHoriz then flipSurface(Image, false);

x:= Max(cpX, leftX);
w:= Min(cpX + Image^.w, LAND_WIDTH) - x;
y:= Max(cpY, topY);
h:= Min(cpY + Image^.h, LAND_HEIGHT) - y;
UpdateLandTexture(x, w, y, h, true)
end;

function GetPlaceCollisionTex(cpX, cpY: LongInt; Obj: TSprite; Frame: LongInt): PTexture;
var X, Y, bpp, h, w, row, col, numFramesFirstCol: LongInt;
    p, pt: PLongWordArray;
    Image, finalSurface: PSDL_Surface;
begin
GetPlaceCollisionTex:= nil;
numFramesFirstCol:= SpritesData[Obj].imageHeight div SpritesData[Obj].Height;

checkFails(SpritesData[Obj].Surface <> nil, 'Assert SpritesData[Obj].Surface failed', true);
Image:= SpritesData[Obj].Surface;
w:= SpritesData[Obj].Width;
h:= SpritesData[Obj].Height;
row:= Frame mod numFramesFirstCol;
col:= Frame div numFramesFirstCol;

if SDL_MustLock(Image) then
    if SDLCheck(SDL_LockSurface(Image) >= 0, 'SDL_LockSurface', true) then
        exit;

bpp:= Image^.format^.BytesPerPixel;
checkFails(bpp = 4, 'It should be 32 bpp sprite', true);



finalSurface:= SDL_CreateRGBSurface(SDL_SWSURFACE, w, h, 32, RMask, GMask, BMask, AMask);

checkFails(finalSurface <> nil, 'GetPlaceCollisionTex: fail to create surface', true);

if SDL_MustLock(finalSurface) then
    SDLCheck(SDL_LockSurface(finalSurface) >= 0, 'GetPlaceCollisionTex', true);

if not allOK then
    begin
    if SDL_MustLock(Image) then
        SDL_UnlockSurface(Image);

    if SDL_MustLock(finalSurface) then
        SDL_UnlockSurface(finalSurface);

    if finalSurface <> nil then
        SDL_FreeSurface(finalSurface);
    end;

p:= PLongWordArray(@(PLongWordArray(Image^.pixels)^[ (Image^.pitch div 4) * row * h + col * w ]));
pt:= PLongWordArray(finalSurface^.pixels);

for y:= 0 to Pred(h) do
    begin
    for x:= 0 to Pred(w) do
        if ((p^[x] and AMask) <> 0)
            and (((cpY + y) < topY) or ((cpY + y) >= LAND_HEIGHT) or
            ((cpX + x) < leftX) or ((cpX + x) > rightX) or (LandGet(cpY + y, cpX + x) <> 0)) then
                pt^[x]:= cWhiteColor
        else
            (pt^[x]):= cWhiteColor and (not AMask);
    p:= PLongWordArray(@(p^[Image^.pitch div 4]));
    pt:= PLongWordArray(@(pt^[finalSurface^.pitch div 4]));
    end;

if SDL_MustLock(Image) then
    SDL_UnlockSurface(Image);

if SDL_MustLock(finalSurface) then
    SDL_UnlockSurface(finalSurface);

GetPlaceCollisionTex:= Surface2Tex(finalSurface, true);

SDL_FreeSurface(finalSurface);
end;


function Despeckle(X, Y: LongInt): boolean;
var nx, ny, i, j, c, xx, yy: LongInt;
    pixelsweep: boolean;

begin
    Despeckle:= true;

    if (cReducedQuality and rqBlurryLand) = 0 then
    begin
        xx:= X;
        yy:= Y;
    end
    else
    begin
        xx:= X div 2;
        yy:= Y div 2;
    end;

    pixelsweep:= (LandGet(Y, X) <= lfAllObjMask) and ((LandPixelGet(yy, xx) and AMask) <> 0);
    if (((LandGet(Y, X) and lfDamaged) <> 0) and ((LandGet(Y, X) and lfIndestructible) = 0)) or pixelsweep then
    begin
        c:= 0;
        for i:= -1 to 1 do
            for j:= -1 to 1 do
                if (i <> 0) or (j <> 0) then
                begin
                    ny:= Y + i;
                    nx:= X + j;
                    if ((ny and LAND_HEIGHT_MASK) = 0) and ((nx and LAND_WIDTH_MASK) = 0) then
                    begin
                        if pixelsweep then
                        begin
                            if ((cReducedQuality and rqBlurryLand) <> 0) then
                            begin
                                ny:= Y div 2 + i;
                                nx:= X div 2 + j;
                                if ((ny and (LAND_HEIGHT_MASK div 2)) = 0) and ((nx and (LAND_WIDTH_MASK div 2)) = 0) then
                                    if (LandPixelGet(ny, nx) and AMASK) <> 0 then
                                        inc(c);
                            end
                            else if (LandPixelGet(ny, nx) and AMASK)  <> 0 then
                                    inc(c);
                        end
                    else if LandGet(ny, nx) > 255 then
                        inc(c);
                    end
                end;

        if c < 4 then // 0-3 neighbours
        begin
            if ((LandGet(Y, X) and lfBasic) <> 0) and (not disableLandBack) then
                LandPixelSet(yy, xx, LandBackPixel(X, Y))
            else
                LandPixelSet(yy, xx, LandPixelGet(yy, xx) and (not AMASK));

            if not pixelsweep then
            begin
                LandSet(Y, X, 0);
                exit
            end
        end;
    end;
    Despeckle:= false
end;

// a bit of AA for explosions
procedure Smooth(X, Y: LongInt);
var c, r, g, b, a, i: integer;
    nx, ny: LongInt;
    pixel: LongWord;
begin

// only AA inwards
if (LandGet(Y, X) and lfDamaged) = 0 then
    exit;

// check location
if (Y <= topY + 1) or (Y >= LAND_HEIGHT-2)
or (X <= leftX + 1) or (X >= rightX - 1) then
    exit;

// counter for neighbor pixels that are not known to be undamaged
c:= 8;

// accumalating rgba value of relevant pixels here
r:= 0;
g:= 0;
b:= 0;
a:= 0;

// iterate over all neighbor pixels (also itself, will be skipped anyway)
for nx:= X-1 to X+1 do
    for ny:= Y-1 to Y+1 do
        // only consider undamaged neighbors (also leads to skipping itself)
        if (LandGet(ny, nx) and lfDamaged) = 0 then
            begin
            pixel:= LandPixelGet(ny, nx);
            inc(r, (pixel and RMask) shr RShift);
            inc(g, (pixel and GMask) shr GShift);
            inc(b, (pixel and BMask) shr BShift);
            inc(a, (pixel and AMask) shr AShift);
            dec(c);
            end;

// nothing do to if all neighbors damaged
if c < 1 then
    exit;

// use explosion color for damaged pixels
for i:= 1 to c do
    begin
    inc(r, ExplosionBorderColorR);
    inc(g, ExplosionBorderColorG);
    inc(b, ExplosionBorderColorB);
    inc(a, 255);
    end;

// set resulting color value based on average of all neighbors
r:= r div 8;
g:= g div 8;
b:= b div 8;
a:= a div 8;
LandPixelSet(y, x, (r shl RShift) or (g shl GShift) or (b shl BShift) or (a shl AShift));

end;

procedure Smooth_oldImpl(X, Y: LongInt);
begin
// a bit of AA for explosions
if (LandGet(Y, X) = 0) and (Y > topY + 1) and
    (Y < LAND_HEIGHT-2) and (X > leftX + 1) and (X < rightX - 1) then
    begin
    if ((((LandGet(y, x-1) and lfDamaged) <> 0) and (((LandGet(y+1,x) and lfDamaged) <> 0)) or ((LandGet(y-1,x) and lfDamaged) <> 0))
    or (((LandGet(y, x+1) and lfDamaged) <> 0) and (((LandGet(y-1,x) and lfDamaged) <> 0) or ((LandGet(y+1,x) and lfDamaged) <> 0)))) then
        begin
        if (cReducedQuality and rqBlurryLand) = 0 then
            begin
            if ((LandPixelGet(y,x) and AMask) shr AShift) < 10 then
                LandPixelSet(y,x, (ExplosionBorderColor and (not AMask)) or (128 shl AShift))
            else
                LandPixelSet(y,x,
                                (((((LandPixelGet(y,x) and RMask shr RShift) div 2)+((ExplosionBorderColor and RMask) shr RShift) div 2) and $FF) shl RShift) or
                                (((((LandPixelGet(y,x) and GMask shr GShift) div 2)+((ExplosionBorderColor and GMask) shr GShift) div 2) and $FF) shl GShift) or
                                (((((LandPixelGet(y,x) and BMask shr BShift) div 2)+((ExplosionBorderColor and BMask) shr BShift) div 2) and $FF) shl BShift) or ($FF shl AShift))
            end;
{
        if (LandGet(y, x-1) = lfObject) then
            LandSet(y,x, lfObject)
        else if (LandGet(y, x+1) = lfObject) then
            LandSet(y,x, lfObject)
        else
            LandSet(y,x, lfBasic);
}
        end
    else if ((((LandGet(y, x-1) and lfDamaged) <> 0) and ((LandGet(y+1,x-1) and lfDamaged) <> 0) and ((LandGet(y+2,x) and lfDamaged) <> 0))
    or (((LandGet(y, x-1) and lfDamaged) <> 0) and ((LandGet(y-1,x-1) and lfDamaged) <> 0) and ((LandGet(y-2,x) and lfDamaged) <> 0))
    or (((LandGet(y, x+1) and lfDamaged) <> 0) and ((LandGet(y+1,x+1) and lfDamaged) <> 0) and ((LandGet(y+2,x) and lfDamaged) <> 0))
    or (((LandGet(y, x+1) and lfDamaged) <> 0) and ((LandGet(y-1,x+1) and lfDamaged) <> 0) and ((LandGet(y-2,x) and lfDamaged) <> 0))
    or (((LandGet(y+1, x) and lfDamaged) <> 0) and ((LandGet(y+1,x+1) and lfDamaged) <> 0) and ((LandGet(y,x+2) and lfDamaged) <> 0))
    or (((LandGet(y-1, x) and lfDamaged) <> 0) and ((LandGet(y-1,x+1) and lfDamaged) <> 0) and ((LandGet(y,x+2) and lfDamaged) <> 0))
    or (((LandGet(y+1, x) and lfDamaged) <> 0) and ((LandGet(y+1,x-1) and lfDamaged) <> 0) and ((LandGet(y,x-2) and lfDamaged) <> 0))
    or (((LandGet(y-1, x) and lfDamaged) <> 0) and ((LandGet(y-1,x-1) and lfDamaged) <> 0) and ((LandGet(y,x-2) and lfDamaged) <> 0))) then
        begin
        if (cReducedQuality and rqBlurryLand) = 0 then
            begin
            if ((LandPixelGet(y,x) and AMask) shr AShift) < 10 then
                LandPixelSet(y,x, (ExplosionBorderColor and (not AMask)) or (64 shl AShift))
            else
                LandPixelSet(y,x,
                                (((((LandPixelGet(y,x) and RMask shr RShift) * 3 div 4)+((ExplosionBorderColor and RMask) shr RShift) div 4) and $FF) shl RShift) or
                                (((((LandPixelGet(y,x) and GMask shr GShift) * 3 div 4)+((ExplosionBorderColor and GMask) shr GShift) div 4) and $FF) shl GShift) or
                                (((((LandPixelGet(y,x) and BMask shr BShift) * 3 div 4)+((ExplosionBorderColor and BMask) shr BShift) div 4) and $FF) shl BShift) or ($FF shl AShift))
            end;
{
        if (LandGet(y, x-1) = lfObject) then
            LandGet(y, x):= lfObject
        else if (LandGet(y, x+1) = lfObject) then
            LandGet(y, x):= lfObject
        else if (LandGet(y+1, x) = lfObject) then
            LandGet(y, x):= lfObject
        else if (LandGet(y-1, x) = lfObject) then
        LandGet(y, x):= lfObject
        else LandGet(y,x):= lfBasic
}
        end
    end
else if ((cReducedQuality and rqBlurryLand) = 0) and ((LandPixelGet(Y, X) and AMask) = AMask)
and (LandGet(Y, X) and (lfDamaged or lfBasic) = lfBasic)
and (Y > topY + 1) and (Y < LAND_HEIGHT-2) and (X > leftX + 1) and (X < rightX - 1) then
    begin
    if ((((LandGet(y, x-1) and lfDamaged) <> 0) and (((LandGet(y+1,x) and lfDamaged) <> 0)) or ((LandGet(y-1,x) and lfDamaged) <> 0))
    or (((LandGet(y, x+1) and lfDamaged) <> 0) and (((LandGet(y-1,x) and lfDamaged) <> 0) or ((LandGet(y+1,x) and lfDamaged) <> 0)))) then
        begin
        LandPixelSet(y,x,
                        (((((LandPixelGet(y,x) and RMask shr RShift) div 2)+((ExplosionBorderColor and RMask) shr RShift) div 2) and $FF) shl RShift) or
                        (((((LandPixelGet(y,x) and GMask shr GShift) div 2)+((ExplosionBorderColor and GMask) shr GShift) div 2) and $FF) shl GShift) or
                        (((((LandPixelGet(y,x) and BMask shr BShift) div 2)+((ExplosionBorderColor and BMask) shr BShift) div 2) and $FF) shl BShift) or ($FF shl AShift))
        end
    else if ((((LandGet(y, x-1) and lfDamaged) <> 0) and ((LandGet(y+1,x-1) and lfDamaged) <> 0) and ((LandGet(y+2,x) and lfDamaged) <> 0))
    or (((LandGet(y, x-1) and lfDamaged) <> 0) and ((LandGet(y-1,x-1) and lfDamaged) <> 0) and ((LandGet(y-2,x) and lfDamaged) <> 0))
    or (((LandGet(y, x+1) and lfDamaged) <> 0) and ((LandGet(y+1,x+1) and lfDamaged) <> 0) and ((LandGet(y+2,x) and lfDamaged) <> 0))
    or (((LandGet(y, x+1) and lfDamaged) <> 0) and ((LandGet(y-1,x+1) and lfDamaged) <> 0) and ((LandGet(y-2,x) and lfDamaged) <> 0))
    or (((LandGet(y+1, x) and lfDamaged) <> 0) and ((LandGet(y+1,x+1) and lfDamaged) <> 0) and ((LandGet(y,x+2) and lfDamaged) <> 0))
    or (((LandGet(y-1, x) and lfDamaged) <> 0) and ((LandGet(y-1,x+1) and lfDamaged) <> 0) and ((LandGet(y,x+2) and lfDamaged) <> 0))
    or (((LandGet(y+1, x) and lfDamaged) <> 0) and ((LandGet(y+1,x-1) and lfDamaged) <> 0) and ((LandGet(y,x-2) and lfDamaged) <> 0))
    or (((LandGet(y-1, x) and lfDamaged) <> 0) and ((LandGet(y-1,x-1) and lfDamaged) <> 0) and ((LandGet(y,x-2) and lfDamaged) <> 0))) then
        begin
        LandPixelSet(y,x,
                        (((((LandPixelGet(y,x) and RMask shr RShift) * 3 div 4)+((ExplosionBorderColor and RMask) shr RShift) div 4) and $FF) shl RShift) or
                        (((((LandPixelGet(y,x) and GMask shr GShift) * 3 div 4)+((ExplosionBorderColor and GMask) shr GShift) div 4) and $FF) shl GShift) or
                        (((((LandPixelGet(y,x) and BMask shr BShift) * 3 div 4)+((ExplosionBorderColor and BMask) shr BShift) div 4) and $FF) shl BShift) or ($FF shl AShift))
        end
    end
end;

function SweepDirty: boolean;
var x, y, xx, yy, ty, tx: LongInt;
    bRes, resweep, recheck: boolean;
begin
bRes:= false;
reCheck:= true;

while recheck do
    begin
    recheck:= false;
    for y:= 0 to LAND_HEIGHT div 32 - 1 do
        begin
        for x:= 0 to LAND_WIDTH div 32 - 1 do
            begin
            if LandDirty[y, x] = 1 then
                begin
                resweep:= true;
                ty:= y * 32;
                tx:= x * 32;
                while(resweep) do
                    begin
                    resweep:= false;
                    for yy:= ty to ty + 31 do
                        for xx:= tx to tx + 31 do
                            if Despeckle(xx, yy) then
                                begin
                                bRes:= true;
                                resweep:= true;
                                if (yy = ty) and (y > 0) then
                                    begin
                                    LandDirty[y-1, x]:= 1;
                                    recheck:= true;
                                    end
                                else if (yy = ty+31) and (y < LAND_HEIGHT div 32 - 1) then
                                    begin
                                    LandDirty[y+1, x]:= 1;
                                    recheck:= true;
                                    end;
                                if (xx = tx) and (x > 0) then
                                    begin
                                    LandDirty[y, x-1]:= 1;
                                    recheck:= true;
                                    end
                                else if (xx = tx+31) and (x < LAND_WIDTH div 32 - 1) then
                                    begin
                                    LandDirty[y, x+1]:= 1;
                                    recheck:= true;
                                    end
                                end;
                    end;
                end;
            end;
        end;
     end;

// smooth explosion borders (except if land is blurry)
if (cReducedQuality and rqBlurryLand) = 0 then
    for y:= 0 to LAND_HEIGHT div 32 - 1 do
        for x:= 0 to LAND_WIDTH div 32 - 1 do
            if LandDirty[y, x] <> 0 then
                begin
                ty:= y * 32;
                tx:= x * 32;
                for yy:= ty to ty + 31 do
                    for xx:= tx to tx + 31 do
                        Smooth(xx,yy)
                end;

for y:= 0 to LAND_HEIGHT div 32 - 1 do
    for x:= 0 to LAND_WIDTH div 32 - 1 do
        if LandDirty[y, x] <> 0 then
            begin
            LandDirty[y, x]:= 0;
            ty:= y * 32;
            tx:= x * 32;
            UpdateLandTexture(tx, 32, ty, 32, false);
            end;

SweepDirty:= bRes;
end;


// Return true if outside of land or not the value tested, used right now for some X/Y movement that does not use normal hedgehog movement in GSHandlers.inc
function CheckLandValue(X, Y: LongInt; LandFlag: Word): boolean;
begin
    CheckLandValue:= ((X and LAND_WIDTH_MASK <> 0) or (Y and LAND_HEIGHT_MASK <> 0)) or ((LandGet(Y, X) and LandFlag) = 0)
end;

function LandBackPixel(x, y: LongInt): LongWord;
var p: PLongWordArray;
begin
    if LandBackSurface = nil then
        LandBackPixel:= 0
    else
        begin
        p:= LandBackSurface^.pixels;
        LandBackPixel:= p^[LandBackSurface^.w * (y mod LandBackSurface^.h) + (x mod LandBackSurface^.w)];// or $FF000000;
        end
end;


procedure DrawLine(X1, Y1, X2, Y2: LongInt; Color: Longword);
var
  eX, eY, dX, dY: LongInt;
  i, sX, sY, x, y, d: LongInt;
begin
eX:= 0;
eY:= 0;
dX:= X2 - X1;
dY:= Y2 - Y1;

if (dX > 0) then
    sX:= 1
else
    if (dX < 0) then
        begin
        sX:= -1;
        dX:= -dX
        end
    else
        sX:= dX;

if (dY > 0) then
    sY:= 1
else
    if (dY < 0) then
        begin
        sY:= -1;
        dY:= -dY
        end
    else
        sY:= dY;

if (dX > dY) then
    d:= dX
else
    d:= dY;

x:= X1;
y:= Y1;

for i:= 0 to d do
    begin
    inc(eX, dX);
    inc(eY, dY);
    if (eX > d) then
        begin
        dec(eX, d);
        inc(x, sX);
        end;
    if (eY > d) then
        begin
        dec(eY, d);
        inc(y, sY);
        end;

    if ((x and LAND_WIDTH_MASK) = 0) and ((y and LAND_HEIGHT_MASK) = 0) then
        LandSet(y, x, Color);
    end
end;

function DrawDots(x, y, xx, yy: Longint; Color: Longword): Longword;
begin
    DrawDots:= 0;

    if (((x + xx) and LAND_WIDTH_MASK) = 0) and (((y + yy) and LAND_HEIGHT_MASK) = 0) and (LandGet(y + yy, x + xx) <> Color) then
        begin inc(DrawDots); LandSet(y + yy, x + xx, Color); end;
    if (((x + xx) and LAND_WIDTH_MASK) = 0) and (((y - yy) and LAND_HEIGHT_MASK) = 0) and (LandGet(y - yy, x + xx) <> Color) then
        begin inc(DrawDots); LandSet(y - yy, x + xx, Color); end;
    if (((x - xx) and LAND_WIDTH_MASK) = 0) and (((y + yy) and LAND_HEIGHT_MASK) = 0) and (LandGet(y + yy, x - xx) <> Color) then
        begin inc(DrawDots); LandSet(y + yy, x - xx, Color); end;
    if (((x - xx) and LAND_WIDTH_MASK) = 0) and (((y - yy) and LAND_HEIGHT_MASK) = 0) and (LandGet(y - yy, x - xx) <> Color) then
        begin inc(DrawDots); LandSet(y - yy, x - xx, Color); end;
    if (((x + yy) and LAND_WIDTH_MASK) = 0) and (((y + xx) and LAND_HEIGHT_MASK) = 0) and (LandGet(y + xx, x + yy) <> Color) then
        begin inc(DrawDots); LandSet(y + xx, x + yy, Color); end;
    if (((x + yy) and LAND_WIDTH_MASK) = 0) and (((y - xx) and LAND_HEIGHT_MASK) = 0) and (LandGet(y - xx, x + yy) <> Color) then
        begin inc(DrawDots); LandSet(y - xx, x + yy, Color); end;
    if (((x - yy) and LAND_WIDTH_MASK) = 0) and (((y + xx) and LAND_HEIGHT_MASK) = 0) and (LandGet(y + xx, x - yy) <> Color) then
        begin inc(DrawDots); LandSet(y + xx, x - yy, Color); end;
    if (((x - yy) and LAND_WIDTH_MASK) = 0) and (((y - xx) and LAND_HEIGHT_MASK) = 0) and (LandGet(y - xx, x - yy) <> Color) then
        begin inc(DrawDots); LandSet(y - xx, x - yy, Color); end;
end;

function DrawLines(X1, Y1, X2, Y2, XX, YY: LongInt; color: Longword): Longword;
var
  eX, eY, dX, dY: LongInt;
  i, sX, sY, x, y, d: LongInt;
  f: boolean;
begin
    eX:= 0;
    eY:= 0;
    dX:= X2 - X1;
    dY:= Y2 - Y1;
    DrawLines:= 0;

    if (dX > 0) then
        sX:= 1
    else
        if (dX < 0) then
            begin
            sX:= -1;
            dX:= -dX
            end
        else
            sX:= dX;

    if (dY > 0) then
        sY:= 1
    else
        if (dY < 0) then
            begin
            sY:= -1;
            dY:= -dY
            end
        else
            sY:= dY;

    if (dX > dY) then
        d:= dX
    else
        d:= dY;

    x:= X1;
    y:= Y1;

    for i:= 0 to d do
        begin
        inc(eX, dX);
        inc(eY, dY);

        f:= eX > d;
        if f then
            begin
            dec(eX, d);
            inc(x, sX);
            inc(DrawLines, DrawDots(x, y, xx, yy, color))
            end;
        if (eY > d) then
            begin
            dec(eY, d);
            inc(y, sY);
            f:= true;
            inc(DrawLines, DrawDots(x, y, xx, yy, color))
            end;

        if not f then
            inc(DrawLines, DrawDots(x, y, xx, yy, color))
        end
end;

function DrawThickLine(X1, Y1, X2, Y2, radius: LongInt; color: Longword): Longword;
var dx, dy, d: LongInt;
begin
    DrawThickLine:= 0;

    dx:= 0;
    dy:= Radius;
    d:= 3 - 2 * Radius;
    while (dx < dy) do
        begin
        inc(DrawThickLine, DrawLines(x1, y1, x2, y2, dx, dy, color));
        if (d < 0) then
            d:= d + 4 * dx + 6
        else
            begin
            d:= d + 4 * (dx - dy) + 10;
            dec(dy)
            end;
        inc(dx)
        end;
    if (dx = dy) then
        inc(DrawThickLine, DrawLines(x1, y1, x2, y2, dx, dy, color));
end;


procedure DumpLandToLog(x, y, r: LongInt);
var xx, yy, dx: LongInt;
    s: shortstring;
begin
    s[0]:= char(r * 2 + 1);
    for yy:= y - r to y + r do
        begin
        for dx:= 0 to r*2 do
            begin
            xx:= dx - r + x;
            if (xx = x) and (yy = y) then
                s[dx + 1]:= 'X'
            else if LandGet(yy, xx) > 255 then
                s[dx + 1]:= 'O'
            else if LandGet(yy, xx) > 0 then
                s[dx + 1]:= '*'
            else
                s[dx + 1]:= '.'
            end;
        AddFileLog('Land dump: ' + s);
        end;
end;

end.
