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
unit uGearsList;

interface
uses uFloat, uTypes, SDLh;

procedure initializeGear(gear: PGear; X, Y: LongInt; Kind: TGearType; State: Longword; dX, dY: hwFloat; Timer, newUid: LongWord);
function  AddGear(X, Y: LongInt; Kind: TGearType; State: Longword; dX, dY: hwFloat; Timer: LongWord): PGear;
function  AddGear(X, Y: LongInt; Kind: TGearType; State: Longword; dX, dY: hwFloat; Timer, newUid: LongWord): PGear;
procedure DeleteGear(Gear: PGear);
procedure InsertGearToList(Gear: PGear);
procedure RemoveGearFromList(Gear: PGear);

var curHandledGear: PGear;

implementation

uses uRandom, uUtils, uConsts, uVariables, uAmmos, uTeams, uStats,
    uTextures, uScript, uRenderUtils, uAI, uCollisions,
    uGearsRender, uGearsUtils, uDebug;

const
    GearKindAmmoTypeMap : array [TGearType] of TAmmoType = (
(*          gtFlame *)   amNothing
(*       gtHedgehog *) , amNothing
(*           gtMine *) , amMine
(*           gtCase *) , amNothing
(*        gtAirMine *) , amAirMine
(*     gtExplosives *) , amNothing
(*        gtGrenade *) , amGrenade
(*          gtShell *) , amBazooka
(*          gtGrave *) , amNothing
(*            gtBee *) , amBee
(*    gtShotgunShot *) , amShotgun
(*     gtPickHammer *) , amPickHammer
(*           gtRope *) , amRope
(*     gtDEagleShot *) , amDEagle
(*       gtDynamite *) , amDynamite
(*    gtClusterBomb *) , amClusterBomb
(*        gtCluster *) , amClusterBomb
(*         gtShover *) , amBaseballBat  // Shover is only used for baseball bat right now
(*      gtFirePunch *) , amFirePunch
(*    gtATStartGame *) , amNothing
(*   gtATFinishGame *) , amNothing
(*      gtParachute *) , amParachute
(*      gtAirAttack *) , amAirAttack
(*        gtAirBomb *) , amAirAttack
(*      gtBlowTorch *) , amBlowTorch
(*         gtGirder *) , amGirder
(*       gtTeleport *) , amTeleport
(*       gtSwitcher *) , amSwitch
(*         gtTarget *) , amNothing
(*         gtMortar *) , amMortar
(*           gtWhip *) , amWhip
(*       gtKamikaze *) , amKamikaze
(*           gtCake *) , amCake
(*      gtSeduction *) , amSeduction
(*     gtWatermelon *) , amWatermelon
(*     gtMelonPiece *) , amWatermelon
(*    gtHellishBomb *) , amHellishBomb
(*        gtWaterUp *) , amNothing
(*          gtDrill *) , amDrill
(*        gtBallGun *) , amBallgun
(*           gtBall *) , amBallgun
(*        gtRCPlane *) , amRCPlane
(*gtSniperRifleShot *) , amSniperRifle
(*        gtJetpack *) , amJetpack
(*        gtMolotov *) , amMolotov
(*          gtBirdy *) , amBirdy
(*            gtEgg *) , amBirdy
(*         gtPortal *) , amPortalGun
(*          gtPiano *) , amPiano
(*        gtGasBomb *) , amGasBomb
(*    gtSineGunShot *) , amSineGun
(*   gtFlamethrower *) , amFlamethrower
(*          gtSMine *) , amSMine
(*    gtPoisonCloud *) , amNothing
(*         gtHammer *) , amHammer
(*      gtHammerHit *) , amHammer
(*    gtResurrector *) , amResurrector
(*    gtPoisonCloud *) , amNothing
(*       gtSnowball *) , amSnowball
(*          gtFlake *) , amNothing
(*        gtLandGun *) , amLandGun
(*         gtTardis *) , amTardis
(*         gtIceGun *) , amIceGun
(*        gtAddAmmo *) , amNothing
(*  gtGenericFaller *) , amNothing
(*          gtKnife *) , amKnife
(*        gtCreeper *) , amCreeper
(*        gtMinigun *) , amMinigun
(*  gtMinigunBullet *) , amMinigun
(*         gtSentry *) , amSentry
    );


var GCounter: LongWord = 0; // this does not get re-initialized, but should be harmless

const
    cUsualZ = 500;
    cOnHHZ = 2000;

procedure InsertGearToList(Gear: PGear);
var tmp, ptmp: PGear;
begin
    tmp:= GearsList;
    ptmp:= GearsList;
    while (tmp <> nil) and (tmp^.Z < Gear^.Z) do
        begin
        ptmp:= tmp;
        tmp:= tmp^.NextGear
        end;

    if ptmp <> tmp then
        begin
        Gear^.NextGear:= ptmp^.NextGear;
        Gear^.PrevGear:= ptmp;
        if ptmp^.NextGear <> nil then
            ptmp^.NextGear^.PrevGear:= Gear;
        ptmp^.NextGear:= Gear
        end
    else
        begin
        Gear^.NextGear:= GearsList;
        if Gear^.NextGear <> nil then
            Gear^.NextGear^.PrevGear:= Gear;
        GearsList:= Gear;
        end;
end;


procedure RemoveGearFromList(Gear: PGear);
begin
if (Gear <> GearsList) and (Gear <> nil) and (Gear^.NextGear = nil) and (Gear^.PrevGear = nil) then
    begin
    AddFileLog('Attempted to remove Gear #'+inttostr(Gear^.uid)+' from the list twice.');
    exit
    end;
    
checkFails((Gear = nil) or (curHandledGear = nil) or (Gear = curHandledGear), 'You''re doing it wrong', true);

if Gear^.NextGear <> nil then
    Gear^.NextGear^.PrevGear:= Gear^.PrevGear;
if Gear^.PrevGear <> nil then
    Gear^.PrevGear^.NextGear:= Gear^.NextGear
else
    GearsList:= Gear^.NextGear;

Gear^.NextGear:= nil;
Gear^.PrevGear:= nil
end;

procedure initializeGear(gear: PGear; X, Y: LongInt; Kind: TGearType; State: Longword; dX, dY: hwFloat; Timer, newUid: LongWord);
var cakeData: PCakeData;
begin
    FillChar(gear^, sizeof(TGear), 0);
    gear^.X:= int2hwFloat(X);
    gear^.Y:= int2hwFloat(Y);
    gear^.Target.X:= NoPointX;
    gear^.Kind := Kind;
    gear^.State:= State;
    gear^.Active:= true;
    gear^.dX:= dX;
    gear^.dY:= dY;
    gear^.doStep:= doStepHandlers[Kind];
    gear^.CollisionIndex:= -1;
    gear^.Timer:= Timer;
    if newUid = 0 then
         gear^.uid:= GCounter
    else gear^.uid:= newUid;
    gear^.SoundChannel:= -1;
    gear^.ImpactSound:= sndNone;
    gear^.Density:= _1;
    // Define ammo association, if any.
    gear^.AmmoType:= GearKindAmmoTypeMap[Kind];
    gear^.CollisionMask:= lfAll;
    gear^.Tint:= $FFFFFFFF;
    gear^.Data:= nil;
    gear^.Sticky:= false;

    if CurrentHedgehog <> nil then
        begin
        gear^.Hedgehog:= CurrentHedgehog;
        if (CurrentHedgehog^.Gear <> nil) and (hwRound(CurrentHedgehog^.Gear^.X) = X) and (hwRound(CurrentHedgehog^.Gear^.Y) = Y) then
            gear^.CollisionMask:= lfNotCurHogCrate
        end;

    if (Ammoz[Gear^.AmmoType].Ammo.Propz and ammoprop_NeedTarget <> 0) then
        gear^.Z:= cHHZ+1
    else gear^.Z:= cUsualZ;

    // set gstInBounceEdge if gear spawned inside the bounce world edge
    if WorldEdge = weBounce then
        if (hwRound(gear^.X) - Gear^.Radius < leftX) or (hwRound(gear^.X) + Gear^.Radius > rightX) then
            case gear^.Kind of
                // list all gears here that could collide with the bounce world edge
                gtHedgehog,
                gtFlame,
                gtMine,
                gtAirBomb,
                gtDrill,
                gtNapalmBomb,
                gtCase,
                gtAirMine,
                gtExplosives,
                gtGrenade,
                gtShell,
                gtBee,
                gtDynamite,
                gtClusterBomb,
                gtMelonPiece,
                gtCluster,
                gtMortar,
                gtKamikaze,
                gtCake,
                gtWatermelon,
                gtGasBomb,
                gtHellishBomb,
                gtBall,
                gtRCPlane,
                gtSniperRifleShot,
                gtShotgunShot,
                gtDEagleShot,
                gtSineGunShot,
                gtMinigunBullet,
                gtEgg,
                gtPiano,
                gtSMine,
                gtSnowball,
                gtKnife,
                gtCreeper,
                gtSentry,
                gtMolotov,
                gtFlake,
                gtGrave,
                gtPortal,
                gtTarget:
                gear^.State := gear^.State or gstInBounceEdge;
            end;

    case Kind of
              gtFlame: Gear^.Boom := 2;  // some additional expl in there are x3, x4 this
           gtHedgehog: Gear^.Boom := 30;
               gtMine: Gear^.Boom := 50;
               gtCase: Gear^.Boom := 25;
            gtAirMine: Gear^.Boom := 30;
         gtExplosives: Gear^.Boom := 75;
            gtGrenade: Gear^.Boom := 50;
              gtShell: Gear^.Boom := 50;
                gtBee: Gear^.Boom := 50;
        gtShotgunShot: Gear^.Boom := 25;
         gtPickHammer: Gear^.Boom := 6;
    //           gtRope: Gear^.Boom := 2; could be funny to have rope attaching to hog deal small amount of dmg?
         gtDEagleShot: Gear^.Boom := 7;
           gtDynamite: Gear^.Boom := 75;
        gtClusterBomb: Gear^.Boom := 20;
         gtMelonPiece,
            gtCluster: Gear^.Boom := Timer;
             gtShover: Gear^.Boom := 30;
          gtFirePunch: Gear^.Boom := 30;
            gtAirBomb: Gear^.Boom := 30;
          gtBlowTorch: Gear^.Boom := 2;
             gtMortar: Gear^.Boom := 20;
               gtWhip: Gear^.Boom := 30;
           gtKamikaze: Gear^.Boom := 30; // both shove and explosion
               gtCake: Gear^.Boom := cakeDmg; // why is cake damage a global constant
         gtWatermelon: Gear^.Boom := 75;
        gtHellishBomb: Gear^.Boom := 90;
              gtDrill: if Gear^.State and gsttmpFlag = 0 then
                            Gear^.Boom := 50
                       else Gear^.Boom := 30;
               gtBall: Gear^.Boom := 40;
            gtRCPlane: Gear^.Boom := 25;
    // sniper rifle is distance linked, this Boom is just an arbitrary scaling factor applied to timer-based-damage
    // because, eh, why not..
    gtSniperRifleShot: Gear^.Boom := 100000;
                gtEgg: Gear^.Boom := 10;
              gtPiano: Gear^.Boom := 80;
            gtGasBomb: Gear^.Boom := 20;
        gtSineGunShot: Gear^.Boom := 35;
              gtSMine: Gear^.Boom := 30;
        gtSnowball: Gear^.Boom := 200000; // arbitrary scaling for the shove
             gtHammer: if cDamageModifier > _1 then // scale it based on cDamageModifier?
                             Gear^.Boom := 2
                        else Gear^.Boom := 3;
        gtPoisonCloud: Gear^.Boom := 20;
              gtKnife: Gear^.Boom := 40000; // arbitrary scaling factor since impact-based
            gtCreeper: Gear^.Boom := 100;
      gtMinigunBullet: Gear^.Boom := 2;
             gtSentry: Gear^.Boom := 40;
        end;

    case Kind of
         gtGrenade,
         gtClusterBomb,
         gtGasBomb: begin
                    gear^.ImpactSound:= sndGrenadeImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.AdvBounce:= 1;
                    gear^.Radius:= 5;
                    gear^.Elasticity:= _0_8;
                    gear^.Friction:= _0_8;
                    gear^.Density:= _1_5;
                    gear^.RenderTimer:= true;
                    if gear^.Timer = 0 then
                        gear^.Timer:= 3000
                    end;
      gtWatermelon: begin
                    gear^.ImpactSound:= sndMelonImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.AdvBounce:= 1;
                    gear^.Radius:= 6;
                    gear^.Elasticity:= _0_8;
                    gear^.Friction:= _0_995;
                    gear^.Density:= _2;
                    gear^.RenderTimer:= true;
                    if gear^.Timer = 0 then
                        gear^.Timer:= 3000
                    end;
      gtMelonPiece: begin
                    gear^.AdvBounce:= 1;
                    gear^.Density:= _2;
                    gear^.Elasticity:= _0_8;
                    gear^.Friction:= _0_995;
                    gear^.Radius:= 4
                    end;
        gtHedgehog: begin
                    gear^.AdvBounce:= 1;
                    gear^.Radius:= cHHRadius;
                    gear^.Elasticity:= _0_35;
                    gear^.Friction:= _0_999;
                    gear^.Angle:= cMaxAngle div 2;
                    gear^.Density:= _3;
                    gear^.Z:= cHHZ;
                    if (GameFlags and gfAISurvival) <> 0 then
                        if gear^.Hedgehog^.BotLevel > 0 then
                            gear^.Hedgehog^.Effects[heResurrectable] := 1;
                    if (GameFlags and gfArtillery) <> 0 then
                        gear^.Hedgehog^.Effects[heArtillery] := 1;
                    // this would presumably be set in the frontend
                    // if we weren't going to do that yet, would need to reinit GetRandom
                    // oh, and, randomising slightly R and B might be nice too.
                    //gear^.Tint:= $fa00efff or ((random(80)+128) shl 16)
                    //gear^.Tint:= $faa4efff
                    //gear^.Tint:= (($e0+random(32)) shl 24) or
                    //             ((random(80)+128) shl 16) or
                    //             (($d5+random(32)) shl 8) or $ff
                    {c:= GetRandom(32);
                    gear^.Tint:= (($e0+c) shl 24) or
                                 ((GetRandom(90)+128) shl 16) or
                                 (($d5+c) shl 8) or $ff}
                    end;
       gtParachute: begin
                    gear^.Tag:= 1; // hog face dir. 1 = right, -1 = left
                    gear^.Z:= cCurrHHZ;
                    end;
           gtShell: begin
                    gear^.Elasticity:= _0_8;
                    gear^.Friction:= _0_8;
                    gear^.Radius:= 4;
                    gear^.Density:= _1;
                    gear^.AdvBounce:= 1;
                    end;
           gtSnowball: begin
                    gear^.ImpactSound:= sndMudballImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.Radius:= 4;
                    gear^.Density:= _0_5;
                    gear^.AdvBounce:= 1;
                    gear^.Elasticity:= _0_8;
                    gear^.Friction:= _0_8;
                    end;

         gtFlake: begin
                    with Gear^ do
                        begin
                        Pos:= 0;
                        Radius:= 1;
                        DirAngle:= random(360);
                        Sticky:= true;
                        if State and gstTmpFlag = 0 then
                            begin
                            dx.isNegative:= GetRandom(2) = 0;
                            dx.QWordValue:= QWord($40DA) * GetRandom(10000) * 8;
                            dy.isNegative:= false;
                            dy.QWordValue:= QWord($3AD3) * GetRandom(7000) * 8;
                            if GetRandom(2) = 0 then
                                dx := -dx;
                            Tint:= $FFFFFFFF
                            end
                        else
                            Tint:= (ExplosionBorderColor shr RShift and $FF shl 24) or
                                   (ExplosionBorderColor shr GShift and $FF shl 16) or
                                   (ExplosionBorderColor shr BShift and $FF shl 8) or $FF;
                        State:= State or gstInvisible;
                        // use health field to store current frameticks
                        if vobFrameTicks > 0 then
                            Health:= random(vobFrameTicks)
                        else
                            Health:= 0;
                        // use timer to store currently displayed frame index
                        if gear^.Timer = 0 then Timer:= random(vobFramesCount);
                        Damage:= (random(2) * 2 - 1) * (vobVelocity + random(vobVelocity)) * 8
                        end
                    end;
           gtGrave: begin
                    gear^.ImpactSound:= sndGraveImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.Radius:= 10;
                    gear^.Elasticity:= _0_6;
                    gear^.Z:= 1;
                    end;
             gtBee: begin
                    gear^.Radius:= 5;
                    if gear^.Timer = 0 then gear^.Timer:= 500;
                    gear^.RenderTimer:= true;
                    gear^.Elasticity:= _0_9;
                    gear^.Tag:= 0;
                    gear^.State:= Gear^.State or gstSubmersible
                    end;
       gtSeduction: begin
                    gear^.Radius:= cSeductionDist;
                    end;
     gtShotgunShot: begin
                    if gear^.Timer = 0 then gear^.Timer:= 900;
                    gear^.Radius:= 2
                    end;
      gtPickHammer: begin
                    gear^.Radius:= 10;
                    if gear^.Timer = 0 then gear^.Timer:= 4000
                    end;
       gtHammerHit: begin
                    gear^.Radius:= 8;
                    if gear^.Timer = 0 then gear^.Timer:= 125
                    end;
            gtRope: begin
                    gear^.Radius:= 3;
                    gear^.Friction:= _450 * _0_01 * cRopePercent;
                    RopePoints.Count:= 0;
                    gear^.Tint:= $D8D8D8FF;
                    gear^.Tag:= 0; // normal rope render
                    gear^.CollisionMask:= lfNotCurHogCrate //lfNotObjMask or lfNotHHObjMask;
                    end;
            gtMine: begin
                    gear^.ImpactSound:= sndMineImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.Health:= 10;
                    gear^.State:= gear^.State or gstMoving;
                    gear^.Radius:= 2;
                    gear^.Elasticity:= _0_55;
                    gear^.Friction:= _0_995;
                    gear^.Density:= _1;
                    if gear^.Timer = 0 then
                        begin
                        if cMinesTime < 0 then
                            begin
                            gear^.Timer:= getrandom(51)*100;
                            gear^.Karma:= 1;
                            end
                        else
                            gear^.Timer:= cMinesTime;
                        end;
                    gear^.RenderTimer:= true;
                    end;
         gtAirMine: begin
                    gear^.AdvBounce:= 1;
                    gear^.ImpactSound:= sndAirMineImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.Health:= 30;
                    gear^.State:= gear^.State or gstMoving or gstNoGravity or gstSubmersible;
                    gear^.Radius:= 8;
                    gear^.Elasticity:= _0_55;
                    gear^.Friction:= _0_995;
                    gear^.Density:= _1;
                    gear^.Angle:= 175; // Radius at which air bombs will start "seeking". $FFFFFFFF = unlimited. check is skipped.
                    gear^.Power:= cMaxWindSpeed.QWordValue div 2; // hwFloat converted. 1/2 g default. defines the "seek" speed when a gear is in range.
                    gear^.Pos:= cMaxWindSpeed.QWordValue * 3 div 2; // air friction. slows it down when not hitting stuff
                    gear^.Tag:= 0;
                    if gear^.Timer = 0 then
                        begin
                        if cMinesTime < 0 then
                            begin
                            gear^.Timer:= getrandom(13)*100;
                            gear^.Karma:= 1;
                            end
                        else
                            gear^.Timer:= cMinesTime div 4;
                        end;
                    gear^.RenderTimer:= true;
                    gear^.WDTimer:= gear^.Timer
                    end;
           gtSMine: begin
                    gear^.Health:= 10;
                    gear^.State:= gear^.State or gstMoving;
                    gear^.Radius:= 2;
                    gear^.Elasticity:= _0_55;
                    gear^.Friction:= _0_995;
                    gear^.Density:= _1_6;
                    gear^.AdvBounce:= 1;
                    gear^.Sticky:= true;
                    if gear^.Timer = 0 then gear^.Timer:= 500;
                    gear^.RenderTimer:= true;
                    end;
           gtKnife: begin
                    gear^.ImpactSound:= sndKnifeImpact;
                    gear^.AdvBounce:= 1;
                    gear^.Elasticity:= _0_8;
                    gear^.Friction:= _0_8;
                    gear^.Density:= _4;
                    gear^.Radius:= 7;
                    gear^.Sticky:= true;
                    end;
            gtCase: begin
                    gear^.ImpactSound:= sndCaseImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.Radius:= 16;
                    gear^.Elasticity:= _0_3;
                    if gear^.Timer = 0 then gear^.Timer:= 500
                    end;
      gtExplosives: begin
                    gear^.AdvBounce:= 1;
                    if GameType in [gmtDemo, gmtRecord] then
                        gear^.RenderHealth:= true;
                    gear^.ImpactSound:= sndGrenadeImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.Radius:= 16;
                    gear^.Elasticity:= _0_4;
                    gear^.Friction:= _0_995;
                    gear^.Density:= _6;
                    gear^.Health:= cBarrelHealth;
                    gear^.Z:= cHHZ-1
                    end;
      gtDEagleShot: begin
                    gear^.Radius:= 1;
                    gear^.Health:= 50;
                    gear^.Data:= nil;
                    end;
      gtSniperRifleShot: begin
                    gear^.Radius:= 1;
                    gear^.Health:= 50
                    end;
        gtDynamite: begin
                    gear^.ImpactSound:= sndDynamiteImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.Radius:= 3;
                    gear^.Elasticity:= _0_55;
                    gear^.Friction:= _0_03;
                    gear^.Density:= _2;
                    if gear^.Timer = 0 then gear^.Timer:= 5000;
                    end;
         gtCluster: begin
                    gear^.AdvBounce:= 1;
                    gear^.Elasticity:= _0_8;
                    gear^.Friction:= _0_8;
                    gear^.Radius:= 2;
                    gear^.Density:= _1_5;
                    gear^.RenderTimer:= true
                    end;
          gtShover: begin
                    gear^.Radius:= 20;
                    gear^.Tag:= 0;
                    gear^.Timer:= 50;
                    end;
           gtFlame: begin
                    gear^.Tag:= GetRandom(32);
                    gear^.Radius:= 1;
                    gear^.Health:= 5;
                    gear^.Density:= _1;
                    gear^.FlightTime:= 9999999; // determines whether in-air flames do damage. disabled by default
                    if (gear^.dY.QWordValue = 0) and (gear^.dX.QWordValue = 0) then
                        begin
                        gear^.dY:= (getrandomf - _0_8) * _0_03;
                        gear^.dX:= (getrandomf - _0_5) * _0_4
                        end
                    end;
       gtFirePunch: begin
                    if gear^.Timer = 0 then gear^.Timer:= 3000;
                    gear^.Radius:= 15;
                    gear^.Tag:= Y
                    end;
       gtAirAttack: begin
                    gear^.Health:= 6;
                    gear^.Damage:= 30;
                    gear^.Z:= cHHZ+2;
                    gear^.Karma:= 0; // for sound effect: 0 = normal, 1 = underwater
                    gear^.Radius:= 150;
                    gear^.FlightTime:= 0; // for timeout in weWrap
                    gear^.Power:= 0; // count number of wraps in weWrap
                    gear^.WDTimer:= 0; // number of required wraps
                    gear^.Density:= _19;
                    gear^.Tint:= gear^.Hedgehog^.Team^.Clan^.Color shl 8 or $FF
                    end;
         gtAirBomb: begin
                    gear^.AdvBounce:= 1;
                    gear^.Radius:= 5;
                    gear^.Density:= _2;
                    gear^.Elasticity:= _0_55;
                    gear^.Friction:= _0_995
                    end;
       gtBlowTorch: begin
                    gear^.Radius:= cHHRadius + cBlowTorchC - 1;
                    if gear^.Timer = 0 then gear^.Timer:= 7500
                    end;
        gtSwitcher: begin
                    gear^.Z:= cCurrHHZ
                    end;
          gtTarget: begin
                    gear^.ImpactSound:= sndGrenadeImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.Radius:= 10;
                    gear^.Elasticity:= _0_3;
                    end;
          gtTardis: begin
                    gear^.Pos:= 1; // tardis phase
                    gear^.Tag:= 0; // 1 = hedgehog died, disappeared, took damage or moved
                    gear^.Z:= cCurrHHZ+1;
                    end;
          gtMortar: begin
                    gear^.AdvBounce:= 1;
                    gear^.Radius:= 4;
                    gear^.Elasticity:= _0_2;
                    gear^.Friction:= _0_08;
                    gear^.Density:= _1;
                    end;
            gtWhip: gear^.Radius:= 20;
          gtHammer: gear^.Radius:= 20;
        gtKamikaze: begin
                    gear^.Health:= 2048;
                    gear^.Radius:= 20
                    end;
            gtCake: begin
                    gear^.Health:= 2048;
                    gear^.Radius:= 7;
                    gear^.Z:= cOnHHZ;
                    gear^.RenderTimer:= false;
                    gear^.DirAngle:= -90 * hwSign(Gear^.dX);
                    gear^.FlightTime:= 100; // (roughly) ticks spent dropping, used to skip getting up anim when stuck.
                                            // Initially set to a high value so cake has at least one getting up anim.
                    if not dX.isNegative then
                        gear^.Angle:= 1
                    else
                        gear^.Angle:= 3;
                    New(cakeData);
                    gear^.Data:= Pointer(cakeData);
                    end;
     gtHellishBomb: begin
                    gear^.ImpactSound:= sndHellishImpact1;
                    gear^.nImpactSounds:= 4;
                    gear^.AdvBounce:= 1;
                    gear^.Radius:= 4;
                    gear^.Elasticity:= _0_5;
                    gear^.Friction:= _0_96;
                    gear^.Density:= _1_5;
                    gear^.RenderTimer:= true;
                    if gear^.Timer = 0 then gear^.Timer:= 5000
                    end;
           gtDrill: begin
                    gear^.AdvBounce:= 1;
                    gear^.Elasticity:= _0_8;
                    gear^.Friction:= _0_8;
                    if gear^.Timer = 0 then
                        gear^.Timer:= 5000;
                    // Tag for drill strike. if 1 then first impact occured already
                    gear^.Tag := 0;
                    // Pos for state. If 1, drill is drilling
                    gear^.Pos := 0;
                    gear^.Radius:= 4;
                    gear^.Density:= _1;
                    end;
            gtBall: begin
                    gear^.ImpactSound:= sndGrenadeImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.AdvBounce:= 1;
                    gear^.Radius:= 5;
                    gear^.Tag:= random(8);
                    if gear^.Timer = 0 then gear^.Timer:= 5000;
                    gear^.Elasticity:= _0_7;
                    gear^.Friction:= _0_995;
                    gear^.Density:= _1_5;
                    end;
         gtBallgun: begin
                    if gear^.Timer = 0 then gear^.Timer:= 5001;
                    end;
         gtRCPlane: begin
                    if gear^.Timer = 0 then gear^.Timer:= 15000;
                    gear^.Health:= 3;
                    gear^.Radius:= 8;
                    gear^.Tint:= gear^.Hedgehog^.Team^.Clan^.Color shl 8 or $FF
                    end;
         gtJetpack: begin
                    gear^.Health:= 2000;
                    gear^.Damage:= 100;
                    gear^.State:= Gear^.State or gstSubmersible
                    end;
         gtMolotov: begin
                    gear^.AdvBounce:= 1;
                    gear^.Radius:= 6;
                    gear^.Elasticity:= _0_8;
                    gear^.Friction:= _0_8;
                    gear^.Density:= _2
                    end;
           gtBirdy: begin
                    gear^.Radius:= 16; // todo: check
                    gear^.Health := 2000;
                    gear^.FlightTime := 2;
                    gear^.Z:= cCurrHHZ;
                    end;
             gtEgg: begin
                    gear^.AdvBounce:= 1;
                    gear^.Radius:= 4;
                    gear^.Elasticity:= _0_6;
                    gear^.Friction:= _0_96;
                    gear^.Density:= _1;
                    if gear^.Timer = 0 then
                        gear^.Timer:= 3000
                    end;
          gtPortal: begin
                    gear^.ImpactSound:= sndMelonImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.Radius:= 17;
                    // set color
                    gear^.Tag:= 2 * gear^.Timer;
                    gear^.Timer:= 15000;
                    gear^.RenderTimer:= false;
                    gear^.Health:= 100;
                    gear^.Sticky:= true;
                    end;
           gtPiano: begin
                    gear^.Radius:= 32;
                    gear^.Density:= _50;
                    end;
     gtSineGunShot: begin
                    gear^.Radius:= 5;
                    gear^.Health:= 6000;
                    end;
    gtFlamethrower: begin
                    gear^.Tag:= 10;
                    if gear^.Timer = 0 then gear^.Timer:= 10;
                    gear^.Health:= 500;
                    gear^.Damage:= 100;
                    end;
         gtLandGun: begin
                    gear^.Tag:= 10;
                    if gear^.Timer = 0 then gear^.Timer:= 10;
                    gear^.Health:= 1000;
                    gear^.Damage:= 100;
                    end;
     gtPoisonCloud: begin
                    if gear^.Timer = 0 then gear^.Timer:= 5000;
                    gear^.WDTimer:= gear^.Timer; // initial timer
                    gear^.dY:= int2hwfloat(-4 + longint(getRandom(8))) / 1000;
                    gear^.Tint:= $C0C000C0
                    end;
     gtResurrector: begin
                    gear^.Radius := cResurrectorDist;
                    gear^.Tag := 0;
                    gear^.Tint:= $F5DB35FF
                    end;
         gtWaterUp: begin
                    gear^.Tag := 47;
                    end;
      gtNapalmBomb: begin
                    gear^.Elasticity:= _0_8;
                    gear^.Friction:= _0_8;
                    if gear^.Timer = 0 then gear^.Timer:= 1000;
                    gear^.Radius:= 5;
                    gear^.Density:= _1_5;
                    end;
          gtIceGun: begin
                    gear^.Health:= 1000;
                    gear^.Radius:= 8;
                    gear^.Density:= _0;
                    gear^.Tag:= 0; // sound state: 0 = no sound, 1 = ice beam sound, 2 = idle sound
                    end;
         gtCreeper: begin
                    // TODO: Finish creeper initialization implementation
                    gear^.Radius:= cHHRadius;
                    gear^.Elasticity:= _0_35;
                    gear^.Friction:= _0_93;
                    gear^.Density:= _5;

                    gear^.AdvBounce:= 1;
                    gear^.ImpactSound:= sndAirMineImpact;
                    gear^.nImpactSounds:= 1;
                    gear^.Health:= 30;
                    gear^.Radius:= 8;
                    gear^.Angle:= 175; // Radius at which it will start "seeking". $FFFFFFFF = unlimited. check is skipped.
                    gear^.Power:= cMaxWindSpeed.QWordValue div 2; // hwFloat converted. 1/2 g default. defines the "seek" speed when a gear is in range.
                    gear^.Pos:= cMaxWindSpeed.QWordValue * 3 div 2; // air friction. slows it down when not hitting stuff
                    if gear^.Timer = 0 then
                        gear^.Timer:= 5000;
                    gear^.WDTimer:= gear^.Timer
                    end;
         gtMinigun: begin
                    // Timer. First, it's the timer before shooting. Then it will become the shooting timer and is set to Karma
                    if gear^.Timer = 0 then
                        gear^.Timer:= 601;
                    // minigun shooting time. 1 bullet is fired every 50ms
                    gear^.Karma:= 3451;
                    end;
     gtMinigunBullet: begin
                    gear^.Radius:= 1;
                    gear^.Health:= 2;
                    gear^.Karma:= 5; //impact radius
                    gear^.Pos:= 0; //uses non-global hit order
                    gear^.Data:= nil;
                    end;
            gtSentry: begin
                    gear^.Radius:= cHHRadius;
                    gear^.Health:= cSentryHealth;
                    gear^.Friction:= _0_999;
                    gear^.Elasticity:= _0_35;
                    gear^.Density:= _3;
                    gear^.Tag:= 0;
                    gear^.Timer:= 1000;
                    gear^.WDTimer:= 0;
                    end;
    gtGenericFaller:begin
                    gear^.AdvBounce:= 1;
                    gear^.Radius:= 1;
                    gear^.Elasticity:= _0_9;
                    gear^.Friction:= _0_995;
                    gear^.Density:= _1;
                    end;
        end;
end;

function AddGear(X, Y: LongInt; Kind: TGearType; State: Longword; dX, dY: hwFloat; Timer: LongWord): PGear;
begin
AddGear:= AddGear(X, Y, Kind, State, dX, dY, Timer, 0);
end;
function AddGear(X, Y: LongInt; Kind: TGearType; State: Longword; dX, dY: hwFloat; Timer, newUid: LongWord): PGear;
var gear: PGear;
    //c: byte;
begin
if newUid = 0 then
    inc(GCounter);

AddFileLog('AddGear: #' + inttostr(GCounter) + ' (' + inttostr(x) + ',' + inttostr(y) + '), d(' + floattostr(dX) + ',' + floattostr(dY) + ') type = ' + EnumToStr(Kind));

New(gear);

initializeGear(gear, X, Y, Kind, State, dX, dY, Timer, newUid);

InsertGearToList(gear);
AddGear:= gear;

ScriptCall('onGearAdd', gear^.uid);
end;

procedure DeleteGear(Gear: PGear);
var team: PTeam;
    t,i: Longword;
    cakeData: PCakeData;
    iterator: PGear;
begin

ScriptCall('onGearDelete', gear^.uid);

DeleteCI(Gear);
RemoveFromProximityCache(Gear);

FreeAndNilTexture(Gear^.Tex);

// remove potential links to this gear
// currently relevant to: gears linked by hammer
if (Gear^.Kind = gtHedgehog) or (Gear^.Kind = gtMine) or (Gear^.Kind = gtExplosives) then
    begin
    // check all gears for stuff to port through
    iterator := nil;
    while true do
        begin

        // iterate through GearsList
        if iterator = nil then
            iterator := GearsList
        else
            iterator := iterator^.NextGear;

        // end of list?
        if iterator = nil then
            break;

        if iterator^.LinkedGear = Gear then
            iterator^.LinkedGear:= nil;
        end;

    end;

// make sure that portals have their link removed before deletion
if (Gear^.Kind = gtPortal) then
    begin
    if (Gear^.LinkedGear <> nil) then
        if (Gear^.LinkedGear^.LinkedGear = Gear) then
            Gear^.LinkedGear^.LinkedGear:= nil;
    end
else if Gear^.Kind = gtCake then
    begin
        cakeData:= PCakeData(Gear^.Data);
        Dispose(cakeData);
        cakeData:= nil;
    end
else if Gear^.Kind = gtHedgehog then
    (*
    This behaviour dates back to revision 4, and I accidentally encountered it with TARDIS.  I don't think it must apply to any modern weapon, since if it was actually hit, the best the gear could do would be to destroy itself immediately, and you'd still end up with two graves.  I believe it should be removed
     if (CurAmmoGear <> nil) and (CurrentHedgehog^.Gear = Gear) then
        begin
        AttackBar:= 0;
        Gear^.Message:= gmDestroy;
        CurAmmoGear^.Message:= gmDestroy;
        exit
        end
    else*)
        begin
        if ((CurrentHedgehog = nil) or (Gear <> CurrentHedgehog^.Gear)) or (CurAmmoGear = nil) or (CurAmmoGear^.Kind <> gtKamikaze) then
            Gear^.Hedgehog^.Team^.Clan^.Flawless:= false;
        if CheckCoordInWater(hwRound(Gear^.X), hwRound(Gear^.Y)) then
            begin
            t:= max(Gear^.Damage, Gear^.Health);
            Gear^.Damage:= t;
            // Display hedgehog damage in water
            spawnHealthTagForHH(Gear, t);
            end;

        team:= Gear^.Hedgehog^.Team;
        if (CurrentHedgehog <> nil) and (CurrentHedgehog^.Gear = Gear) then
            begin
            AttackBar:= 0;
            FreeActionsList; // to avoid ThinkThread on drawned gear
            if ((Ammoz[CurrentHedgehog^.CurAmmoType].Ammo.Propz and ammoprop_NoRoundEnd) <> 0)
            and (CurrentHedgehog^.MultiShootAttacks > 0) then
                OnUsedAmmo(CurrentHedgehog^);
            end;

        Gear^.Hedgehog^.Gear:= nil;

        if Gear^.Hedgehog^.King then
            // If king died, kill the rest of the team
            begin
            with Gear^.Hedgehog^.Team^ do
                begin
                Gear^.Hedgehog^.Team^.hasKing:= false;
                for t:= 0 to cMaxHHIndex do
                    if Hedgehogs[t].Gear <> nil then
                        Hedgehogs[t].Gear^.Health:= 0
                    else if (Hedgehogs[t].GearHidden <> nil) then
                        Hedgehogs[t].GearHidden^.Health:= 0  // Hog is still hidden. If tardis should return though, Lua, eh ...
                end;
            end;

        // Update passive status of clan
        if (not Gear^.Hedgehog^.Team^.Clan^.Passive) then
            begin
            Gear^.Hedgehog^.Team^.Clan^.Passive:= true;
            for i:= 0 to Pred(team^.Clan^.TeamsNumber) do
                begin
                with team^.Clan^.Teams[i]^ do
                    if (not Passive) then
                        for t:= 0 to cMaxHHIndex do
                            if (Hedgehogs[t].Gear <> nil) or (Hedgehogs[t].GearHidden <> nil) then
                                begin
                                Gear^.Hedgehog^.Team^.Clan^.Passive:= false;
                                break;
                                end;
                if (not Gear^.Hedgehog^.Team^.Clan^.Passive) then
                    break;
                end;
            end;

        // should be not CurrentHedgehog, but hedgehog of the last gear which caused damage to this hog
        // same stand for CheckHHDamage
        if (Gear^.LastDamage <> nil) and (CurrentHedgehog <> nil) then
            uStats.HedgehogDamaged(Gear, Gear^.LastDamage, 0, true)
        else if CurrentHedgehog <> nil then
            uStats.HedgehogDamaged(Gear, CurrentHedgehog, 0, true);

        inc(KilledHHs);
        RecountTeamHealth(team);
        if (CurrentHedgehog <> nil) and (CurrentHedgehog^.Effects[heResurrectable] <> 0)  and
        //(Gear^.Hedgehog^.Effects[heResurrectable] = 0) then
        (Gear^.Hedgehog^.Team^.Clan <> CurrentHedgehog^.Team^.Clan) then
            with CurrentHedgehog^ do
                begin
                inc(Team^.stats.AIKills);
                FreeAndNilTexture(Team^.AIKillsTex);
                Team^.AIKillsTex := RenderStringTex(ansistring(inttostr(Team^.stats.AIKills)), Team^.Clan^.Color, fnt16);
                end
        end;
with Gear^ do
    begin
    AddFileLog('Delete: #' + inttostr(uid) + ' (' + inttostr(hwRound(x)) + ',' + inttostr(hwRound(y)) + '), d(' + floattostr(dX) + ',' + floattostr(dY) + ') type = ' + EnumToStr(Kind));
    AddRandomness(X.round xor X.frac xor dX.round xor dX.frac xor Y.round xor Y.frac xor dY.round xor dY.frac)
    end;
if CurAmmoGear = Gear then
    CurAmmoGear:= nil;
if FollowGear = Gear then
    FollowGear:= nil;
if lastGearByUID = Gear then
    lastGearByUID := nil;
if (Gear^.Hedgehog = nil) or (Gear^.Hedgehog^.GearHidden <> Gear) then // hidden hedgehogs shouldn't be in the list
     RemoveGearFromList(Gear)
else Gear^.Hedgehog^.GearHidden:= nil;

Dispose(Gear)
end;

end.
