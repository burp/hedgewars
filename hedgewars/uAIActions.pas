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

unit uAIActions;
interface
uses uFloat, uTypes;

const MAXACTIONS     = 96;
    aia_none       = 0;
    aia_Left       = 1;
    aia_Right      = 2;
    aia_Timer      = 3;
    aia_attack     = 4;
    aia_Up         = 5;
    aia_Down       = 6;
    aia_Switch     = 7;
    aia_Precise    = 8;

    aia_Weapon     = $8000;
    aia_WaitXL     = $8001;
    aia_WaitXR     = $8002;
    aia_LookLeft   = $8003;
    aia_LookRight  = $8004;
    aia_AwareExpl  = $8005;
    aia_HJump      = $8006;
    aia_LJump      = $8007;
    aia_Skip       = $8008;
    aia_Wait       = $8009;
    aia_Put        = $800A;
    aia_waitAngle  = $800B;
    aia_waitAmmoXY = $800C;

    aim_push       = $8000;
    aim_release    = $8001;
    ai_specmask    = $8000;

type TAction = record
        Action: Longword;
        X, Y, Param: LongInt;
        Time: Longword;
        end;

    TActions = record
        Count, Pos: Longword;
        ticks: Longword;
        actions: array[0..Pred(MAXACTIONS)] of TAction;
        Score: LongInt;
        isWalkingToABetterPlace: boolean;
        end;

procedure AddAction(var Actions: TActions; Action: Longword; Param: LongInt; TimeDelta: Longword; X, Y: LongInt);
procedure ProcessAction(var Actions: TActions; Me: PGear);

implementation
uses uAIMisc, uAI, uAmmos, uVariables, uCommands, uConsts, uUtils, uIO{$IFDEF TRACEAIACTIONS}, uConsole{$ENDIF};

var PrevX: LongInt = 0;
    timedelta: Longword = 0;

const ActionIdToStr: array[0..8] of string[16] = (
{aia_none}           '',
{aia_Left}           'left',
{aia_Right}          'right',
{aia_Timer}          'timer',
{aia_attack}         'attack',
{aia_Up}             'up',
{aia_Down}           'down',
{aia_Switch}         'switch',
{aia_Precise}        'precise'
                     );

{$IFDEF TRACEAIACTIONS}
const SpecActionIdToStr: array[$8000..$800C] of string[16] = (
{aia_Weapon}             'aia_Weapon',
{aia_WaitX}              'aia_WaitX',
{aia_WaitY}              'aia_WaitY',
{aia_LookLeft}           'aia_LookLeft',
{aia_LookRight}          'aia_LookRight',
{aia_AwareExpl}          'aia_AwareExpl',
{aia_HJump}              'aia_HJump',
{aia_LJump}              'aia_LJump',
{aia_Skip}               'aia_Skip',
{aia_Wait}               'aia_Wait',
{aia_Put}                'aia_Put',
{aia_waitAngle}          'aia_waitAngle',
{aia_waitAmmoXY}         'aia_waitAmmoXY'
);

procedure DumpAction(Action: TAction; Me: PGear);
begin
if (Action.Action and ai_specmask) = 0 then
    WriteLnToConsole('AI action: '+ActionIdToStr[Action.Action])
else
    begin
    WriteLnToConsole('AI action: '+SpecActionIdToStr[Action.Action]);
    if (Action.Action = aia_WaitXL) or (Action.Action = aia_WaitXR) then
        WriteLnToConsole('AI action Wait X = '+IntToStr(Action.Param)+', current X = '+IntToStr(hwRound(Me^.X)))

    else if (Action.Action = aia_AwareExpl) then
        WriteLnToConsole('Aware X = ' + IntToStr(Action.X) + ', Y = ' + IntToStr(Action.Y));
    end
end;
{$ENDIF}

procedure AddAction(var Actions: TActions; Action: Longword; Param: LongInt; TimeDelta: Longword; X, Y: LongInt);
var t: Longword;
begin
if ((Action = aia_LookLeft) or (Action = aia_LookRight)) and (Actions.Count > 0) then
    begin
    t:= Actions.actions[Actions.Count - 1].Action;
    if ((t = aia_LookLeft) or (t = aia_LookRight)) then
        begin
        dec(Actions.Count)
        end;
    end;

if Actions.Count < MAXACTIONS then
    with Actions do
        begin
        actions[Count].Action:= Action;
        actions[Count].Param:= Param;
        actions[Count].X:= X;
        actions[Count].Y:= Y;
        if Count > 0 then
            actions[Count].Time:= TimeDelta
        else
            actions[Count].Time:= GameTicks + TimeDelta;
        inc(Count);
        end
end;

procedure CheckHang(Me: PGear; fromLeft: boolean);
var newX: LongInt;
begin
newX:= hwRound(Me^.X);
if newX <> PrevX then
    begin
    if (newX < PrevX) = fromLeft then
        begin
        FreeActionsList
        end;
    
    PrevX:= newX;
    timedelta:= 0
    end else
        begin
        inc(timedelta);
        if timedelta > 900 then
            begin
            timedelta:= 0;
            FreeActionsList
            end
        end
end;

procedure ProcessAction(var Actions: TActions; Me: PGear);
var s: shortstring;
begin
repeat
if Actions.Pos >= Actions.Count then exit;

with Actions.actions[Actions.Pos] do
    begin
    if Time > GameTicks then
        exit;
    {$IFDEF TRACEAIACTIONS}
    DumpAction(Actions.actions[Actions.Pos], Me);
    {$ENDIF}
    if (Action and ai_specmask) <> 0 then
        case Action of
            aia_Weapon:
                SetWeapon(TAmmoType(Param));

            aia_WaitXL:
                if hwRound(Me^.X) = Param then
                    begin
                    Action:= aia_LookLeft;
                    Time:= GameTicks;
                    exit
                    end
                    else if hwRound(Me^.X) < Param then
                        begin
                        //OutError('AI: WaitXL assert (' + IntToStr(hwRound(Me^.X)) + ' < ' + IntToStr(Param) + ')', false);
                        FreeActionsList;
                        exit
                        end
                    else
                        begin
                        CheckHang(Me, false);
                        exit
                        end;

            aia_WaitXR:
                if hwRound(Me^.X) = Param then
                    begin
                    Action:= aia_LookRight;
                    Time:= GameTicks;
                    exit
                    end
                    else if hwRound(Me^.X) > Param then
                        begin
                        //OutError('AI: WaitXR assert (' + IntToStr(hwRound(Me^.X)) + ' > ' + IntToStr(Param) + ')', false);
                        FreeActionsList;
                        exit
                        end
                    else
                        begin
                        CheckHang(Me, true);
                        exit
                        end;
            aia_LookLeft: begin
                if (Me^.State and gstMoving) <> 0 then
                    exit;
            
                if not Me^.dX.isNegative then
                begin
                    if (Me^.Message and gmLeft) = 0 then
                        ParseCommand('+left', true);
                    exit
                    end
                else
                    ParseCommand('-left', true);
            end;
            aia_LookRight: begin
                if (Me^.State and gstMoving) <> 0 then
                    exit;
            
                if Me^.dX.isNegative then
                begin
                    if (Me^.Message and gmRight) = 0 then
                        ParseCommand('+right', true);
                    exit
                    end
                else ParseCommand('-right', true);
            end;
            aia_AwareExpl:
                AwareOfExplosion(X, Y, Param);

            aia_HJump:
                ParseCommand('hjump', true);

            aia_LJump:
                ParseCommand('ljump', true);

            aia_Skip:
                ParseCommand('skip', true);

            aia_Put:
                doPut(X, Y, true);

            aia_waitAngle:
                if LongInt(Me^.Angle) <> Abs(Param) then exit;

            aia_waitAmmoXY:
                if (CurAmmoGear <> nil) and ((hwRound(CurAmmoGear^.X) <> X) or (hwRound(CurAmmoGear^.Y) <> Y)) then
                    exit;
            end
        else
            begin
            s:= ActionIdToStr[Action];
            if (Param and ai_specmask) <> 0 then
                case Param of
                aim_push:
                s:= '+' + s;

                aim_release:
                s:= '-' + s;
            end
        else if Param <> 0 then
            s:= s + ' ' + IntToStr(Param);
        ParseCommand(s, true)
        end
    end;
inc(Actions.Pos);
if Actions.Pos <= Actions.Count then
    inc(Actions.actions[Actions.Pos].Time, GameTicks);
until false
end;

end.
