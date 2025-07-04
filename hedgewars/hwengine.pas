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

{$IFDEF WINDOWS}
{$IFNDEF SKIP_RESOURCES}
{$R res/hwengine.rc}
{$ENDIF}
{$ENDIF}

{$IFDEF HWLIBRARY}
unit hwengine;
interface
{$ELSE}
program hwengine;
{$ENDIF}

uses {$IFDEF IPHONEOS}cmem, {$ENDIF} SDLh, uMisc, uConsole, uGame, uConsts, uLand, uAmmos, uVisualGears, uGears, uStore, uWorld, uInputHandler
     , uSound, uScript, uTeams, uStats, uIO, uLocale, uChat, uAI, uAIMisc, uAILandMarks, uLandTexture, uCollisions
     , SysUtils, uTypes, uVariables, uCommands, uUtils, uCaptions, uDebug, uCommandHandlers, uLandPainted
     , uPhysFSLayer, uCursor, uRandom, ArgParsers, uVisualGearsHandlers, uTextures, uRender
     {$IFDEF USE_VIDEO_RECORDING}, uVideoRec {$ENDIF}
     {$IFDEF USE_TOUCH_INTERFACE}, uTouch {$ENDIF}
     {$IFDEF ANDROID}, GLUnit{$ENDIF}
     {$IFDEF UNIX}, clocale{$ENDIF}
     {$IFDEF WINDOWS}, dynlibs{$ENDIF}
     ;

{$IFDEF HWLIBRARY}
function RunEngine(argc: LongInt; argv: PPChar): LongInt; cdecl; export;

procedure preInitEverything();
procedure initEverything(complete:boolean);
procedure freeEverything(complete:boolean);
{$IFNDEF PAS2C}
procedure catchUnhandledException(Obj: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);
{$ENDIF}

implementation
{$ELSE}
procedure preInitEverything(); forward;
procedure initEverything(complete:boolean); forward;
procedure freeEverything(complete:boolean); forward;
{$IFNDEF PAS2C}
procedure catchUnhandledException(Obj: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer); forward;
{$ENDIF}
{$ENDIF}

{$IFDEF WINDOWS}
type TSetProcessDpiAwareness = function(value: Integer): Integer; stdcall;
var SetProcessDpiAwareness: TSetProcessDpiAwareness;
var ShcoreLibHandle: TLibHandle;
{$ENDIF}

///////////////////////////////////////////////////////////////////////////////
function DoTimer(Lag: LongInt): boolean;
var s: shortstring;
    t: LongWord;
begin
    DoTimer:= false;
    inc(RealTicks, Lag);

    case GameState of
        gsLandGen:
            begin
            GenMap;
            SetLandTexture;
            UpdateLandTexture(0, LAND_WIDTH, 0, LAND_HEIGHT, false);
            setAILandMarks;
            GameState:= gsStart;
            end;
        gsStart:
            begin
            SetDefaultBinds;
            if HasBorder then
                DisableSomeWeapons;
            // wave "clouds" on underwater theme look weird w/ weSea, esp the blended bottom portion
            if (WorldEdge <> weSea) or (Theme <> 'Underwater') then
                AddClouds;
            AddFlakes;
            SetRandomSeed(cSeed, false);
            StoreLoad(false);
            ParseCommand('sendlanddigest', true); // extending land digest to all synced pixels (anything that could modify land)
            if not allOK then exit;
            AssignHHCoords;
            AddMiscGears;
            InitWorld;
            ResetKbd;
            if GameType = gmtSave then
                SetSound(false);
            FinishProgress;
            PlayMusic;
            InitZoom(zoom);
            ScriptCall('onGameStart');
            RandomizeHHAnim;
            for t:= 0 to Pred(TeamsCount) do
                with TeamsArray[t]^ do
                    MaxTeamHealth:= TeamHealth;
            RecountAllTeamsHealth;
            GameState:= gsGame;
            end;
        gsConfirm, gsGame:
            begin
            // disable screenshot flash effect when about to make another screenshot
            if flagMakeCapture and (ScreenFade = sfFromWhite) then
                ScreenFade:= sfNone;
            if not cOnlyStats then
                // never place between ProcessKbd and DoGameTick - bugs due to /put cmd and isCursorVisible
                DrawWorld(Lag);
            DoGameTick(Lag);
            if not cOnlyStats then ProcessVisualGears(Lag);
            end;
        gsExit:
            begin
            DoTimer:= true;
            end;
        gsSuspend:
            exit(false);
            end;

    if not cOnlyStats then SwapBuffers;

{$IFDEF USE_VIDEO_RECORDING}
    if flagPrerecording then
        SaveCameraPosition;
{$ENDIF}

    if flagMakeCapture then
        begin
        flagMakeCapture:= false;
        if flagDumpLand then
             s:= '/Screenshots/mapdump_'
        else s:= '/Screenshots/hw_';
        {$IFDEF PAS2C}
        s:= s + inttostr(GameTicks);
        {$ELSE}
        s:= s + FormatDateTime('YYYY-MM-DD_HH-mm-ss', Now()) + inttostr(GameTicks);
        {$ENDIF}

        // flash
        playSound(sndShutter);
        ScreenFade:= sfFromWhite;
        ScreenFadeValue:= sfMax;
        ScreenFadeSpeed:= 5;

        if (not flagDumpLand and MakeScreenshot(s, 1, 0)) or
           (flagDumpLand and MakeScreenshot(s, 1, 1) and ((cReducedQuality and rqBlurryLand <> 0) or MakeScreenshot(s, 1, 2))) then
            WriteLnToConsole('Screenshot saved: ' + s)
        else
            begin
            WriteLnToConsole('Screenshot failed.');
            AddChatString(#5 + 'screen capture failed (lack of memory or write permissions)');
            end
        end;
end;

///////////////////////////////////////////////////////////////////////////////
procedure MainLoop;
var event: TSDL_Event;
    PrevTime, CurrTime: LongWord;
    isTerminated: boolean;
    previousGameState: TGameState;
    wheelEvent: boolean;
begin
    previousGameState:= gsStart;
    isTerminated:= false;
    PrevTime:= SDL_GetTicks;
    while (not isTerminated) and allOK do
    begin
        wheelEvent:= false;
        SDL_PumpEvents();

        while SDL_PeepEvents(@event, 1, SDL_GETEVENT, SDL_FIRSTEVENT, SDL_LASTEVENT) > 0 do
        begin
            case event.type_ of
                SDL_KEYDOWN:
                    if isInChatMode then
                        begin
                    // sdl on iphone supports only ashii keyboards and the unicode field is deprecated in sdl 1.3
                        KeyPressChat(event.key.keysym);
                        end
                    else
                        if GameState >= gsGame then ProcessKey(event.key);
                SDL_KEYUP:
                    if (not isInChatMode) and (GameState >= gsGame) then
                        ProcessKey(event.key);

                SDL_TEXTINPUT: if isInChatMode then uChat.TextInput(event.text);

                SDL_WINDOWEVENT:
                    begin
                    case event.window.event of
                        SDL_WINDOWEVENT_FOCUS_GAINED:
                                begin
                                cHasFocus:= true;
                                onFocusStateChanged();
                                end;
                        SDL_WINDOWEVENT_FOCUS_LOST:
                                begin
                                cHasFocus:= false;
                                onFocusStateChanged();
                                end;
{$IFDEF MOBILE}
(* Suspend game if minimized on mobile.
NOTE: Mobile doesn't support online multiplayer yet, so it's not a problem.
BUT: This section WILL become a bug when online multiplayer is added to
Hedgewars and needs to be rethought. This is because it will cause the
game to freeze if one online player minimizes Hedgewars. *)
                        SDL_WINDOWEVENT_MINIMIZED:
                                begin
                                previousGameState:= GameState;
                                GameState:= gsSuspend;
                                end;
{$ENDIF}
                        SDL_WINDOWEVENT_RESTORED:
                                begin
                                if GameState = gsSuspend then
                                    GameState:= previousGameState;
                                cWindowedMaximized:= false;
{$IFDEF ANDROID}
                                //This call is used to reinitialize the glcontext and reload the textures
                                ParseCommand('fullscr '+intToStr(LongInt(cFullScreen)), true);
{$ENDIF}
                                end;
                        SDL_WINDOWEVENT_MAXIMIZED:
                                cWindowedMaximized:= true;
                        SDL_WINDOWEVENT_RESIZED:
                                begin
                                cNewScreenWidth:= max(2 * (event.window.data1 div 2), cMinScreenWidth);
                                cNewScreenHeight:= max(2 * (event.window.data2 div 2), cMinScreenHeight);
                                cScreenResizeDelay:= RealTicks + 500{$IFDEF IPHONEOS}div 2{$ENDIF};
                                end;
                        end; // case closed
                    end;

{$IFDEF USE_TOUCH_INTERFACE}
                SDL_FINGERMOTION:
                    onTouchMotion(event.tfinger.x, event.tfinger.y, event.tfinger.dx, event.tfinger.dy, event.tfinger.fingerId);

                SDL_FINGERDOWN:
                    onTouchDown(event.tfinger.x, event.tfinger.y, event.tfinger.fingerId);

                SDL_FINGERUP:
                    onTouchUp(event.tfinger.x, event.tfinger.y, event.tfinger.fingerId);
{$ELSE}
                SDL_MOUSEMOTION:
                    ProcessMouseMotion(event.motion.xrel, event.motion.yrel);

                SDL_MOUSEBUTTONDOWN:
                    if GameState = gsConfirm then
                        ParseCommand('quit', true)
                    else
                        if (GameState >= gsGame) then ProcessMouseButton(event.button, true);

                SDL_MOUSEBUTTONUP:
                    if (GameState >= gsGame) then ProcessMouseButton(event.button, false);

                SDL_MOUSEWHEEL:
                    begin
                    wheelEvent:= true;
                    ProcessMouseWheel(event.wheel.y);
                    end;
{$ENDIF}

                SDL_JOYAXISMOTION:
                    ControllerAxisEvent(event.jaxis.which, event.jaxis.axis, event.jaxis.value);
                SDL_JOYHATMOTION:
                    ControllerHatEvent(event.jhat.which, event.jhat.hat, event.jhat.value);
                SDL_JOYBUTTONDOWN:
                    ControllerButtonEvent(event.jbutton.which, event.jbutton.button, true);
                SDL_JOYBUTTONUP:
                    ControllerButtonEvent(event.jbutton.which, event.jbutton.button, false);
                SDL_QUITEV:
                    isTerminated:= true
            end; //end case event.type_ of
        end; //end while SDL_PollEvent(@event) <> 0 do

        if (not wheelEvent) then
            ResetMouseWheel();

        if (CursorMovementX <> 0) or (CursorMovementY <> 0) then
            handlePositionUpdate(CursorMovementX, CursorMovementY);

        if (cScreenResizeDelay <> 0) and (cScreenResizeDelay < RealTicks) and
           ((cNewScreenWidth <> cScreenWidth) or (cNewScreenHeight <> cScreenHeight)) then
        begin
            cScreenResizeDelay:= 0;
            cWindowedWidth:= cNewScreenWidth;
            cWindowedHeight:= cNewScreenHeight;
            cScreenWidth:= cWindowedWidth;
            cScreenHeight:= cWindowedHeight;

            ParseCommand('fullscr '+intToStr(LongInt(cFullScreen)), true);
            if cWindowedMaximized then
                WriteLnToConsole('window resize: ' + IntToStr(cScreenWidth) + ' x ' + IntToStr(cScreenHeight) + ' (maximized)')
            else
                WriteLnToConsole('window resize: ' + IntToStr(cScreenWidth) + ' x ' + IntToStr(cScreenHeight));
            ScriptOnScreenResize();
            InitCameraBorders();
            InitTouchInterface();
            InitZoom(zoomValue);
            if cWindowedMaximized then
                SendIPC('W' + IntToStr(cScreenWidth) + 'x' + IntToStr(cScreenHeight) + 'M')
            else
                SendIPC('W' + IntToStr(cScreenWidth) + 'x' + IntToStr(cScreenHeight));
        end;

        CurrTime:= SDL_GetTicks();
        if PrevTime + longword(cTimerInterval) <= CurrTime then
        begin
            isTerminated:= isTerminated or DoTimer(CurrTime - PrevTime);
            PrevTime:= CurrTime;
        end
        else SDL_Delay(1);
        IPCCheckSock();

    end;
end;

{$IFDEF USE_VIDEO_RECORDING}
procedure RecorderMainLoop;
var oldGameTicks, oldRealTicks, newGameTicks, newRealTicks: LongInt;
begin
    if not BeginVideoRecording() then
        exit;
    DoTimer(0); // gsLandGen -> gsStart
    DoTimer(0); // gsStart -> gsGame

    newGameTicks:= 0;
    newRealTicks:= 0;

    if not LoadNextCameraPosition(newRealTicks, newGameTicks) then
        exit;
    fastScrolling:= true;
    DoGameTick(newGameTicks);
    fastScrolling:= false;
    oldRealTicks:= 0;
    oldGameTicks:= newGameTicks;

    while LoadNextCameraPosition(newRealTicks, newGameTicks) do
    begin
        IPCCheckSock();
        RealTicks:= newRealTicks;
        DoGameTick(newGameTicks - oldGameTicks);
        if GameState = gsExit then
            break;
        ProcessVisualGears(newRealTicks - oldRealTicks);
        DrawWorld(newRealTicks - oldRealTicks);
        EncodeFrame();
        oldRealTicks:= newRealTicks;
        oldGameTicks:= newGameTicks;
    end;
    StopVideoRecording();
end;
{$ENDIF}

///////////////////////////////////////////////////////////////////////////////
procedure GameRoutine;
var s: shortstring;
    i: LongInt;
begin
{$IFDEF PAS2C}
    AddFileLog('Generated using pas2c');
{$ENDIF}
    WriteLnToConsole('Hedgewars engine ' + cVersionString + '-r' + cRevisionString +
                     ' (' + cHashString + ') with protocol #' + inttostr(cNetProtoVersion));
    AddFileLog('Prefix: "' + shortstring(PathPrefix) +'"');
    AddFileLog('UserPrefix: "' + shortstring(UserPathPrefix) +'"');

    for i:= 0 to ParamCount do
        AddFileLog(inttostr(i) + ': ' + ParamStr(i));

    WriteToConsole('Init SDL... ');
    if not cOnlyStats then SDLCheck(SDL_Init(SDL_INIT_VIDEO or SDL_INIT_NOPARACHUTE) >= 0, 'SDL_Init', true);
    WriteLnToConsole(msgOK);
    if not cOnlyStats then
        begin
        WriteToConsole('Init SDL_ttf... ');
        SDLCheck(TTF_Init() <> -1, 'TTF_Init', true);
        WriteLnToConsole(msgOK);
        end;

    if not allOK then exit;

    SDL_ShowCursor(SDL_DISABLE);

{$IFDEF USE_VIDEO_RECORDING}
    if GameType = gmtRecord then
        InitOffscreenOpenGL()
    else
{$ENDIF}
        begin
        // show main window
        if cFullScreen then
            ParseCommand('fullscr 1', true)
        else
            ParseCommand('fullscr 0', true);
        end;

    ControllerInit(); // has to happen before InitKbdKeyTable to map keys
    InitKbdKeyTable();
    if not allOK then exit;

    LoadLocale(cPathz[ptLocale] + '/en.txt');  // Do an initial load with english
    if cLanguageFName <> 'en.txt' then
        begin
        // Try two letter locale first before trying specific locale overrides
        if (Length(cLanguage) > 3) and (Copy(cLanguage, 1, 2) <> 'en') then
            begin
            LoadLocale(cPathz[ptLocale] + '/' + Copy(cLanguage, 1, 2) + '.txt')
            end;
        LoadLocale(cPathz[ptLocale] + '/' + cLanguageFName)
        end
    else cLanguage := 'en';

    if not allOK then exit;
    WriteLnToConsole(msgGettingConfig);

    LoadFonts();
    AddProgress();
    LoadDefaultClanColors(cPathz[ptConfig] + '/settings.ini');

    if cTestLua then
        begin
        ParseCommand('script ' + cScriptName, true);
        end
    else
        begin
        if recordFileName = '' then
            begin
            InitIPC;
            SendIPCAndWaitReply(_S'C');        // ask for game config
            end
        else
            LoadRecordFromFile(recordFileName);
        end;

    if not allOK then exit;
    ScriptOnGameInit;
    s:= 'eproto ' + inttostr(cNetProtoVersion);
    SendIPCRaw(@s[0], Length(s) + 1); // send proto version

    InitTeams();
    AssignStores();

    if GameType = gmtRecord then
        SetSound(false);

    InitSound();

    isDeveloperMode:= false;
    if checkFails(InitStepsFlags = cifAllInited, 'Some parameters not set (flags = ' + inttostr(InitStepsFlags) + ')', true) then exit;
    if not allOK then exit;

{$IFDEF USE_VIDEO_RECORDING}
    if GameType = gmtRecord then
    begin
        RecorderMainLoop();
        exit;
    end;
{$ENDIF}

    MainLoop;
end;

procedure Game;
begin
    initEverything(true);
    GameRoutine;
    // clean up all the memory allocated
    freeEverything(true);
end;
///////////////////////////////////////////////////////////////////////////////
// preInitEverything - init variables that are going to be ovewritten by arguments
// initEverything - init variables only. Should be coupled by below
// freeEverything - free above. Pay attention to the init/free order!
procedure preInitEverything;
begin
    allOK:= true;
    Randomize();

    uVariables.preInitModule;
    uSound.preInitModule;
end;

procedure initEverything (complete:boolean);
begin
    PathPrefix:= PathPrefix + #0;
    UserPathPrefix:= UserPathPrefix + #0;
    uPhysFSLayer.initModule(@PathPrefix[1], @UserPathPrefix[1]);
    PathPrefix:= copy(PathPrefix, 1, length(PathPrefix) - 1);
    UserPathPrefix:= copy(UserPathPrefix, 1, length(UserPathPrefix) - 1);

    uUtils.initModule(complete);    // opens the debug file, must be the first
    uVariables.initModule;          // inits all global variables
    uCommands.initModule;           // helps below
    uCommandHandlers.initModule;    // registers all messages from frontend

    uLand.initModule;               // computes land
    uLandPainted.initModule;        // computes drawn land
    uIO.initModule;                 // sets up sockets

    uScript.initModule;

    if complete then
    begin
        uTextures.initModule;
{$IFDEF ANDROID}GLUnit.initModule;{$ENDIF}
{$IFDEF USE_TOUCH_INTERFACE}uTouch.initModule;{$ENDIF}
{$IFDEF USE_VIDEO_RECORDING}uVideoRec.initModule;{$ENDIF}
        uAI.initModule;
        uAIMisc.initModule;
        uAILandMarks.initModule;    //stub
        uAmmos.initModule;
        uCaptions.initModule;

        uChat.initModule;
        uCollisions.initModule;
        uGears.initModule;
        uInputHandler.initModule;
        uMisc.initModule;
        uLandTexture.initModule;    //stub
        uSound.initModule;
        uStats.initModule;
        uStore.initModule;
        uRender.initModule;
        uTeams.initModule;
        uVisualGears.initModule;
        uVisualGearsHandlers.initModule;
        uWorld.initModule;
    end;
end;

procedure freeEverything (complete:boolean);
begin
    if complete then
        begin
        WriteLnToConsole('Freeing resources...');
        uAI.freeModule;             // AI things need to be freed first
        uAIMisc.freeModule;         //stub
        uAILandMarks.freeModule;
        uCaptions.freeModule;
        uWorld.freeModule;
        uVisualGears.freeModule;
        uTeams.freeModule;
        uInputHandler.freeModule;
        uStats.freeModule;          //stub
        uSound.freeModule;
        uMisc.freeModule;
        uLandTexture.freeModule;
        uGears.freeModule;
        uCollisions.freeModule;     //stub
        uChat.freeModule;
        uAmmos.freeModule;
        uRender.freeModule;
        uStore.freeModule;          // closes SDL
{$IFDEF USE_VIDEO_RECORDING}uVideoRec.freeModule;{$ENDIF}
{$IFDEF USE_TOUCH_INTERFACE}uTouch.freeModule;{$ENDIF}  //stub
{$IFDEF ANDROID}GLUnit.freeModule;{$ENDIF}
        uTextures.freeModule;
        SDL_GL_DeleteContext(SDLGLcontext);
        SDL_DestroyWindow(SDLwindow);
        SDL_Quit()
        end;

    uIO.freeModule;
    uLand.freeModule;
    uLandPainted.freeModule;

    uCommandHandlers.freeModule;
    uCommands.freeModule;
    uVariables.freeModule;
    uUtils.freeModule;              // closes debug file
    uPhysFSLayer.freeModule;
    uScript.freeModule;
end;

///////////////////////////////////////////////////////////////////////////////
procedure GenLandPreview;
{$IFDEF MOBILE}
var Preview: TPreview;
{$ELSE}
var Preview: TPreviewAlpha;
{$ENDIF}
begin
    initEverything(false);

    InitIPC;
    if allOK then
    begin
        IPCWaitPongEvent;
        if checkFails(InitStepsFlags = cifRandomize, 'Some parameters not set (flags = ' + inttostr(InitStepsFlags) + ')', true) then exit;

        ScriptOnPreviewInit;
    {$IFDEF MOBILE}
        GenPreview(Preview);
    {$ELSE}
        GenPreviewAlpha(Preview);
    {$ENDIF}
        WriteLnToConsole('Sending preview...');
        SendIPCRaw(@Preview, sizeof(Preview));
        SendIPCRaw(@MaxHedgehogs, sizeof(byte));
        WriteLnToConsole('Preview sent, disconnect');
    end;

    freeEverything(false);
end;

{$IFNDEF PAS2C}
// Write backtrace to console and log when an unhandled exception occurred
procedure catchUnhandledException(Obj: TObject; Addr: Pointer; FrameCount: Longint; Frames: PPointer);
var
  Message: string;
  i: LongInt;
begin
  WriteLnToConsole('An unhandled exception occurred at $' + HexStr(Addr) + ':');
  if Obj is exception then
   begin
     Message := Exception(Obj).ClassName + ': ' + Exception(Obj).Message;
     WriteLnToConsole(Message);
   end
  else
    WriteLnToConsole('Exception object ' + Obj.ClassName + ' is not of class Exception.');
  WriteLnToConsole(BackTraceStrFunc(Addr));
  if (FrameCount > 0) then
    begin
      for i := 0 to FrameCount - 1 do
        WriteLnToConsole(BackTraceStrFunc(Frames[i]));
    end;
end;
{$ENDIF}

{$IFDEF HWLIBRARY}
function RunEngine(argc: LongInt; argv: PPChar): LongInt; cdecl; export;
begin
    operatingsystem_parameter_argc:= argc;
    operatingsystem_parameter_argv:= argv;
{$ELSE}
begin
{$ENDIF}

{$IFDEF WINDOWS}
    ShcoreLibHandle := LoadLibrary('Shcore.dll');
    if (ShcoreLibHandle <> 0) then
    begin
        SetProcessDpiAwareness :=
            TSetProcessDpiAwareness(GetProcedureAddress(ShcoreLibHandle, 'SetProcessDpiAwareness'));
        if (SetProcessDpiAwareness <> nil) then
            SetProcessDpiAwareness(1);
    end;
{$ENDIF}

///////////////////////////////////////////////////////////////////////////////
/////////////////////////////////// m a i n ///////////////////////////////////
///////////////////////////////////////////////////////////////////////////////
{$IFDEF PAS2C}
    // workaround for pascal's ParamStr and ParamCount
    init(argc, argv);
{$ENDIF}
{$IFNDEF PAS2C}
    // Custom procedure for unhandled exceptions; ExceptProc is used by SysUtils module
    ExceptProc:= @catchUnhandledException;
{$ENDIF}

    preInitEverything();

    GetParams();

    if GameType = gmtLandPreview then
        GenLandPreview()
    else if (GameType <> gmtBadSyntax) and (GameType <> gmtSyntaxHelp) then
        Game();

    // return error when engine is not called correctly
    if GameType = gmtBadSyntax then
        {$IFDEF PAS2C}
        exit(HaltUsageError);
        {$ELSE}
        halt(HaltUsageError);
        {$ENDIF}

    if cTestLua then
        begin
        WriteLnToConsole(errmsgLuaTestTerm);
        {$IFDEF PAS2C}
        exit(HaltTestUnexpected);
        {$ELSE}
        halt(HaltTestUnexpected);
        {$ENDIF}
        end;

    {$IFDEF PAS2C}
        exit(HaltNoError);
    {$ELSE}
        {$IFDEF IPHONEOS}
            exit(HaltNoError);
        {$ELSE}
            halt(HaltNoError);
        {$ENDIF}
    {$ENDIF}
{$IFDEF HWLIBRARY}
end;
{$ENDIF}

end.
