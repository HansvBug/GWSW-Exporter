{$Region 'License'}
{ BSD 3 Clause license
Copyright (c)2023-2024, Benny Christensen a.k.a. cdbc

All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright notice,
      this list of conditions and the following disclaimer in the documentation
      and/or other materials provided with the distribution.
    * Neither the names of "obs_prosu.pas" nor the names
      of its contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. }
{$EndRegion 'License'}
unit obs_prosu;
{$mode ObjFPC}{$H+}
{$define debug}
interface
uses Classes, SysUtils;
const        
  prMaxWord = 65535; { 2 bytes ~ lower limit for pointers in most OS', 19.12.24 /bc }
  { defined const reasons, pertaining to "TProviderReason" and used in the
    observer-pattern. can easily be extended by: prMyNewReason = prUser + 1;
    somewhere visible to both obsProvider & obsSubscriber. (resembles c-enums) }
  prNone          = 0;
  prCustom        = 1;
  prDataAdded     = 2;
  prDataChanged   = 3;
  prDataDeleted   = 4;
  prDataRead      = 5;
  prDataWritten   = 6;
  prDataUpdate    = 7;
  prErrorIsEmpty  = 8;
  prErrorFile     = 9;
  prErrorNotFound = 10;
  prStatus        = 11;
  prUser          = 12; { (p)rovider (r)eason consts } 
  prMaxReason     = prMaxWord;
  { if user needs more/other reasons, just do: prPercentCalculated = prUser + 1; }
  // etc...
  { sguid pertaining to IobsSubscriber }
  SGUIDIobsSubscriber = '{23C4090B-DF30-4400-A0CE-84B34C0762FA}';
  { sguid pertaining to IobsProvider }
  SGUIDIobsProvider = '{A41E973D-EF2E-4941-9F16-514435D5A5C7}';
  { ©2023-2024 Benny Christensen a.k.a. cdbc }
  obsVersion = '3.19.12.2024'; { last update, small adjustments }
  oprDescription = '"IobsProvider":"obsProvider" is a service, that implements'+#10+
                   'the Observed-part of the "Observer-pattern". It needs the'+#10+
                   '"IobsSubscriber", Observer-part to function.'+#10+
                   'Remember to: iPro.Obj.Free; when you''re done.'; {208}
  osuDescription = '"IobsSubscriber":"obsSubscriber" is a service, that implements'+#10+
                   'the Observer-part of the "Observer-pattern". It needs the'+#10+
                   '"IobsProvider", Observed-part to function.'+#10+
                   'Remember to: iSub.Obj.Free; when you''re done.'; {210}

type
  TProviderReason = ptrint; { see consts above }
{ an implementation of Observer/Subscriber & Observed/Subject/Provider,
  as CORBA interfaces / services, this means NO refcounting! manual memory handling }
{$interfaces corba}
  { this is an adaptive way of decoupling the update-logic from the interfaces, <gui>
    sig= procedure(aReason: TProviderReason; aNotifyClass: TObject; UserData: pointer) of object; }
  TobsUpdateSubscriberMethod = procedure(aReason: TProviderReason;aNotifyClass: TObject;UserData: pointer) of object;
  { this is an adaptive way of decoupling the update-logic from the interfaces, <cli>
    sig= procedure(aReason: TProviderReason; aNotifyClass: TObject; UserData: pointer); }
  TobsUpdateSubscriberProc = procedure(aReason: TProviderReason;aNotifyClass: TObject;UserData: pointer);

  { in the observer pattern this is called the Observer or Subscriber
  var fSubscriber: TobsSubscriber;
    begin        " if you use the factories, you call it like this: "
      fSubscriber:= CreateObsSubscriber(@SomeNotifyHandler,ISomeprovider);
      (i) DON'T CALL: SetUpdateSubscriberMethod & ISomeprovider.Subscribe (i)
      or
      fSubscriber:= CreateObsSubscriber(@SomeNotifyHandler);
      ISomeprovider.Subscribe(fSubscriber);
      or
      fSubscriber:= CreateObsSubscriber; = NO param
      (i) if created without params then the following is necessary (i)
      fSubscriber.SetUpdateSubscriberMethod(@SomeMethod); <--- VERY IMPORTANT!!!
      ISomeprovider.Subscribe(fSubscriber);               <--- IMPORTANT TOO!!!!
      (i) if you forget these 2 above, nothing works! (i)
    ... do work ...
      ISomeprovider.UnSubscribe(fSubscriber); // unsubscribe before free & nil
      fSubscriber.Free; //
      fSubscriber:= nil; // no dead references dangling
    end; }
  IobsSubscriber = interface['{23C4090B-DF30-4400-A0CE-84B34C0762FA}']
    function Obj: TObject;
    procedure SetUpdateSubscriberMethod(aMethod: TobsUpdateSubscriberMethod);
    procedure SetUpdateSubscriberProc(aProc: TobsUpdateSubscriberProc);
    procedure UpdateSubscriber(aReason: TProviderReason;aNotifyClass: TObject;UserData: pointer);
  end; { IobsSubscriber }

  { in the observer pattern this is called the Subject or Observed
  var pro: IobsProvider;
    This service takes NO creation parameters!
    begin
      fProvider:= CreateObsProvider;
      fProvider.Subscribe(SomeIobsSubscriber);        //  <--- VERY IMPORTANT!!!
    ... do work ...
      fProvider.Free;                                 //  <--- VERY IMPORTANT!!!
      fProvider:= nil;                          // no dead references dangling
    end; }
  IobsProvider = interface['{A41E973D-EF2E-4941-9F16-514435D5A5C7}']
    procedure NotifySubscribers(aReason: TProviderReason;aNotifyClass: TObject;UserData: pointer);
    function Obj: TObject;
    procedure Subscribe(const aSubscriber: IobsSubscriber);
    procedure UnSubscribe(const aSubscriber: IobsSubscriber);
  end; { IobsProvider }
{$interfaces com}
{$Region 'TobsSubscriberH'}
  { TobsSubscriber implementor of our observer / subscriber }
  TobsSubscriber = class(TObject,IobsSubscriber)
  private
    fProvider: IobsProvider; { if created with one }
    fUpdateMethod: TobsUpdateSubscriberMethod;
    fUpdateProc: TobsUpdateSubscriberProc;
    function Obj: TObject;
  public
    constructor Create(aNotifyHandler: TobsUpdateSubscriberMethod = nil;aProvider: IobsProvider = nil);
    destructor Destroy; override;
    procedure SetUpdateSubscriberMethod(aMethod: TobsUpdateSubscriberMethod);
    procedure SetUpdateSubscriberProc(aProc: TobsUpdateSubscriberProc);
    function ToString: ansistring; override;
    procedure UpdateSubscriber(aReason: TProviderReason;aNotifyClass: TObject;UserData: pointer);
  end;
{$EndRegion 'TobsSubscriberH'}
{$Region 'TobsProviderH'}
  { TobsProvider implementor of our provider / subject / observed }
  TobsProvider = class(TObject,IobsProvider)
  private
    fSubscribers: TFPList;
    function Obj: TObject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure NotifySubscribers(aReason: TProviderReason;aNotifyClass: TObject;UserData: pointer);
    procedure Subscribe(const aSubscriber: IobsSubscriber);
    function ToString: ansistring; override;
    procedure UnSubscribe(const aSubscriber: IobsSubscriber);
  end;
{$EndRegion 'TobsProviderH'}
/////  provider & subscriber factories  \\\\\
{ this factory takes NO parameters :o) }
function CreateObsProvider: TobsProvider;
{ Optional Params: @SomeNotifyHandler & ISomeProvider }
function CreateObsSubscriber(aNotifyHandler: TobsUpdateSubscriberMethod = nil;aProvider: IobsProvider = nil): TobsSubscriber;

implementation

function CreateObsProvider: TobsProvider;
begin
  Result:= TobsProvider.Create;
end;

function CreateObsSubscriber(aNotifyHandler: TobsUpdateSubscriberMethod;
                             aProvider: IobsProvider): TobsSubscriber;
begin
  Result:= TobsSubscriber.Create(aNotifyHandler,aProvider);
end;

{$Region 'TobsSubscriber'}
{ TobsSubscriber }
function TobsSubscriber.Obj: TObject;
begin
  Result:= Self;
end;

constructor TobsSubscriber.Create(aNotifyHandler: TobsUpdateSubscriberMethod;
                                  aProvider: IobsProvider);
begin
  inherited Create;
  fUpdateMethod:= aNotifyHandler;
  fProvider:= aProvider;
  if fProvider <> nil then fProvider.Subscribe(Self);
end;

destructor TobsSubscriber.Destroy;
begin
  if Assigned(fProvider) then fProvider.UnSubscribe(Self);
  inherited Destroy;
end;

procedure TobsSubscriber.SetUpdateSubscriberMethod(aMethod: TobsUpdateSubscriberMethod);
begin
  fUpdateMethod:= aMethod;
end;

procedure TobsSubscriber.SetUpdateSubscriberProc(aProc: TobsUpdateSubscriberProc);
begin
  fUpdateProc:= aProc;
end;

function TobsSubscriber.ToString: ansistring;
begin
  Result:= osuDescription;
end;

procedure TobsSubscriber.UpdateSubscriber(aReason: TProviderReason;aNotifyClass: TObject;UserData: pointer);
begin
  if Assigned(fUpdateMethod) then fUpdateMethod(aReason,aNotifyClass,UserData);
  if Assigned(fUpdateProc) then fUpdateProc(aReason,aNotifyClass,UserData);
end;
{$EndRegion 'TobsSubscriber'}
{$Region 'TobsProvider'}
{ TobsProvider }
function TobsProvider.Obj: TObject;
begin
  Result:= Self;
end;

constructor TobsProvider.Create;
begin
  inherited Create;
  fSubscribers:= nil;
end;

destructor TobsProvider.Destroy;
var I: integer;
begin
  if assigned(fSubscribers) then begin
    for i:= fSubscribers.Count-1 downto 0 do UnSubscribe(IobsSubscriber(fSubscribers[i]));
    FreeAndNil(fSubscribers);
  end;
  inherited Destroy;
end;

procedure TobsProvider.NotifySubscribers(aReason: TProviderReason;aNotifyClass: TObject;UserData: pointer);
var
  i: integer;
  Sub: IobsSubscriber;
begin
  if assigned(fSubscribers) then for i:= fSubscribers.Count-1 downto 0 do begin
    Sub:= IobsSubscriber(fSubscribers[i]);
    if Sub <> nil then try Sub.UpdateSubscriber(aReason,aNotifyClass,UserData); except end;
  end;
end;

procedure TobsProvider.Subscribe(const aSubscriber: IobsSubscriber);
begin
  if aSubscriber = nil then exit;
  if not assigned(fSubscribers) then fSubscribers:= TFPList.Create;
  if fSubscribers.IndexOf(aSubscriber) = -1 then fSubscribers.Add(aSubscriber);
end;

function TobsProvider.ToString: ansistring;
begin
  Result:= oprDescription;
end;

procedure TobsProvider.UnSubscribe(const aSubscriber: IobsSubscriber);
begin
  if aSubscriber = nil then exit;
  if assigned(fSubscribers) then begin
    fSubscribers.Remove(aSubscriber);
    if fSubscribers.Count = 0 then FreeAndNil(fSubscribers);
  end;
end;
{$EndRegion 'TobsProvider'}

end.
