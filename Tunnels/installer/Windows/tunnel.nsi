;NSIS Modern User Interface
;Tunnel Client Install Script
!include "FileFunc.nsh"
!include "EnvVarUpdate.nsh"
;--------------------------------
;Include Modern UI

  !include "MUI2.nsh"

;--------------------------------
;General

  ;Name and file
  Name "Dymium Tunneling Client"
  OutFile "DymiumInstaller.exe"
  Unicode True

  ;Default installation folder
  InstallDir "$PROGRAMFILES64\Dymium"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKLM "Software\Dymium" ""

  ;Request application privileges for Windows Vista
  RequestExecutionLevel highest

;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  !insertmacro MUI_PAGE_LICENSE "..\License.txt"
  !insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections

Section "Install Dymium Secure Tunnel" DymiumClient

  SetOutPath "$INSTDIR"
 


  ;ADD YOUR OWN FILES HERE...
  File ..\..\go\client\dymium.exe
  File ..\..\go\GUI\dymiumgui.exe
  
  ;Store installation folder
  WriteRegStr HKLM "Software\Dymium" "" $INSTDIR  
  Push "Path"
  Push "A"
  Push "HKLM"
  Push $INSTDIR 
  Call EnvVarUpdate
  Pop  $0
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"
WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Dymium" \
                 "DisplayName" "Dymium Secure Tunnel"
WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Dymium" \
                 "UninstallString" "$\"$INSTDIR\Uninstall.exe$\""
WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\Dymium" \
                 "DisplayIcon" "$\"$INSTDIR\logo.ico$\""

 ${GetSize} "$INSTDIR" "/S=0K" $0 $1 $2
 IntFmt $0 "0x%08X" $0

 
SetShellVarContext all
CreateShortcut "$DESKTOP\Dymium.lnk" "$instdir\dymiumgui.exe"

SectionEnd

;--------------------------------
;Descriptions

  ;Language strings
  LangString DESC_DymiumClient ${LANG_ENGLISH} "A test section."

  ;Assign language strings to sections
  !insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    !insertmacro MUI_DESCRIPTION_TEXT ${DymiumClient} $(DESC_DymiumClient)
  !insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"

  ;ADD YOUR OWN FILES HERE...
  SetShellVarContext all
  Delete "$INSTDIR\Uninstall.exe"
  Delete "$INSTDIR\dymium.exe"
  Delete "$INSTDIR\dymiumgui.exe"
  Delete "$DESKTOP\Dymium.lnk" 
  RMDir "$INSTDIR"

  WriteRegStr HKLM "Software\Dymium" "" $INSTDIR  
  Push "Path"
  Push "R"
  Push "HKLM"
  Push $INSTDIR 
  Call un.EnvVarUpdate
  Pop  $0
  
  DeleteRegKey /ifempty HKLM "Software\Dymium"

SectionEnd