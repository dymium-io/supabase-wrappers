;NSIS Modern User Interface
;Tunnel Client Install Script

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
  InstallDir "$LOCALAPPDATA\Dymium"
  
  ;Get installation folder from registry if available
  InstallDirRegKey HKCU "Software\Dymium" ""

  ;Request application privileges for Windows Vista
  RequestExecutionLevel user

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

Section "Install Dymium Client" DymiumClient

  SetOutPath "$INSTDIR"
 


  ;ADD YOUR OWN FILES HERE...
  File ..\..\go\client\dymium.exe

  ;Store installation folder
  WriteRegStr HKCU "Software\Dymium" "" $INSTDIR  
  Push "Path"
  Push "A"
  Push "HKCU"
  Push $INSTDIR 
  Call EnvVarUpdate
  Pop  $0
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

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

  Delete "$INSTDIR\Uninstall.exe"
  Delete "$INSTDIR\dymium.exe"
  RMDir "$INSTDIR"

  WriteRegStr HKCU "Software\Dymium" "" $INSTDIR  
  Push "Path"
  Push "R"
  Push "HKCU"
  Push $INSTDIR 
  Call un.EnvVarUpdate
  Pop  $0
  
  DeleteRegKey /ifempty HKCU "Software\Dymium"

SectionEnd