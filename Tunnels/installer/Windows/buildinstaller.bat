cd ..\..\go\client
call build.bat
cd ..\GUI
call buildwin.bat
cd ..\..\installer\Windows
makensis /V4 tunnel.nsi

