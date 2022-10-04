

cd ..\..\go\client
go build -a -tags netgo -ldflags "-X ^"main.MajorVersion=0^" -X ^"main.MinorVersion=1^" -X ^"main.ProtocolVersion=4^" -extldflags ^"-static^"" -o dymium.exe
copy dymium.exe ..\..\installer\Windows
cd   ..\..\installer\Windows
makensis tunnel.nsi

aws s3 --profile dymium --region us-west-2 cp DymiumInstaller.exe s3://dymium-dev/installers/macos/