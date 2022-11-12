#!/bin/bash

#GOOS=darwin GOARCH=amd64 \
#           go build -a -tags netgo -ldflags '-X 'main.MajorVersion=0' -X 'main.MinorVersion=5' -X 'main.ProtocolVersion=5' -w -extldflags "-static"' 

# fails to build statically, let's build dynamically
GOOS=darwin GOARCH=amd64 \
           go build -a -tags netgo -ldflags '-X 'main.MajorVersion=0' -X 'main.MinorVersion=5' -X 'main.ProtocolVersion=5' -w ' 

rm -rf dymium.app
fyne package -os darwin -icon logo.png
cd ../client
CGO_ENABLED=0 GOOS=darwin GOARCH=amd64 \
           go build -a -tags netgo -ldflags '-X 'main.MajorVersion=0' -X 'main.MinorVersion=5' -X 'main.ProtocolVersion=5' -w -extldflags "-static"' -o ../GUI/dymiumgui.app/Contents/MacOS/dymium
cd ../GUI
codesign -dvv --timestamp -s "Developer ID Application: Dymium Inc (RC7F4R4R28)" --options=hard,expires,runtime ./dymiumgui.app/Contents/MacOS/dymium
codesign -dvv --timestamp -s "Developer ID Application: Dymium Inc (RC7F4R4R28)" --options=hard,expires,runtime ./dymiumgui.app

