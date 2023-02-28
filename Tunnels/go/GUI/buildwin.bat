go build -a -tags netgo -ldflags "-X 'main.MajorVersion=0' -X 'main.MinorVersion=6' -X 'main.ProtocolVersion=6'-w -extldflags '-static'"

fyne package -os windows -icon logo.png


