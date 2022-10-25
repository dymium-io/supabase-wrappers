go build -a -tags netgo -ldflags "-X 'main.MajorVersion=0' -X 'main.MinorVersion=2' -X 'main.ProtocolVersion=4'-w -extldflags '-static'"

fyne package -os windows -icon logo.png


