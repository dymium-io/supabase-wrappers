go build -a -tags netgo -ldflags "-X 'main.MajorVersion=0' -X 'main.MinorVersion=5' -X 'main.ProtocolVersion=5'-w -extldflags '-static'"

fyne package -os windows -icon logo.png


