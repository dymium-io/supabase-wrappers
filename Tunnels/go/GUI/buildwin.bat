go build -a -tags netgo -ldflags "-w -extldflags '-static'"

fyne package -os windows -icon logo.png


