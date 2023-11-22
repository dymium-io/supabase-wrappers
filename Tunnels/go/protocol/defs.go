package protocol

const (
	Close = iota
	Open
	Send
	Ping
	Error
	Version
)

type TransmissionUnit struct {
	Action int
	Id     int
	Data   []byte
}

var MeshServerVersion = "0.1.4"
var TunnelServerVersion = "0.1.4"