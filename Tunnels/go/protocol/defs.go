package protocol

const (
	Close = iota
 	Open
	Send
	Ping
)

type TransmissionUnit struct {
	Action int
	Id     int
	Data   []byte
}

