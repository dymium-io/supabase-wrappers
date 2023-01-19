package protocol

const (
	Close = iota
	Open
	Send
	Ping
	Error
)

type TransmissionUnit struct {
	Action int
	Id     int
	Data   []byte
}
