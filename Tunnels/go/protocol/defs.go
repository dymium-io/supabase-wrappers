package protocol

const (
	Close = iota
 	Open
	Send
)

type TransmissionUnit struct {
	Action int
	Id     int
	Data   []byte
}

