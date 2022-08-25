package protocol

const Close = 0
const Open = 1 
const Send = 2

type TransmissionUnit struct {
	Action int
	Id     int
	Data   []byte
}

