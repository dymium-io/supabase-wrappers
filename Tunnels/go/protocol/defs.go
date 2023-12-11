package protocol

import (
	"encoding/binary"
	"io"
	"net"
)

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


var MeshServerVersion = "0.1.5"
var TunnelServerVersion = "0.1.5"

/*

Protocol chunk:

Action: byte
Id: int32
Length: int32
[Data: byte[] ]
*/
const ProtocolChunkSize = 9

func ReadFull(conn net.Conn, buf []byte, length int) (int, error) {
    if len(buf) < length {
        return 0, io.ErrShortBuffer
    }

    totalRead := 0
    for totalRead < length {
        n, err := conn.Read(buf[totalRead:length])
        if err != nil {
            if err == io.EOF && totalRead > 0 {
                break // EOF is only OK if we read some bytes
            }
            return 0, err
        }
        totalRead += n
    }

    return totalRead, nil
}

func WriteToTunnel(buff *TransmissionUnit, conn net.Conn) error {
	b := int8(buff.Action)
    err := binary.Write(conn, binary.BigEndian, &b ) 
	if err != nil {
		return err
	}
	i := int32(buff.Id)
    err = binary.Write(conn, binary.BigEndian, &i ) 
	if err != nil {
		return err
	}
	var l int32
	d := buff.Data
	if d != nil {
		l = int32(len(d))
	}
	err = binary.Write(conn, binary.BigEndian, &l ) 
	if err != nil {
		return err
	}
	if d != nil {
		_, err := conn.Write(buff.Data)
		if err != nil {
			return err
		}	
	}
	return nil
}

func GetTransmissionUnit(st []byte, buff *TransmissionUnit, ingress net.Conn) error {
	buff.Action = int(st[0])
	buff.Id = int(binary.BigEndian.Uint32(st[1:5]))
	l := int(binary.BigEndian.Uint32(st[5:9]))
	if l > 0 {
		buff.Data = make([]byte, l)
		_, err := ReadFull(ingress, buff.Data, l)
		if err != nil {
			return err
		}
	}
	return nil
}