package protocol

import (
	"bufio"
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

var MeshServerVersion = "1.0.1"
var TunnelServerVersion = "1.0.2"

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
	// Wrap the connection with a buffered writer
	bw := bufio.NewWriter(conn)

	// Write Action directly
	if err := bw.WriteByte(byte(buff.Action)); err != nil {
		return err
	}

	// Write Id
	if err := binary.Write(bw, binary.BigEndian, int32(buff.Id)); err != nil {
		return err
	}

	// Write length of Data field
	var dataLength int32
	if buff.Data != nil {
		dataLength = int32(len(buff.Data))
	}
	if err := binary.Write(bw, binary.BigEndian, dataLength); err != nil {
		return err
	}

	// Write the data field if it exists
	if buff.Data != nil {
		if _, err := bw.Write(buff.Data); err != nil {
			return err
		}
	}

	// Flush the buffered writer to send the data to the connection
	return bw.Flush()
}

func WriteBufferedToTunnel(buff *TransmissionUnit, bw *bufio.Writer, conn net.Conn) error {

	// Write Action directly
	if err := bw.WriteByte(byte(buff.Action)); err != nil {
		return err
	}

	// Write Id
	if err := binary.Write(bw, binary.BigEndian, int32(buff.Id)); err != nil {
		return err
	}

	// Write length of Data field
	var dataLength int32
	if buff.Data != nil {
		dataLength = int32(len(buff.Data))
	}
	if err := binary.Write(bw, binary.BigEndian, dataLength); err != nil {
		return err
	}

	// Write the data field if it exists
	if buff.Data != nil {
		if _, err := bw.Write(buff.Data); err != nil {
			return err
		}
	}

	// Flush the buffered writer to send the data to the connection
	return bw.Flush()
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
func GetBufferedTransmissionUnit(st []byte, buff *TransmissionUnit, b []byte, ingress *bufio.Reader) error {
	buff.Action = int(st[0])
	buff.Id = int(binary.BigEndian.Uint32(st[1:5]))
	l := int(binary.BigEndian.Uint32(st[5:9]))
	if l > 0 {
		buff.Data = b[:l]
		_, err := io.ReadFull(ingress, buff.Data)
		if err != nil {
			return err
		}
	}
	return nil
}
