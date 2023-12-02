//go:build windows
// +build windows

package sockopts

import (
	"net"

	"golang.org/x/sys/windows"
)

func SetReuseAddr(listener net.Listener) error {
	tcpListener, ok := listener.(*net.TCPListener)
	if !ok {
		return nil
	}

	file, err := tcpListener.File()
	if err != nil {
		return err
	}
	defer file.Close()

	return windows.SetsockoptInt(windows.Handle(file.Fd()), windows.SOL_SOCKET, windows.SO_REUSEADDR, 1)
}
