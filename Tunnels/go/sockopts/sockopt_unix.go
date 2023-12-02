//go:build !windows
// +build !windows

package sockopts

import (
	"net"
	"syscall"
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

	return syscall.SetsockoptInt(int(file.Fd()), syscall.SOL_SOCKET, syscall.SO_REUSEADDR, 1)
}
