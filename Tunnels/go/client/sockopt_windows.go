//go:build windows
// +build windows

package main

import (
	"golang.org/x/sys/windows"
	"net"
)

func setReuseAddr(listener net.Listener) error {
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
