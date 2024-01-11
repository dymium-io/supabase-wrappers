package selfupdate

// original code in https://github.com/inconshreveable/go-update
// Apache license: https://github.com/inconshreveable/go-update/blob/master/LICENSE
// changes from https://github.com/minio/selfupdate
//https://github.com/minio/selfupdate/blob/master/LICENSE

import (
	"syscall"
	"unsafe"
)

func hideFile(path string) error {
	kernel32 := syscall.NewLazyDLL("kernel32.dll")
	setFileAttributes := kernel32.NewProc("SetFileAttributesW")

	r1, _, err := setFileAttributes.Call(uintptr(unsafe.Pointer(syscall.StringToUTF16Ptr(path))), 2)

	if r1 == 0 {
		return err
	} else {
		return nil
	}
}
func GetEx() string {
	return "meshconnector.exe"
}
