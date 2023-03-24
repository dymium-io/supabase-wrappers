// +build !windows

package selfupdate
// original code in https://github.com/inconshreveable/go-update
// Apache license: https://github.com/inconshreveable/go-update/blob/master/LICENSE
// changes from https://github.com/minio/selfupdate
//https://github.com/minio/selfupdate/blob/master/LICENSE

func hideFile(path string) error {
	return nil
}
