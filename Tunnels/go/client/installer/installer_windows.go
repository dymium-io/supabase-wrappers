package installer

import (
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	_"os/exec"
	"golang.org/x/sys/windows"
	"syscall"
	"github.com/apex/log"
)

func UpdateInstaller(portalUrl string) error {

	url := fmt.Sprintf("%sDymiumInstaller.exe", portalUrl)

	log.Infof("Downloading new version...")
	resp, err := http.Get(url)
	if err != nil {
		log.Errorf("Error getting installer: %s", url)
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		return errors.New(fmt.Sprintf("Error downloading update, status %d", resp.StatusCode))
	}
	body, err := io.ReadAll(resp.Body)
	if err != nil {
		log.Errorf("Error loading file: %s", err.Error())
		return err
	} else {
		log.Info("Installer downloaded")
	}

	tmp := os.Getenv("TEMP")
	name := filepath.Join(tmp, "DymiumInstaller.exe")
	fi, err := os.Create(name)
	if err != nil {
		log.Errorf("Error creating file: %s", err.Error())
	}
	fi.Write(body)
	fi.Close()
	
	verb := "runas"
	exe := "DymiumInstaller.exe" 
	cwd := tmp
	args := ""
	
	verbPtr, _ := syscall.UTF16PtrFromString(verb)
	exePtr, _ := syscall.UTF16PtrFromString(exe)
	cwdPtr, _ := syscall.UTF16PtrFromString(cwd)
	argPtr, _ := syscall.UTF16PtrFromString(args)
	
	var showCmd int32 = 1 //SW_NORMAL
	
	err = windows.ShellExecute(0, verbPtr, exePtr, argPtr, cwdPtr, showCmd)
	if err != nil {
		log.Errorf("Error elevating privilege", err.Error())
	}
	
	return err
}
