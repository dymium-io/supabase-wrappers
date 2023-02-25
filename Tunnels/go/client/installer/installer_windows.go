package installer

import (
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"

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

	fi, err := os.Create("/tmp/DymiumInstaller.exe")
	if err != nil {
		log.Errorf("Error creating file: %s", err.Error())
	}
	fi.Write(body)
	fi.Close()
	//os.Chmod("/tmp/DymiumInstaller.pkg", 0777)

	cmd := exec.Command("/tmp/DymiumInstaller.exe")
	cmd.Run()
	return err
}
