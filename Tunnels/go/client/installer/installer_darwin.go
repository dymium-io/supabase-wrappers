package installer

import (
	"errors"
	"fmt"
	"net/http"
	"os"
	"io"
    "os/exec"
	"github.com/apex/log"
)

func UpdateInstaller(portalUrl string) error {

	url := fmt.Sprintf("%sDymiumInstaller.pkg", portalUrl)

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

	fi, err := os.Create("/tmp/DymiumInstaller.pkg")
	if err != nil {
		log.Errorf("Error creating file: %s", err.Error())
	}
	fi.Write(body)
	fi.Close()
	os.Chmod("/tmp/DymiumInstaller.pkg", 0777)

	cmd := exec.Command("open", "/tmp/DymiumInstaller.pkg")
	cmd.Run()
	return err
}
