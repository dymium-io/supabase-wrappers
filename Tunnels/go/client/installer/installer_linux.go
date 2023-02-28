package installer

import (
	"dymium.com/client/selfupdate"
	"errors"
	"fmt"
	"github.com/apex/log"
	"net/http"
	"os"
	"runtime"
)

func UpdateInstaller(portalUrl string) error {
	url := fmt.Sprintf("%sapi/downloadupdate?os=%s&arch=%s", portalUrl, runtime.GOOS, runtime.GOARCH)

	log.Infof("Downloading new version...")
	resp, err := http.Get(url)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode != 200 {
		return errors.New(fmt.Sprintf("Error downloading update, status %d", resp.StatusCode))
	}
	ex, _ := os.Executable()
	log.Infof("Updating the client...")
	err = selfupdate.Apply(resp.Body, selfupdate.Options{ex, 0, nil, 0, ex + ".old"})
	if err != nil {
		log.Errorf("Error updating: %s", err.Error())
		if rerr := selfupdate.RollbackError(err); rerr != nil {
			log.Errorf("Failed to rollback from bad update: %s", rerr.Error())
		}
		// error handling
	} else {
		log.Infof("Utility successfully updated, please restart the client.")
	}
	return err
}
