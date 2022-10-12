package main

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"os/exec"
	"os/user"
	"errors"
	"fmt"
)
func (g *Gui) GetClientPath() string{
	return "../MacOS/dymium"
}

func (g *Gui) GetConfigPath() string {
	currentUser, _ := user.Current()

	dymium := currentUser.HomeDir + "/.dymium"
	if _, err := os.Stat(dymium); errors.Is(err, os.ErrNotExist) {
		err := os.Mkdir(dymium, os.ModePerm)
		if err != nil {
			fmt.Println(err)
		}
	}

	return dymium + "/config.txt"
}
func (g *Gui) ReadConfig() {
	inp, err := ioutil.ReadFile(g.GetConfigPath())
	if err != nil {
		return
	}
	err = json.Unmarshal(inp, &g.confs)
	if err != nil {
		return
	}
}
func (g *Gui) WriteConfig() {
	data, _ := json.Marshal(g.confs)
	os.WriteFile(g.GetConfigPath(), data, 0644)
}
func (g *Gui) Launch(cmdslice []string) *exec.Cmd {
	return exec.Command(g.GetClientPath(), cmdslice...)
}
