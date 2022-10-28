package main

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"os/exec"
)


func (g *Gui) GetClientPath() string{
	return "./dymium"
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
	var config Config
	err = json.Unmarshal(inp, &config)
	g.confs = config.Confs
	g.last = config.Last
	if err != nil {
		return
	}
}
func (g *Gui) WriteConfig() {
	var config Config
	config.Confs = g.confs
	config.Last = g.input.Text
	data, _ := json.Marshal(config)	
	os.WriteFile(g.GetConfigPath(), data, 0644)
}
func (g *Gui) Launch(cmdslice []string) *exec.Cmd {
	return exec.Command(g.GetClientPath(), cmdslice...)
}
