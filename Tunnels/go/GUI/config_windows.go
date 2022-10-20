package main

import (
	"encoding/json"
	"io/ioutil"
	"os"
	"os/exec"
)

func (g *Gui) ReadConfig() {
	inp, err := ioutil.ReadFile("config.txt")
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
	os.WriteFile("config.txt", data, 0644)
}
func (g *Gui) Launch(cmdslice []string) *exec.Cmd {
	return exec.Command("client", cmdslice...)
}
