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
	err = json.Unmarshal(inp, &g.confs)
	if err != nil {
		return
	}
}
func (g *Gui) WriteConfig() {
	data, _ := json.Marshal(g.confs)
	os.WriteFile("config.txt", data, 0644)
}
func (g *Gui) Launch(cmdslice []string) *exec.Cmd {
	return exec.Command("client", cmdslice...)
}
