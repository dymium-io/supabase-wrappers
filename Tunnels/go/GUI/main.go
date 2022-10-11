
package main
import (
	"errors"
    "fyne.io/fyne/v2"
    "fyne.io/fyne/v2/app"
    "fyne.io/fyne/v2/container"
    "fyne.io/fyne/v2/widget"
	"fyne.io/fyne/v2/layout"
	"fyne.io/fyne/v2/theme"
	"fyne.io/fyne/v2/dialog"
	"encoding/json"
	"io/ioutil"
	"os/exec"
	"os"
	"strings"
	"fmt"
)


type Cfg struct {
    Name, Command string
}


var input, intro, name   *widget.Entry
var button *widget.Button
var title, memory, area, top *fyne.Container
var checkbox *widget.Check
var split *container.Split
var sidebar *widget.List
var nameEnabled bool
var active = false
var confs = []Cfg{ }

var pid *os.Process

func AddConnection(name, connection string) {
	for i, conf := range confs {
		if(conf.Name == name) {
			confs = append(confs[:i], confs[i+1:]... )
			break
		}
	}
	confs = append(confs,  Cfg{name, connection})
	WriteConfig()
}
func DeleteConnection(id int) {
	confs = append(confs[:id], confs[id+1:]... )
	WriteConfig()
}
func ReadConfig() {
	inp, err := ioutil.ReadFile("config.txt")
	if err != nil {
		return
	}
	err = json.Unmarshal(inp, &confs)
	if err != nil {
		return
	}
}
func WriteConfig() {
	data, _ := json.Marshal(confs)
	os.WriteFile("config.txt", data, 0644)
}
func LaunchDymium() error {
	cmdstr := input.Text
	cmdslice := strings.Split(cmdstr, " ")
	if(
		cmdslice[0] == "tunnel" || 
		cmdslice[0] == "tunnel.exe" || 
		cmdslice[0] == "./tunnel" || 
		cmdslice[0] == "./tunnel.exe" || 
		cmdslice[0] == "client" || 
		cmdslice[0] == "client.exe" || 
		cmdslice[0] == "dymium" || 
		cmdslice[0] == "dymium.exe" ||
		cmdslice[0] == "./client" || 
		cmdslice[0] == "./client.exe" || 
		cmdslice[0] == "./dymium" || 
		cmdslice[0] == "./dymium.exe"){
		cmdslice = cmdslice[1:]
	}
	intro.SetText("")
	cmd := exec.Command("./client", cmdslice...)
	stdout, err := cmd.StdoutPipe()
	cmd.Stderr = cmd.Stdout
	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		return err
	}
	if err = cmd.Start(); err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		return err
	}
	pid = cmd.Process

	for {
		tmp := make([]byte, 1024)
		n, err := stdout.Read(tmp)
		line := string(tmp[:n])
		intro.SetText(intro.Text  + line)
		if err != nil {
			fmt.Printf("Error: %s\n", err.Error())
			break
		}
		fmt.Println(line)
	}	
	pid.Kill()
	pid = nil
	return err
}
func main() {
	ReadConfig()

    a := app.New()
	a.Settings().SetTheme(theme.DarkTheme()) 
    w := a.NewWindow("Dymium Secure Connection")

    w.SetMaster()

    //content := container.NewMax()

	input = widget.NewEntry()
	input.SetPlaceHolder("Enter connection string...")
	button = widget.NewButton("Connect", func() {
		if(active) {
			button.SetText("Connect")
			active = false

		} else {
			if( input.Text == "") {
				dialog.ShowError(errors.New("Please specify connection parameters"), w)
				return
			}
			if( checkbox.Checked && name.Text == "") {
				dialog.ShowError(errors.New("Please specify connection name"), w)
				return
			}
			if( checkbox.Checked) {
				AddConnection(name.Text, input.Text)
				sidebar.Refresh()
			}

			active = true
			button.SetText("Abort")
			go LaunchDymium()
		}
	})
	paste := widget.NewButtonWithIcon("", theme.ContentPasteIcon(),  func() {
		input.SetText( w.Clipboard().Content() )
	}) 
	
	pair := container.New(layout.NewHBoxLayout(), paste, layout.NewSpacer(), button)
	//title := container.New(layout.NewGridLayout(2), input, button)
	title = container.NewBorder(nil, nil, nil, pair, input)
	checkbox = widget.NewCheck("Remember connection", func(b bool) {
		if(nameEnabled) {
			name.Disable()
		} else {
			name.Enable()
		}
		nameEnabled = !nameEnabled
	})
	name = widget.NewEntry()
	name.SetPlaceHolder("Give it a name...")
	name.Disable()
	nameEnabled = false
	memory = container.NewBorder(nil, nil, checkbox, nil, name)

	top = container.NewVBox(title, memory)

	intro = widget.NewEntry()
    intro.SetText(``)
	intro.MultiLine = true 
	//intro.Disable() 

	area = container.NewBorder(top, nil, nil, nil, intro)

	sidebar = widget.NewList(func() int {
		return len(confs)
	}, func() fyne.CanvasObject {
		return container.NewBorder(nil, nil, nil, widget.NewButtonWithIcon("", theme.DeleteIcon(), nil),
			widget.NewButton("xxx", func() {}),
		) 
	}, func(id widget.ListItemID, object fyne.CanvasObject) {
		box := object.(*fyne.Container)
		butt := box.Objects[0].(*widget.Button)
		del := box.Objects[1].(*widget.Button)


		conf := confs[id]
		needRefresh := butt.Text != conf.Name
		butt.SetText( conf.Name )
		if(needRefresh) {
			butt.FocusGained()
			butt.FocusLost()
		}
		butt.OnTapped = func() {
			if(active) {
				return
			}
			conn := confs[id].Command
			input.SetText(conn)
		}
		del.OnTapped = func() {
			DeleteConnection(id)
			sidebar.Refresh()
		}

	})

    split = container.NewHSplit(sidebar, area)

    split.Offset = 0
    w.SetContent(split)

    w.Resize(fyne.NewSize(600, 300))
    w.ShowAndRun()
	if(pid != nil) {
		pid.Kill()
	}
	fmt.Println("Exited")
}
/*
var items []Item = 
   []Item {
	{"Facebook", "./dymium -c org_RWhgMvJ6alaosBUy -p https://portal.dymium.us/"},
	{"Twitter", "./dymium -c org_YYYZMvJ6alaosXXX -p https://portal.dymium.us/"},
}
*/