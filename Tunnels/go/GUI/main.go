
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
	"os"
	"strings"
	"fmt"
)


type Cfg struct {
    Name string
	Command string 
}
type Config struct {
	Last string 
	Confs []Cfg
}
type Gui struct {
	input, intro, name   *widget.Entry
	button *widget.Button
	title, memory, area, top *fyne.Container
	checkbox *widget.Check
	split *container.Split
	sidebar *widget.List
	nameEnabled bool
	active bool
	confs  []Cfg
	last  string
	pid *os.Process
}
func  (g *Gui) AddConnection(name, connection string) {
	for i, conf := range g.confs {
		if(conf.Name == name) {
			g.confs = append(g.confs[:i], g.confs[i+1:]... )
			break
		}
	}
	g.confs = append(g.confs,  Cfg{name, connection})
	g.WriteConfig()
}
func (g *Gui) DeleteConnection(id int) {
	g.confs = append(g.confs[:id], g.confs[id+1:]... )
	g.WriteConfig()
}

func (g *Gui) LaunchDymium() error {
	cmdstr := g.input.Text
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
	g.intro.SetText("")
	cmd := g.Launch(cmdslice)

	stdout, err := cmd.StdoutPipe()
	cmd.Stderr = cmd.Stdout
	//d, _ := os.Getwd()
	//g.intro.SetText(g.intro.Text  +d + "\n")

	if err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		g.intro.SetText(g.intro.Text  + err.Error() + "\n")
		return err
	}
	if err = cmd.Start(); err != nil {
		fmt.Printf("Error: %s\n", err.Error())
		g.intro.SetText(g.intro.Text  + err.Error() + "\n")
		return err
	}
	g.pid = cmd.Process
	g.WriteConfig()


	for {
		tmp := make([]byte, 1024)
		n, err := stdout.Read(tmp)
		line := string(tmp[:n])
		g.intro.SetText(g.intro.Text  + line)
		if err != nil {
			fmt.Printf("Error: %s\n", err.Error())
			g.intro.SetText(g.intro.Text  + "\nClient disconnected")
			
			break
		}
		fmt.Println(line)
	}	
	g.SetInactive()
	g.pid = nil
	return err
}
func (g *Gui) SetInactive() {
	g.button.SetText("Connect")
	g.active = false
	if(g.pid != nil) {
		g.pid.Kill()
	}
}

func (g *Gui) Run() {
	g.ReadConfig()

    a := app.New()
	a.Settings().SetTheme(theme.DarkTheme()) 
    w := a.NewWindow("Dymium Secure Connection")

    w.SetMaster()

    //content := container.NewMax()

	g.input = widget.NewEntry()
	g.input.SetPlaceHolder("Enter connection string...")
	g.input.SetText(g.last)
	g.button = widget.NewButton("Connect", func() {
		if(g.active) {
			g.SetInactive()
		} else {
			if( g.input.Text == "") {
				dialog.ShowError(errors.New("Please specify connection parameters"), w)
				return
			}
			if( g.checkbox.Checked && g.name.Text == "") {
				dialog.ShowError(errors.New("Please specify connection name"), w)
				return
			}
			if( g.checkbox.Checked) {
				g.AddConnection(g.name.Text, g.input.Text)
				g.sidebar.Refresh()
			}

			g.active = true
			g.button.SetText("Abort")
			go g.LaunchDymium()
		}
	})
	paste := widget.NewButtonWithIcon("", theme.ContentPasteIcon(),  func() {
		text := w.Clipboard().Content()
		g.input.SetText( text )
		params := strings.Split(text, " ")
		if( ! strings.HasSuffix(params[2], "org_")){
			g.name.SetText(params[2])
			g.checkbox.SetChecked(true)
		}
		g.WriteConfig()
	}) 
	
	pair := container.New(layout.NewHBoxLayout(), paste, layout.NewSpacer(), g.button)
	
	g.title = container.NewBorder(nil, nil, nil, pair, g.input)
	g.checkbox = widget.NewCheck("Remember connection", func(b bool) {
		if(g.nameEnabled) {
			g.name.Disable()
		} else {
			g.name.Enable()
		}
		g.nameEnabled = !g.nameEnabled
	})
	g.name = widget.NewEntry()
	g.name.SetPlaceHolder("Give it a name...")
	g.name.Disable()
	g.nameEnabled = false
	g.memory = container.NewBorder(nil, nil, g.checkbox, nil, g.name)

	g.top = container.NewVBox(g.title, g.memory)

	g.intro = widget.NewEntry()
    g.intro.SetText(``)
	g.intro.MultiLine = true 
	//intro.Disable() 

	g.area = container.NewBorder(g.top, nil, nil, nil, g.intro)

	g.sidebar = widget.NewList(func() int {
		return len(g.confs)
	}, func() fyne.CanvasObject {
		return container.NewBorder(nil, nil, nil, widget.NewButtonWithIcon("", theme.DeleteIcon(), nil),
			widget.NewButton("xxxххххххххх", func() {}),
		) 
	}, func(id widget.ListItemID, object fyne.CanvasObject) {
		box := object.(*fyne.Container)
		butt := box.Objects[0].(*widget.Button)
		del := box.Objects[1].(*widget.Button)


		conf := g.confs[id]
		needRefresh := butt.Text != conf.Name
		butt.SetText( conf.Name )
		if(needRefresh) {
			butt.FocusGained()
			butt.FocusLost()
		}
		butt.OnTapped = func() {
			if(g.active) {
				return
			}
			conn := g.confs[id].Command
			g.input.SetText(conn)
		}
		del.OnTapped = func() {
			g.DeleteConnection(id)
			g.sidebar.Refresh()
		}

	})

    g.split = container.NewHSplit(g.sidebar, g.area)

    g.split.Offset = 0
    w.SetContent(g.split)

    w.Resize(fyne.NewSize(660, 300))
	w.SetFixedSize(true)
    w.ShowAndRun()
	if(g.pid != nil) {
		g.pid.Kill()
	}
	fmt.Println("Exited")
}
func main() {

	gui :=  Gui{}
	gui.Run()

}
/*
var items []Item = 
   []Item {
	{"Facebook", "./dymium -c org_RWhgMvJ6alaosBUy -p https://portal.dymium.us/"},
	{"Twitter", "./dymium -c org_YYYZMvJ6alaosXXX -p https://portal.dymium.us/"},
}
*/