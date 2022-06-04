package main

import (
        "github.com/aws/aws-lambda-go/lambda"
	
        "fmt"

	"os"
	"os/exec"
	
	"strings"
	"io"
	"io/ioutil"

	"path/filepath"

	"log"

	"sort"

	"encoding/json"
	"encoding/base64"
)

type Document struct {
        Name           string          `json:"name"`
        FileExtension  string          `json:"fileExtension"`
        DocumentId     string          `json:"documentId"`
        LoanId         string          `json:"loanId"`
        UserId         string          `json:"userId"`
        ForWhom        string          `json:"forWhom"`
        BuiltFrom      json.RawMessage `json:"builtFrom"`
        DocumentBase64 string          `json:"documentBase64"`
        PngResolution  string          `json:"pngResolution"`
        PngPagesBase64 []string        `json:"pngPagesBase64"`
}

const docName = "doc.pdf"
const pngDirName = "png"


// define struct to sort files in png directory
type nfT struct {
	num int
	nam string
}
type nfL []nfT

func (nfl nfL) Len() int {
	return len(nfl)
}
func (nfl nfL) Swap(i, j int) {
	nfl[i], nfl[j] = nfl[j], nfl[i]
}
func (nfl nfL) Less(i, j int) bool {
	return nfl[i].num < nfl[j].num
}

func LambdaHandler(doc Document) (*Document, error) {

	var tmpDir string
	var err error
	
	if tmpDir, err = ioutil.TempDir("", "pngsplitter"); err != nil {
		log.Printf("error %s:",err)
		return nil, err
	}
	defer os.RemoveAll(tmpDir)

	// store document to drive
	if docF, err := os.Create(filepath.Join(tmpDir, docName)); err != nil {
		log.Printf("error %s:",err)
		return nil, err
	} else {
		defer docF.Close()
		decoder := base64.NewDecoder(base64.StdEncoding, strings.NewReader(doc.DocumentBase64))
		if _, err = io.Copy(docF, decoder); err != nil {
			log.Printf("error %s:",err)
			return nil, err
		}
	}
	doc.DocumentBase64 = ""

	// create directory for .png images
	pngDir := filepath.Join(tmpDir,pngDirName)
	if err = os.MkdirAll(pngDir, 0700); err != nil {
		log.Printf("error %s:",err)
		return nil, err
	}

	
	cmd := exec.Command("pdftoppm", "-png", docName, filepath.Join(pngDirName,"z"))
	cmd.Dir = tmpDir
	if out, err := cmd.CombinedOutput(); err != nil {
		log.Printf("error %s:\n\n%s\n",err,out)
		return nil, fmt.Errorf("error %s:\n\n%s",err,out)
	}

	var files []os.FileInfo
	pngs := make([]string, 0)
	if files, err = ioutil.ReadDir(pngDir); err != nil {
		log.Printf("error %s:",err)
		return nil, err
	}
	
	nfl := make(nfL, 0)
	for _, f := range files {
		if !f.IsDir() {
			nf := nfT{
				nam: f.Name(),
				num: 0,
			}
			if _, err := fmt.Sscanf(nf.nam, "z-%d.png", &nf.num); err != nil {
				log.Printf("error %s:",err)
				return nil, err
			}
			nfl = append(nfl, nf)
		}
	}
	sort.Sort(nfl)
	for _, nf := range nfl {
		if png, err := Base64FileEncode(filepath.Join(tmpDir, pngDirName, nf.nam)); err != nil {
			log.Printf("error %s:",err)
			return nil, err
		} else {
			pngs = append(pngs, png)
		}
	}

        doc.PngResolution = "150"
        doc.PngPagesBase64 = pngs
	
	return &doc, nil
}

func Base64FileEncode(fn string) (string, error) {
	fp, err := os.Open(fn)
	if err != nil {
		return "", err
	}
	defer fp.Close()

	var b64 strings.Builder
	encoder := base64.NewEncoder(base64.StdEncoding, &b64)
	// Magic! Copy from base64 decoder to output buffer
	io.Copy(encoder, fp)
	return b64.String(), nil
}

func main() {
        lambda.Start(LambdaHandler)
}
