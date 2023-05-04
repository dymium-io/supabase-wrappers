package detect

import (
	"fmt"
	"strings"

	"aws"
)

func tryComprehend(sample []Sample, comprehendDetected map[string]string) error {

	size := 0
	for _, s := range sample {
		if s.IsSamplable {
			size = len(s.Data)
			break
		}
	}
	if size == 0 {
		return nil
	}

	var inp strings.Builder

	type loc struct {
		col int
		beg int
		end int
	}
	lims := make([]loc, 0)

	for k := 0; k != size; k++ {
		start := true
		for kk, s := range sample {
			if s.IsSamplable {
				if start {
					inp.WriteString(" The value of ")
					start = false
				} else {
					inp.WriteString(", and the value of ")
				}
				l := loc{
					col: kk,
					beg: inp.Len(),
				}
				inp.WriteString(s.Name)
				if s.Data[k] == nil {
					l.end = inp.Len()
					inp.WriteString(" is not dfined")
				} else {
					inp.WriteString(" is ")
					inp.WriteString(*s.Data[k])
					l.end = inp.Len()
				}
				lims = append(lims, l)
			}
		}
		inp.WriteString(".\n")
	}

	piis, err := aws.Comprehend(inp.String())
	if err != nil {
		return err
	}

	for _, pii := range piis {
		for _, l := range lims {
			if pii.BeginOffset >= l.beg && pii.EndOffset <= l.end {
				if t, ok := comprehendDetected[pii.Type]; ok {
					sample[l.col].Semantics = &t
				} else {
					fmt.Printf("Undefined PII type %s (ignored)\n", pii.Type)
				}
				break
			}
		}
	}

	return nil
}
