package detect

import (
	// "github.com/aws/aws-sdk-go/aws"
	// "github.com/aws/aws-sdk-go/aws/session"
	//"github.com/aws/aws-sdk-go/service/comprehend"

	"fmt"
	"strings"
)

func tryComprehend(sample []Sample) error {

	fmt.Println(sample)

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
		inp.WriteByte('.')
	}

	fmt.Println(inp.String())

	/*
		awsInp = make([]*string, len(inp))
		for k := 0; k != len(inp); k++ {
			awsInp[k] = aws.String(inp[k].String())
		}

		comprehendInp := &comprehend.BatchDetectEntitiesInput{
			TextList: awsInp,
			LanguageCode: aws.String("en"),
		}

		output, err := client.BatchDetectEntities(comprehendInp)
		if err != nil {
			return err
		}

		// Process the output from Comprehend
		for _, result := range output.ResultList {
			for _, entity := range result.Entities {
				// Check if the entity is PII
				if aws.StringValue(entity.Type) == "PERSONAL_IDENTIFIABLE_INFORMATION" {
					// This is PII, do something with it...
				}
			}
		}
	*/

	return nil
}
