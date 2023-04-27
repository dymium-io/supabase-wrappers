package utils

import (
	"regexp"
	"fmt"
)

var (
	Timestamp_r, Timestamp_with_zone_r,
	Interval_year_r, Interval_day_r *regexp.Regexp
)

func init() {
	var err error
	if Timestamp_r,err = regexp.Compile(`^timestamp *(\(\d+\))? *$`); err != nil {
		panic(fmt.Sprintf("Invalid regular expression timestamp_r: %v", err))
	}
	if Timestamp_with_zone_r,err = regexp.Compile(`^timestamp( *\( *\d+ *\))? +with( +local)? +time +zone( *\( *\d+ *\))? *$`);
	err != nil {
		panic(fmt.Sprintf("Invalid regular expression timestamp_with_zone_r: %v", err))
	}
	if Interval_year_r,err = regexp.Compile(`^interval +year( *\( *\d+ *\))? +to +month *$`);
	err != nil {
		panic(fmt.Sprintf("Invalid regular expression interval_year_r: %v", err))
	}
	if Interval_day_r,err = regexp.Compile(`^interval +day( *\( *\d+ *\))? +to +second( *\(\d+\))? *$`);
	err != nil {
		panic(fmt.Sprintf("Invalid regular expression interval_day_r: %v", err))
	}
}
