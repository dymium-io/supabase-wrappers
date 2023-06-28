aws_usage() {
	if [ "$1" = "-exe" ]; then
		shift
		ex="$1"
		shift
	else
		ex="$0"
	fi
	[ -z "$1" ] || {
		echo -e "\033[1;31mError\033[0m: $1"
	}
	echo -e "\033[1;34mUsage\033[0m: $ex <subaccount> <tag> [region]"
	echo "  where:"
	echo -e "    \033[1msubaccount\033[0m is one of: production |stage |dev"
	echo -e "    \033[1mtag\033[0m is something like \"dymium-1.4\" or \"latest\""
	echo -e "    AWS \033[1mregion\033[0m is something like \"us-west-2\" or \"Oregon\" (the default)"
	exit 255
}
aws_params() {
	if [ "$1" = "-exe" ]; then
		shift
		ex="$1"
		shift
	else
		ex="$0"
	fi
	case "$1" in
	"production")
		ARN="482973908181"
		PROFILE="dymium-prod"
		shift
		;;
	"stage")
		ARN="626593984035"
		PROFILE="dymium-stage"
		shift
		;;
	"dev")
		ARN="564835066653"
		PROFILE="dymium-dev"
		shift
		;;
	"")
		aws_usage -exe "$ex" "mandatory <subaccount> parameter is not defined"
		;;
	*)
		aws_usage -exe "$ex" "Subaccount '$1' is not known"
		;;
	esac

	case "$1" in
	    "") aws_usage -exe "$ex" "mandatory <tag> parameter is not defined"
		;;
	    "" | "Oregon" | "us-west-2")
		aws_usage -exe "$ex" "<tag> parameter value \"$1\" does not look right"
		;;
	    *)
		tag="$1"
		shift
		;;
	esac

	
	case "$1" in
	"" | "Oregon" | "us-west-2") # default
		REGION="us-west-2"
		shift
		;;
	"Ohio" | "us-east-2")
		REGION="us-east-2"
		shift
		;;
	"Virginia" | "us-east-1")
		REGION="us-east-1"
		shift
		;;
	"California" | "us-west-1")
		REGION="us-west-1"
		shift
		;;
	"Africa" | "Cape Town" | "af-south-1")
		REGION="af-south-1"
		shift
		;;
	"Hong Kong" | "ap-east-1")
		REGION="ap-east-1"
		shift
		;;
	"Hyderabad" | "ap-south-2")
		REGION="ap-south-2"
		shift
		;;
	"Jakarta" | "ap-southeast-3")
		REGION="ap-southeast-3"
		shift
		;;
	"Mumbai" | "ap-south-1")
		REGION="ap-south-1"
		shift
		;;
	"Osaka" | "ap-northeast-3")
		REGION="ap-northeast-3"
		shift
		;;
	"Seoul" | "ap-northeast-2")
		REGION="ap-northeast-2"
		shift
		;;
	"Singapore" | "ap-southeast-1")
		REGION="ap-southeast-1"
		shift
		;;
	"Sydney" | "ap-southeast-2")
		REGION="ap-southeast-2"
		shift
		;;
	"Tokyo" | "ap-northeast-1")
		REGION="ap-northeast-1"
		shift
		;;
	"Canada" | "ca-central-1")
		REGION="ca-central-1"
		shift
		;;
	"Frankfurt" | "eu-central-1")
		REGION="eu-central-1"
		shift
		;;
	"Ireland" | "eu-west-1")
		REGION="eu-west-1"
		shift
		;;
	"London" | "eu-west-2")
		REGION="eu-west-2"
		shift
		;;
	"Milan" | "eu-south-1")
		REGION="eu-south-1"
		shift
		;;
	"Paris" | "eu-west-3")
		REGION="eu-west-3"
		shift
		;;
	"Spain" | "eu-south-2")
		REGION="eu-south-2"
		shift
		;;
	"Stockholm" | "eu-north-1")
		REGION="eu-north-1"
		shift
		;;
	"Zurich" | "eu-central-2")
		REGION="eu-central-2"
		shift
		;;
	"Bahrain" | "me-south-1")
		REGION="me-south-1"
		shift
		;;
	"UAE" | "me-central-1")
		REGION="me-central-1"
		shift
		;;
	"São Paulo" | "sa-east-1")
		REGION="sa-east-1"
		shift
		;;
	"us-gov-east-1")
		REGION="us-gov-east-1"
		shift
		;;
	"us-gov-west-1")
		REGION="us-gov-west-1"
		shift
		;;
	*)  aws_usage -exe "$ex" "Region $1 is not known" ;;
	esac
}
