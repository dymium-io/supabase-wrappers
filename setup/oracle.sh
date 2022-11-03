#!/bin/zsh

set -e

script_name=$0

declare -a linux_packages
linux_path='https://download.oracle.com/otn_software/linux/instantclient/218000'
linux_packages+=('instantclient-basic-linux.x64-21.8.0.0.0dbru.zip'
	'instantclient-sdk-linux.x64-21.8.0.0.0dbru.zip')

declare -a darwin_packages
darwin_path='https://download.oracle.com/otn_software/mac/instantclient/198000'
darwin_packages+=('instantclient-basic-macos.x64-19.8.0.0.0dbru.zip'
	'instantclient-sqlplus-macos.x64-19.8.0.0.0dbru.zip'
	'instantclient-sdk-macos.x64-19.8.0.0.0dbru.zip')

usage() {
	echo "Usage: ${script_name} <get|copy-to-aws|copy-from-aws> <linux|darwin|all>"
}

main() {
	case "$1" in
	get)
		shift
		case "$1" in
		linux)
			get_pack ${linux_path} ${linux_packages}
			exit $?
			;;
		darwin)
			get_pack ${darwin_path} ${darwin_packages}
			exit $?
			;;
		all)
			get_pack ${linux_path} ${linux_packages}
			r=$?
			[ $r -eq 0 ] || exit $r
			get_pack ${darwin_path} ${darwin_packages}
			exit $?
			;;
		*)
			usage
			exit -1
			;;
		esac
		;;
	copy-to-aws)
		shift
		case "$1" in
		linux)
			copy_to_aws ${linux_packages}
			exit $?
			;;
		darwin)
			copy_to_aws ${darwin_packages}
			exit $?
			;;
		all)
			copy_to_aws ${linux_packages} ${darwin_packages}
			exit $?
			;;
		*)
			usage
			exit -1
			;;
		esac
		;;
	copy-from-aws)
		shift
		case "$1" in
		linux)
			copy_from_aws ${linux_packages}
			exit $?
			;;
		darwin)
			copy_from_aws ${darwin_packages}
			exit $?
			;;
		all)
			copy_from_aws ${linux_packages} ${darwin_packages}
			exit $?
			;;
		*)
			usage
			exit -1
			;;
		esac
		;;
	*)
		usage
		exit
		;;
	esac
}

get_pack() {
	local pth=$1
	shift
	local packages=($@)
	mkdir -p oracle
	for p in ${packages[@]}; do
		echo "curl $pth/$p -o oracle/$p"
		curl $pth/$p -o oracle/$p
		r=$?
		[ $r -eq 0 ] || {
			return $r
		}
	done
	return 0
}

copy_to_aws() {
	packages=($@)
	for p in ${packages[@]}; do
		[ -f oracle/$p ] || {
			echo "=> package $p is not present in the oracle directory"
			usage
			return -1
		}
	done
	for p in ${packages[@]}; do
		md5=$(md5sum oracle/$p | cut -d ' ' -f 1)
		echo "aws s3 cp --profile dymium --region us-west-2 oracle/$p s3://dymium-dev/oracle/$p --metadata md5=$md5"
		aws s3 cp --profile dymium --region us-west-2 oracle/$p s3://dymium-dev/oracle/$p --metadata md5=$md5
		r=$?
		[ $r -eq 0 ] || {
			return $r
		}
	done
	return 0
}

copy_from_aws() {
	packages=($@)
	for p in ${packages[@]}; do
		if [ -f oracle/$p ]; then
			md5=$(md5sum oracle/$p | cut -d ' ' -f 1)
			meta="$(aws --profile dymium --region us-west-2 s3api head-object --bucket dymium-dev --key oracle/${p})"
			[ $? -eq 0 ] || {
				echo "package $p not present on AWS S3"
				exit -1
			}
			if [ "$md5" = "$(echo ${meta} | jq -r '.Metadata.md5')" ]; then
				echo "=> $p is up to date"
			else
				echo "aws s3 cp --profile dymium --region us-west-2 s3://dymium-dev/oracle/$p oracle/$p"
				aws s3 cp --profile dymium --region us-west-2 s3://dymium-dev/oracle/$p oracle/$p
				r=$?
				[ $r -eq 0 ] || {
					return $r
				}
			fi
		else
			echo "aws s3 cp --profile dymium --region us-west-2 s3://dymium-dev/oracle/$p oracle/$p"
			aws s3 cp --profile dymium --region us-west-2 s3://dymium-dev/oracle/$p oracle/$p
			r=$?
			[ $r -eq 0 ] || {
				return $r
			}
		fi
	done
	return 0
}

main $1 $2
