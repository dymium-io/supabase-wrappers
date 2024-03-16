#!/usr/bin/env zsh

set -e

script_name=$0

declare -a linux_packages
linux_version='v11.5.8_linuxx64'
linux_packages+=('v11.5.8_linuxx64_rtcl.tar.gz'
'v11.5.4_linuxx64_odbc_cli.tar.gz' 'db2_home.zip')


usage() {
	echo "Usage: ${script_name} <version|get|copy-to-aws|copy-from-aws> <linux|all>"
}

main() {
	case "$1" in
	version)
		shift
		case "$1" in
		linux)
			echo ${linux_version}
			exit 0
			;;
		*)
			usage
			exit -1
			;;
		esac
		;;
	packages)
		shift
		case "$1" in
		linux)
			echo ${linux_packages}
			exit 0
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
		all)
			copy_to_aws ${linux_packages}
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
		all)
			copy_from_aws ${linux_packages}
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

copy_to_aws() {
	packages=($@)
	for p in ${packages[@]}; do
		[ -f db2/$p ] || {
			echo "=> package $p is not present in the db2 directory"
			usage
			return -1
		}
	done
	for p in ${packages[@]}; do
		md5=$(md5sum db2/$p | cut -d ' ' -f 1)
		echo "aws s3 cp --profile dymium-dev --region us-west-2 db2/$p s3://dymium-dev/db2/$p --metadata md5=$md5"
		aws s3 cp --profile dymium-dev --region us-west-2 db2/$p s3://dymium-dev/db2/$p --metadata md5=$md5
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
		if [ -f db2/$p ]; then
			md5=$(md5sum db2/$p | cut -d ' ' -f 1)
			meta="$(aws --profile dymium-dev --region us-west-2 s3api head-object --bucket dymium-dev --key db2/${p})"
			[ $? -eq 0 ] || {
				echo "package $p not present on AWS S3"
				exit -1
			}
			if [ "$md5" = "$(echo ${meta} | jq -r '.Metadata.md5')" ]; then
				echo "=> $p is up to date"
			else
				echo "aws s3 cp --profile dymium-dev --region us-west-2 s3://dymium-dev/db2/$p db2/$p"
				aws s3 cp --profile dymium-dev --region us-west-2 s3://dymium-dev/db2/$p db2/$p
				r=$?
				[ $r -eq 0 ] || {
					return $r
				}
			fi
		else
			echo "aws s3 cp --profile dymium-dev --region us-west-2 s3://dymium-dev/db2/$p db2/$p"
			aws s3 cp --profile dymium-dev --region us-west-2 s3://dymium-dev/db2/$p db2/$p
			r=$?
			[ $r -eq 0 ] || {
				return $r
			}
		fi
	done
	return 0
}

main $1 $2
