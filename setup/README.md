# Miscellaneous utilities #

## aws-update.xsh ##
updates `~/.aws/credentials` with subaccount credentials.
How to use: go to [Dymium SSO Page](https://dymium.awsapps.com/start)
Select `Command line or programmatic access` for the subaccount, and then
to `Option 2: Add a profile to your AWS credentials file`. Click on the
selected credentials to copy them to the Clipboard.
Then, run `aws-update.xsh` to update `~/.aws/credentials` file.

## get-databases.sh ##
copy `northwind` and `adventureworks` test databases from S3.

## oracle.sh ##
a script to get `instantclient` software from Oracle development
site.
It has a number of options to be used by different utilities.
Normally, the usage should be:
`./oracle.sh copy-from-aws linux`

## start-db ##
utility to run dockers with different databases.
The databases descriptions (accounts, passwords, etc) are supposed
to be defined in `~/.config/dymium/databases.yaml` file.

## commit-msg ##
this script should be put into `../.git/hooks` directory.
It adds `[<branch name>]` tag to the beginning of each commit to track
changes in PRs.
