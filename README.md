# Dev

## Dymium Binary Utilities via Git Annex

This repository uses git-annex to manage binaries that are stored in the `dymium-dev-binary-utilities` S3 bucket. Developers intending to use or modify the binaries should follow the instructions below to install them to the `./bin` directory.

### Prerequisites
1. Install git-annex on your machine (either `brew install git-annex` on MacOS, or `apt install git-annex` on Linux).
2. Get AWS access keys to `dymium-dev` subaccount (this is where the S3 bucket is located).

### Setup
1. Make sure that your access keys in `~/.aws/creadentials` are up-to-date. 
2. `cd` to the root of this repository on your machine
3. Initialize git-annex: 
   ```bash
	git annex init 'description of this repo/machine...'
	```
4. Enable remote:
   ```bash
   git annex enableremote dymium-dev-binary-utilities
   ```
5. Install binaries:
   ```bash
   ./setup/with-aws.sh git annex get --from=dymium-dev-binary-utilities bin/
   ```
