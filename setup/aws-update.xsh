#!/usr/bin/env python3

import configparser
import os
import subprocess

cred_file = os.path.expanduser("~/.aws/credentials")

config = configparser.ConfigParser()
with open(cred_file, "r") as fp:
    config.read_file(fp)

# Capture the output of the pbpaste command
cred_process = subprocess.Popen(["pbpaste"], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
cred_output, _ = cred_process.communicate()

data = configparser.ConfigParser()
data.read_string(cred_output)

s = data.sections()[0]
section = None

if s == '391714386929_AdministratorAccess':
    section = 'demo-spoofcorp'
elif s == '411064808315_AdministratorAccess':
    section = 'dymium'
elif s == '564835066653_AdministratorAccess':
    section = 'dymium-dev'
elif s == '482973908181_AdministratorAccess':
    section = 'dymium-prod'
elif s == '655193105197_AdministratorAccess':
    section = 'dymium-sandbox'
elif s == '626593984035_AdministratorAccess':
    section = 'dymium-stage'
elif s == '570319887642_AdministratorAccess':
    section = 'test-org'
elif s == '647668538014_AdministratorAccess':
    section = 'prod-support'
else:
    print(f'Unknown subaccount {s}')
    exit(-1)

config[section] = {
    'aws_access_key_id': data[s]['aws_access_key_id'],
    'aws_secret_access_key': data[s]['aws_secret_access_key']
}

try:
    config[section]['aws_session_token'] = data[s]['aws_session_token']
except KeyError:
    pass

# Save the updated credentials to the file
with open(cred_file, 'w') as fp:
    config.write(fp)
