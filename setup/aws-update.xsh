#!/usr/bin/env xonsh

import configparser

cred_file=f"{$HOME}/.aws/credentials"

config=configparser.ConfigParser()
with open(cred_file) as fp:
    config.read_file(fp,cred_file)

cred=$(pbpaste)
data=configparser.ConfigParser()
data.read_string(cred)

s = data.sections()[0]
match s:
  case '391714386929_AdministratorAccess':
    section = 'demo-spoofcorp'
  case '411064808315_AdministratorAccess':
    section = 'dymium'
  case '564835066653_AdministratorAccess':
    section = 'dymium-dev'
  case '482973908181_AdministratorAccess':
    section = 'dymium-prod'
  case '655193105197_AdministratorAccess':
    section = 'dymium-sandbox'
  case '626593984035_AdministratorAccess':
    section = 'dymium-stage'
  case '570319887642_AdministratorAccess':
    section = 'test-org'
  case _:
    print(f'Unknown subaccount {data.sections()[0]}')
    exit(-1)

config[section] = { 'aws_access_key_id': data[s]['aws_access_key_id'],
                    'aws_secret_access_key': data[s]['aws_secret_access_key'] }
try:
    config[section]['aws_session_token'] = data[s]['aws_session_token']
except:
    pass

with open(cred_file,'w') as fp:
    config.write(fp)
