#!/usr/bin/env nu

let cnf = open "~/.config/dymium/local-cnf.yaml"

def main [
  #db: string
  #user: string
  ...params: string
] {
  $params | echo
  # let d = ($cnf.Databases | get -i $db)
  # if $d == null {
  #   echo $"Database ($db) not defined"
  #   exit
  # }
  # let-env PGPASSWORD = ($d | get $user).password
  # psql -h localhost -p ($d.port) -U (($d | get $user).name) ($params)
}
