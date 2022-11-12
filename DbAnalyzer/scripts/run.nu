#!/usr/bin/env nu

let cnf = open "~/.config/dymium/local-cnf.yaml"

# Run DbAnalyzer 
def main [
  --detach (-d) # Run in background
  --debug       # Show docker startup command
  --kill (-k)   # kill running docker
] {
  if $kill {
    do -i { docker stop $cnf.DbAnalyzer.address }
    exit
  }
  let args = ([ 
      run --rm
      --net dymium
      --name $cnf.DbAnalyzer.address
      -p $"($cnf.DbAnalyzer.ePort):($cnf.DbAnalyzer.iPort)"
      (if $detach { -d })
      db-analyzer
      /main
  ] | where $it != null)

  if $debug { ^echo docker $args }
  echo $"Starting ($cnf.DbAnalyzer.address):($cnf.DbAnalyzer.ePort)..."
  docker $args
}
