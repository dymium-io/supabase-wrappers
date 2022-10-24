#!/usr/bin/env nu

let cnf = open "~/.config/dymium/local-cnf.yaml"

# Run guardian apps for a list of customers
def main [
  --detach (-d) # Run in background
  --debug       # Show docker startup command
  --kill (-k)   # Kill running docker
  --list (-l)   # List of configured guardians
  ...customers: string # List of customrs (all if empty)  
] {

  if $list {
     ($cnf.Customers | each { |x| echo $x.name })
     exit
  }

  let guardians = (
    if $customers == [] { $cnf.Customers }
    else ($cnf.Customers | where $it.name in $customers)
  )

  if $kill {
     for g in $guardians {
        do -i { docker stop $"($g).guardian.local" }
     }
     exit
  }

  if not $detach && ($guardians | length) != 1 {
     echo "Only one guardian may be started in the foreground mode!"
     echo "  (try to use '-d' flag)"
     echo
     help main
     exit
  }

  let lambdas = ({
    DbSync: $"($cnf.DbSync.address):($cnf.DbSync.iPort)"
  } | to json)

  for g in $guardians {
    let args = ([
      run --rm
      --net dymium
      --name $"($g.name).guardian.local"
      -p $"($g.port):5432"
      -e $"POSTGRES_PASSWORD=($g.postgres.password)"
      -e "DATABASE_USER=dymium"
      -e $"DATABASE_PASSWORD=($g.dymium.password)"
      -e $"CUSTOMER=($g.name)"
      -e $"AWS_LAMBDAS=($lambdas)"
      (if $detach { -d })
      data-guardian
    ] | where $it != null)
    if $debug { ^echo $args }
    echo $"Starting ($g.name).guardian.local..."
    docker $args
  }
}
