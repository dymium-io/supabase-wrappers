#!/usr/bin/env nu

let cnf = open "~/.config/dymium/local-cnf.yaml"

# Run DbSync 
def main [
  --detach (-d) # Run in background
  --debug       # Show docker startup command
  --kill (-k)   # Kill running docker
] {
  if $kill {
    do -i { docker stop $cnf.DbSync.address }
    exit
  }

  let guardianCnf = ($cnf.Customers
      | reduce -f {
	  DEFAULT: {
	      guardian_tls: false
	      guarduan_user: dymium
	      guardian_database: postgres
	  }
      } { |c, gc| $gc 
          | insert $c.name { guardian_address: [ $"($c.name).guardian.local" ]
			     guardian_port: $c.port
			     guardian_password: $c.dymium.password
			   }
      } | to json)

  let args = ([ 
      run --rm
      --net dymium
      --name $cnf.DbSync.address
      -p $"($cnf.DbSync.ePort):($cnf.DymiumDB.iPort)"
      -e $"DATABASE_HOST=($cnf.DymiumDB.address)"
      -e $"DATABASE_PORT=($cnf.DymiumDB.iPort)"
      -e "DATABASE_DB=dymium"
      -e "DATABASE_USER=dymium"
      -e $"DATABASE_PASSWORD=($cnf.DymiumDB.dymium.password)"
      -e $"DATABASE_TLS=($cnf.DymiumDB.tls)"
      -e $"GUARDIAN_CONF=($guardianCnf)"
      (if $detach { -d })
      db-sync
      /main
  ] | where $it != null)

  echo $"Starting ($cnf.DbSync.address):($cnf.DbSync.ePort)..."
  if $debug { ^echo docker $args }
  docker $args
}
