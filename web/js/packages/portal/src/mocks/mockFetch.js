

export default async function mockFetch(url, js) {
    console.log("in mock fetch: ")
    switch (url) {
        case "/api/getconnections": {
            console.log(url)
            return  new Promise( (resolve, reject) => resolve(
                {
                    ok: true,
                    status: 200,
                    json:  () => new Promise( (resolve, reject) => resolve(JSON.parse('{"status":"OK","errormessage":"","data":[{"name":"adventureworks","dbtype":"PostgreSQL","address":"docker.for.mac.host.internal","port":5432,"dbname":"Adventureworks","useTLS":false,"description":"edited test data base from Microsoft","username":"postgres","password":null,"id":"a2a4fce3-1db5-47ea-a084-4601f880b9ba","credid":"2bd1e042-f5d1-4bcd-8563-7058f110c546"},{"name":"northwind","dbtype":"PostgreSQL","address":"docker.for.mac.host.internal","port":5432,"dbname":"northwind","useTLS":false,"description":"another MS database edited with password","username":"newusername","password":null,"id":"b157fc33-64ad-4173-a54b-b652ef21e0b7","credid":"876e712c-34ba-4bf0-a315-f3784b204512"}]}') ))
                }
              )
            )
        }

        default: {
            throw new Error(`Unhandled request: ${url}`);
        }
    }
}

