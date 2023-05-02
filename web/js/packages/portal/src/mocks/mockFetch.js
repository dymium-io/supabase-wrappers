import {queryconnection, querypolicy, querytable} from './queryconnection'

export default async function mockFetch(url, js) {
    console.log("in mock fetch: ", url)
    switch (url) {
        case "/api/getconnections": {
            return  new Promise( (resolve, reject) => resolve(
                {
                    ok: true,
                    status: 200,
                    json:  () => new Promise( (resolve, reject) => resolve(JSON.parse('{"status":"OK","errormessage":"","data":[{"name":"adventureworks","dbtype":"PostgreSQL","usesconnector":false,"connectorname":"","connectorid":"","tunnelname":"","tunnelid":"","address":"docker.for.mac.host.internal","port":5432,"dbname":"Adventureworks","useTLS":false,"description":"test data base from Microsoft","username":"postgres","password":null,"id":"ca7eb388-df2c-4b1f-a78e-35716061b63f","credid":"6d9e06fa-2660-44d6-b86a-83109b4812c3"},{"name":"northwind","dbtype":"PostgreSQL","usesconnector":false,"connectorname":"","connectorid":"","tunnelname":"","tunnelid":"","address":"docker.for.mac.host.internal","port":5432,"dbname":"northwind","useTLS":false,"description":"another MS database","username":"postgres","password":null,"id":"2e3f623c-64b7-428f-9186-9c4286936c8e","credid":"7e5b6636-8401-4579-a631-5ba6fa515861"},{"name":"aircrafts","dbtype":"PostgreSQL","usesconnector":false,"connectorname":"","connectorid":"","tunnelname":"","tunnelid":"","address":"docker.for.mac.host.internal","port":5432,"dbname":"aircrafts","useTLS":false,"description":"images","username":"dymium","password":null,"id":"64c59820-86fe-4ac0-b289-0170dce2519d","credid":"2f311469-db72-4f93-8bc4-468bdcaeff09"},{"name":"xxx","dbtype":"PostgreSQL","usesconnector":true,"connectorname":"Test","connectorid":"4a87fb02-f76c-4a0d-82b1-cfdcc23bdb6b","tunnelname":"test","tunnelid":"ba906750-0746-434a-acf0-d23c2d8f3fc6","address":"","port":0,"dbname":"adventureworks","useTLS":false,"description":"ccc","username":"dymium","password":null,"id":"45c9e82f-5d4b-41df-b798-b8e44fbcf680","credid":"9bff14c3-bccc-4c28-8ed5-3350fd22473b"},{"name":"aworks","dbtype":"PostgreSQL","usesconnector":false,"connectorname":"","connectorid":"","tunnelname":"","tunnelid":"","address":"docker.for.mac.host.internal","port":5432,"dbname":"Adventureworks","useTLS":false,"description":"MS test database, yes yes","username":"postgres","password":null,"id":"3f3f9866-77d2-41b4-8c7b-ef34c58fe97d","credid":"61e90024-2a15-402a-8991-d32de04c2a9b"},{"name":"111","dbtype":"PostgreSQL","usesconnector":false,"connectorname":"","connectorid":"","tunnelname":"","tunnelid":"","address":"docker.for.mac.host.internal","port":5432,"dbname":"Adventureworks","useTLS":false,"description":"xxx","username":"postgres","password":null,"id":"95da4e85-3e3e-46dc-8c1b-c8cc824f6d99","credid":"7d91f00c-6a14-40df-9834-b53cc3796883"},{"name":"sqlserver","dbtype":"SqlServer","usesconnector":false,"connectorname":"","connectorid":"","tunnelname":"","tunnelid":"","address":"host.docker.internal","port":1433,"dbname":"WideWorldImporters","useTLS":false,"description":"test sql server","username":"sa","password":null,"id":"80642caf-1319-41e7-9768-c06caab3ab45","credid":"650d9fcc-a96d-437b-9a7d-f78f8892182c"}]}') ))
                }
              )
            )
        }
        case "/api/getdatascopes" : {
            return  new Promise( (resolve, reject) => resolve(
                {
                    ok: true,
                    status: 200,
                    json:  () => new Promise( (resolve, reject) => resolve(JSON.parse('{"status":"OK","errormessage":"","records":[{"name":"test1","id":"50fe96d1-b1a4-48df-87bd-6aced2ab1f5a"},{"name":"test2","id":"b3fddf89-5822-4227-9948-3ecb3c8c2e33"}]}') ))
                }
              )
            )            
        }
        case "/api/getmappings" : {
            return  new Promise( (resolve, reject) => resolve(
                {
                    ok: true,
                    status: 200,
                    json:  () => new Promise( (resolve, reject) => resolve(JSON.parse('{"status":"OK","errormessage":"","records":[{"id":"788b3c71-5371-41e8-b135-8037bcfcfd9f","dymiumgroup":"foo","directorygroup":"bar","comments":"Admin group fubar"},{"id":"27713f32-6772-4509-9476-41099b11cb18","dymiumgroup":"dada","directorygroup":"production","comments":"Comment changed via test"}]}') ))
                }
              )
            )                     
        }
        
        case "/api/getgroupsfordatascopes" : {
            return  new Promise( (resolve, reject) => resolve(
                {
                    ok: true,
                    status: 200,
                    json:  () => new Promise( (resolve, reject) => resolve(JSON.parse('[{"id":"50fe96d1-b1a4-48df-87bd-6aced2ab1f5a","name":"test1","groupid":"788b3c71-5371-41e8-b135-8037bcfcfd9f","groupname":"foo"}]') ))
                }
              )
            )                     
        }        
        case "/api/updateconnection" : {
            expect(js).toEqual({
                method: 'POST',
                headers: { Authorization: 'Bearer mockJWT', Cache: 'no-cache' },
                body: '{"Id":"a2a4fce3-1db5-47ea-a084-4601f880b9ba","Name":"adventureworks","DbType":"PostgreSQL","Address":"docker.for.mac.host.internal","Port":5432,"Dbname":"Adventureworks","Description":"edited test data base from Microsoft"}'
              });
            return  new Promise( (resolve, reject) => resolve(
                {
                    ok: true,
                    status: 200,
                    json:  () => new Promise( (resolve, reject) => resolve(JSON.parse('{"status": "OK", "errormessage": "All is fine"}') ))
                }
              )
            )               
        }
        case "/api/deleteconnection": {
            expect(js).toEqual(
                {
                    method: 'POST',
                    headers: { Authorization: 'Bearer mockJWT', Cache: 'no-cache' },
                    body: '{"Id":"a2a4fce3-1db5-47ea-a084-4601f880b9ba"}'
                  }
            );
            return  new Promise( (resolve, reject) => resolve(
                {
                    ok: true,
                    status: 200,
                    json:  () => new Promise( (resolve, reject) => resolve(JSON.parse('{"status": "OK", "errormessage": ""}') ))
                }
              )
            )                
        }
        case "/api/queryconnection" : {
            expect(js).toEqual(
                {
                    method: 'POST',
                    headers: { Authorization: 'Bearer mockJWT', Cache: 'no-cache' },
                    body: '{"ConnectionId":"ca7eb388-df2c-4b1f-a78e-35716061b63f"}'
                  }
            );
            return  new Promise( (resolve, reject) => resolve(
                {
                    ok: true,
                    status: 200,
                    json:  () => new Promise( (resolve, reject) => resolve(JSON.parse(queryconnection) ))
                }
              )
            )    
            
        }
        case "/api/savedatascope" : {
            console.log(js)
            expect(js).toEqual(
                {
                    method: 'POST',
                    headers: { Authorization: 'Bearer mockJWT', Cache: 'no-cache' },
                    body: "{\"name\":\"testdb\",\"id\":null,\"records\":[{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"integer\",\"position\":1,\"reference\":null,\"action\":\"Obfuscate\",\"col\":\"businessentityid\",\"semantics\":\"Business entity Id\",\"dflt\":null,\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"varchar(15)\",\"position\":2,\"reference\":null,\"action\":\"Obfuscate\",\"col\":\"nationalidnumber\",\"semantics\":\"Social Security Number\",\"dflt\":null,\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"varchar(256)\",\"position\":3,\"reference\":null,\"action\":\"Block\",\"col\":\"loginid\",\"semantics\":\"Login details\",\"dflt\":null,\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"varchar(50)\",\"position\":6,\"reference\":null,\"action\":\"Obfuscate\",\"col\":\"jobtitle\",\"semantics\":\"Job position\",\"dflt\":null,\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"date\",\"position\":7,\"reference\":null,\"action\":\"Allow\",\"col\":\"birthdate\",\"semantics\":\"N/A\",\"dflt\":null,\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"character(1)\",\"position\":8,\"reference\":null,\"action\":\"Allow\",\"col\":\"maritalstatus\",\"semantics\":\"N/A\",\"dflt\":null,\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"character(1)\",\"position\":9,\"reference\":null,\"action\":\"Obfuscate\",\"col\":\"gender\",\"semantics\":\"Gender\",\"dflt\":null,\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"date\",\"position\":10,\"reference\":null,\"action\":\"Allow\",\"col\":\"hiredate\",\"semantics\":\"N/A\",\"dflt\":null,\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"boolean\",\"position\":11,\"reference\":null,\"action\":\"Allow\",\"col\":\"salariedflag\",\"semantics\":\"N/A\",\"dflt\":\"true\",\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"smallint\",\"position\":12,\"reference\":null,\"action\":\"Allow\",\"col\":\"vacationhours\",\"semantics\":\"N/A\",\"dflt\":\"0\",\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"smallint\",\"position\":13,\"reference\":null,\"action\":\"Allow\",\"col\":\"sickleavehours\",\"semantics\":\"N/A\",\"dflt\":\"0\",\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"boolean\",\"position\":14,\"reference\":null,\"action\":\"Allow\",\"col\":\"currentflag\",\"semantics\":\"N/A\",\"dflt\":\"true\",\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"uuid\",\"position\":15,\"reference\":null,\"action\":\"Allow\",\"col\":\"rowguid\",\"semantics\":\"N/A\",\"dflt\":\"uuid_generate_v1()\",\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"timestamp without time zone\",\"position\":16,\"reference\":null,\"action\":\"Allow\",\"col\":\"modifieddate\",\"semantics\":\"N/A\",\"dflt\":\"now()\",\"isnullable\":false},{\"id\":null,\"connection\":\"adventureworks\",\"connectionId\":null,\"schema\":\"humanresources\",\"table\":\"employee\",\"typ\":\"varchar\",\"position\":17,\"reference\":null,\"action\":\"Allow\",\"col\":\"organizationnode\",\"semantics\":\"N/A\",\"dflt\":\"'/'::character varying\",\"isnullable\":true}]}"
                  }
            );
            return  new Promise( (resolve, reject) => resolve(
                {
                    ok: true,
                    status: 200,
                    json:  () => new Promise( (resolve, reject) => resolve(JSON.parse(`{"status":"OK","errormessage":"Ghost Database created"}`) ))
                }
              )
            )    
            
        }        
        case "/api/querytable" : {
            console.log(js)
            expect(js).toEqual(
                {
                    method: 'POST',
                    headers: { Authorization: 'Bearer mockJWT', Cache: 'no-cache' },
                    body: '{"ConnectionId":"ca7eb388-df2c-4b1f-a78e-35716061b63f","Schema":"humanresources","Table":"shift"}'
                  }
            );
            return  new Promise( (resolve, reject) => resolve(
                {
                    ok: true,
                    status: 200,
                    json:  () => new Promise( (resolve, reject) => resolve(JSON.parse(querytable) ))
                }
              )
            )   
        }
        case "/api/getpolicies" : {
        
            return  new Promise( (resolve, reject) => resolve(
                {
                    ok: true,
                    status: 200,
                    json:  () => new Promise( (resolve, reject) => resolve(JSON.parse(querypolicy) ))
                }
              )
            )   
        }

        default: {
            throw new Error(`Unhandled request: ${url}`);
        }
    }
}

