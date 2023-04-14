import {queryconnection, querypolicy, querytable} from './queryconnection'

export default async function mockFetch(url, js) {
    console.log("in mock fetch: ", url)
    switch (url) {
        case "/api/getconnections": {
            return  new Promise( (resolve, reject) => resolve(
                {
                    ok: true,
                    status: 200,
                    json:  () => new Promise( (resolve, reject) => resolve(JSON.parse('{"status":"OK","errormessage":"","data":[{"name":"adventureworks","dbtype":"PostgreSQL","address":"docker.for.mac.host.internal","port":5432,"dbname":"Adventureworks","useTLS":false,"description":"edited test data base from Microsoft","username":"postgres","password":null,"id":"a2a4fce3-1db5-47ea-a084-4601f880b9ba","credid":"2bd1e042-f5d1-4bcd-8563-7058f110c546"},{"name":"northwind","dbtype":"PostgreSQL","address":"docker.for.mac.host.internal","port":5432,"dbname":"northwind","useTLS":false,"description":"another MS database edited with password","username":"newusername","password":null,"id":"b157fc33-64ad-4173-a54b-b652ef21e0b7","credid":"876e712c-34ba-4bf0-a315-f3784b204512"}]}') ))
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
                    body: '{"ConnectionId":"a2a4fce3-1db5-47ea-a084-4601f880b9ba"}'
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
                    body: '{"ConnectionId":"a2a4fce3-1db5-47ea-a084-4601f880b9ba","Schema":"humanresources","Table":"shift"}'
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

