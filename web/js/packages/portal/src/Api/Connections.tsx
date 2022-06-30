import Alert from 'react-bootstrap/Alert'
import * as types from '@dymium/common/Types/Common'
import * as internal from '@dymium/common/Types/Internal'
import * as com from '../Common'

export function getConnections(setSpinner, setConns, setAlert, remap:internal.ConnectionMap|undefined, onSuccess) {
    setSpinner(true)
    setConns([])
    com.sendToServer("GET", "/api/getconnections",
        null, "",
        resp => {

            resp.json().then(_js => {
                let js = types.ConnectionResponse.fromJson(_js)
                if (js.status !== "OK") {
                    setAlert(
                        <Alert variant="danger" onClose={() => setAlert(<></>)} dismissible>
                            Error retrieving connections: {js.errormessage} { }
                        </Alert>
                    )
                    setTimeout(() => setSpinner(false), 500)
                    return
                }
            
                let cc = js.data.map(x => {
                    let ob = types.ConnectionRecord.fromJson({
                        id: x.id,
                        credid: x.credid,
                        dbtype: x.dbtype,
                        name: x.name,
                        dbname: x.dbname,
                        address: x.address,
                        port: x.port,
                        description: x.description,
                        useTLS: x.useTLS,

                    })
                    if(undefined != remap && x.name != null)
                        remap[x.name] = ob
                    return ob
                })

                setConns(cc)
                if(onSuccess != undefined) {
                    onSuccess()
                }
            })

            setTimeout(() => setSpinner(false), 500)
        },
        resp => {
            console.log("on error")
            setSpinner(false)
        },
        error => {
            console.log("on exception: " + error)
            setSpinner(false)
        })
}