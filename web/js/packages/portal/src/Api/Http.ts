
export function sendToServer(method: string, url: string,
    headers, body: string, onsuccess, onfailure, onexception) {
    let token = window.sessionStorage.getItem("Session");

    if (token === null) {
        onfailure(null)
        return;
    }
    let _headers = {
        Authorization: "Bearer " + token,
        Cache: "no-cache",
    }
    if (headers != null) {
        _headers = {
            ..._headers,
            ...headers
        }
    }
    let params: any = {
        method,
        headers: _headers
    }
    if (body != "") {
        params = { ...params, body: body }
    }
  

    fetch(url, params
    ).then(response => {

        if (!response.ok) {
            onfailure(response)
        } else {
            onsuccess(response)
        }
    }).catch(onexception)
}
