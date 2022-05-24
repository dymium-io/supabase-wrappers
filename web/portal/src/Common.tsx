let appliedColors = `
:root{
--primary-color: rgb(255, 158, 24);
--primary-color-text: rgb(31, 31, 32);
--primary-color-2: rgb(119, 74, 10);
--primary-color-2-text: #eee;
--primary-color-3: rgb(220, 158, 140); 
--primary-color-3-text: black;
--primary-pale: rgb(183, 114, 17);
--primary-pale-text: #eeeeee;
--secondary-color: rgb(6, 33, 76); 
}
`
function insertStyleSheet(colors)  {

    var style = document.createElement('style');
    style.type = 'text/css';

    style.innerHTML = colors;

    document.getElementsByTagName('head')[0].appendChild(style);
}

let cssInserted = false
export function customizeStyleSheet() {
    if(cssInserted)
        return
    cssInserted = true
    insertStyleSheet(appliedColors)
}
export function getTokenProperty(prop) {
    let token = sessionStorage.getItem("Session")
    if(token === "" || token === null)    
        return undefined

    let sp = token.split('.')
    let claims = atob(sp[1])
    let j = JSON.parse(claims)
    return j[prop]
}

export const databaseTypes = {
    postgres: "PostgresSQL",
    mysql: "MySQL",
    mariadb: "MariaDB",
    sqlserver: "SQL Server",
    oracle: "Oracle DB",
}