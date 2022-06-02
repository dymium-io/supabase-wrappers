let appliedColors = `
:root{
--primary-color: rgb(255, 158, 24);
--primary-color-text: white;
--primary-color-2: rgb(208, 125, 103); 
--primary-color-2-text: #eee;
--primary-color-3: rgb(220, 158, 140); 
--primary-color-3-text: black;
--primary-pale: rgb(245, 227, 223); 
--primary-pale-text: #222;
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