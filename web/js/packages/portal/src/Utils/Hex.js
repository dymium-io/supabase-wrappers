// hex encoder and (later) encoder


export function HexStringToByteArray(hexString) {
  var result = [];
  for (var i = 0; i < hexString.length; i += 2) {
    let h = hexString.charAt(i) +  hexString.charAt(i + 1)
    result.push(parseInt(h, 16));
  }
  return result;
}

export function StringFromUTF8Array(data) {
  const extraByteMap = [ 1, 1, 1, 1, 2, 2, 3, 0 ];
  var count = data.length;
  var str = "";
  
  for (var index = 0;index < count;)
  {
    var ch = data[index++];
    if (ch & 0x80)
    {
      var extra = extraByteMap[(ch >> 3) & 0x07];
      if (!(ch & 0x40) || !extra || ((index + extra) > count))
        return "";
      
      ch = ch & (0x3F >> extra);
      for (;extra > 0;extra -= 1)
      {
        var chx = data[index++];
        if ((chx & 0xC0) != 0x80)
          return "";
        
        ch = (ch << 6) | (chx & 0x3F);
      }
    }
    
    str += String.fromCharCode(ch);
  }
  
  return str;
}