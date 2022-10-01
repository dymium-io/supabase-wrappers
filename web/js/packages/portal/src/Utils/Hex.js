// hex encoder and (later) encoder


export function HexStringToByteArray(hexString) {
  var result = [];
  for (var i = 0; i < hexString.length; i += 2) {
    let h = hexString.charAt(i) +  hexString.charAt(i + 1)
    result.push(parseInt(h, 16));
  }
  return result;
}