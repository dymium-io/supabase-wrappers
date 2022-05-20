const { createProxyMiddleware } = require('http-proxy-middleware');
module.exports = function(app) {
  app.use(['/api/', '/auth/'],
    createProxyMiddleware({
      target: 'http://portal.dymium.local',
      changeOrigin: true,
    })
  );
};
console.log("in proxy")
function onError(err, req, res, target) {
    res.writeHead(500, {
      'Content-Type': 'text/plain',
    });
    res.end('Something went wrong. And we are reporting a custom error message.');
  }


  function onProxyReq(proxyReq, req, res) {
    // add custom header to request
    console.log("in proxy request")
    // or log the req
  }