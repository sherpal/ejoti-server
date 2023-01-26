
import * as http from "node:http"

// Create an HTTP server
const server = http.createServer((req, res) => {
  console.log(req)
  res.writeHead(200, { 'Content-Type': 'text/plain' });
  res.end('okay');
});

// Now that server is running
server.listen(3000, '127.0.0.1', () => {
  console.log("on")
});
