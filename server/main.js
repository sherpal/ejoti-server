import * as http from "node:http";

// Create an HTTP server
const server = http.createServer((req, res) => {
  console.log("in the handler")
  res.writeHead(200, { 'Content-Type': 'text/plain' });
  res.end('okay');
});
server.on('upgrade', (req, socket, head) => {
  //console.log(req)
  console.log("hi there")
  socket.write('HTTP/1.1 101 Web Socket Protocol Handshake\r\n' +
    'Upgrade: WebSocket\r\n' +
    'Connection: Upgrade\r\n' +
    '\r\n');
  console.log("I wrote")

  //socket.pipe(socket); // echo back
  socket.on("data", (chunk) => {
    console.log("chunk", chunk)
    console.log("hello", chunk.toString())
    socket.write("hi there")
  })
});

// Now that server is running
server.listen(1337, 'localhost', () => {

  // make a request
  const options = {
    port: 1337,
    host: 'localhost',
    headers: {
      'Connection': 'Upgrade',
      'Upgrade': 'websocket',
    },
  };

  const req = http.request(options);
  req.end();

  req.on('upgrade', (res, socket, upgradeHead) => {
    console.log("the upgrade head was", upgradeHead.toString())
    console.log('got upgraded!');
    socket.write("hello les amis")
    socket.on("data", (chunk) => {
      console.log("other chunk", chunk.buffer)
      socket.end()
      //process.exit(0)
    })
    //socket.end();
    //process.exit(0);
  });
});