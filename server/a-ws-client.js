import * as http from "node:http";

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
        process.exit(0)
    })
    //socket.end();
    //process.exit(0);
});
