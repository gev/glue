const WebSocket = require('ws');

const wss = new WebSocket.Server({ port: 8080 });

wss.on('connection', ws => {
    // console.log('Client connected'); // Комментируем для уменьшения вывода при нагрузке

    ws.on('message', message => {
        // console.log(`Received: ${message}`); // Комментируем для уменьшения вывода
        ws.send(message); // Эхо-ответ
    });

    ws.on('close', () => {
        // console.log('Client disconnected');
    });

    ws.on('error', error => {
        console.error('WebSocket error:', error);
    });
});

console.log('WebSocket server is running on ws://localhost:8080');
