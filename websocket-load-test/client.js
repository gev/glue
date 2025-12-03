const WebSocket = require('ws');

const numConnections = 13000;
const serverAddress = 'ws://localhost:8080';
let connections = 0;
let messagesReceived = 0;

console.log(`Attempting to open ${numConnections} connections...`);

wss = []

for (let i = 0; i < numConnections; i++) {
    const ws = new WebSocket(serverAddress);

    ws.on('open', () => {
        connections++;
        if (connections === numConnections) {
            console.log(`Successfully opened ${numConnections} connections.`);
            // Начинаем отправку сообщений после установки всех соединений
            setInterval(sendMessages, 1000);
            setInterval(() => {
                for (const ws of wss) {
                    ws.send(`Hello from client ${i}`);
                }
            }, 0);

        }
        wss.push(ws);
    });

    ws.on('message', message => {
        messagesReceived++;
        // Просто обрабатываем эхо-ответ
    });

    ws.on('close', () => {
        // console.log('A connection closed');
    });

    ws.on('error', error => {
        console.error(`Connection error for client ${i}:`, error.message);
    });
}

function sendMessages() {
    // Отправка сообщений для поддержания активности и тестирования эхо
    // В этом простом примере мы не храним ссылки на ws, 
    // в реальном нагрузочном тесте их нужно хранить в массиве.
    // Вместо этого можно просто логировать количество принятых сообщений.
    console.log(`Total messages received in the last second: ${messagesReceived}. Total open connections: ${connections}`);
    messagesReceived = 0;
}
