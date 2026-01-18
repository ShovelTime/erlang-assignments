package paradis.assignment4;

import java.io.*;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.nio.channels.ClosedByInterruptException;
import java.util.Collections;
import java.util.List;
import java.util.concurrent.*;


/**
 * Chat server which will echo any sent message to it to all connected clients.
 * Will run connected sockets on their separate unique threads.
 * Message received are guaranteed to be retransmitted in the same order in which they came in.
 */
public class ChatServer extends Thread {
    public final static int MAX_CLIENT_CONNECTIONS = 24;
    public final static InetSocketAddress SERVER_ADDRESS = new InetSocketAddress("127.0.0.1", 8000);

    // + 1 is for the message handler thread.
    private final ExecutorService threadPool = Executors.newFixedThreadPool(MAX_CLIENT_CONNECTIONS + 1);
    //Allows us to concurrently write and read form the clients list, with the caveat that add operations will be more expensive.
    private final CopyOnWriteArrayList<ConnectionHandler> clients = new CopyOnWriteArrayList<>();
    //The priority blocking queue allows us to put earlier messages in front of the queue, even if somehow a "later" message got added first.
    private final PriorityBlockingQueue<Message> messageQueue = new PriorityBlockingQueue<>(MAX_CLIENT_CONNECTIONS * 2);


    //private int connectedClients = 0;

    @Override
    public void run() {
        ServerSocket serverSocket;
        try {
            serverSocket = new ServerSocket();
            serverSocket.bind(SERVER_ADDRESS);
        } catch (IOException e) {
            e.printStackTrace();
            return;
        }

        MessageHandler messageHandler = new MessageHandler(messageQueue, Collections.unmodifiableList(clients));
        threadPool.execute(messageHandler);

        System.out.println("Server started at " + SERVER_ADDRESS.toString());

        while (true) {
            try {
                Socket incomingSocket = serverSocket.accept();
                System.out.println("Incoming socket received");
                threadPool.execute(() -> {
                    ConnectionHandler client;
                    try {
                        client = new ConnectionHandler(this, incomingSocket, messageQueue);
                    } catch (IOException e) {
                        e.printStackTrace();
                        return;
                    }
                    clients.add(client);
                    client.run();
                });

            } catch (ClosedByInterruptException e) {
                e.printStackTrace();
                break;
            } catch (IOException e) {
                e.printStackTrace();
            }
        }

        terminateClients();
        try { serverSocket.close(); } catch (IOException _) {}
        threadPool.shutdownNow();

    }

    private void terminateClients(){
        for (ConnectionHandler client : clients) {
            try {
                client.closeSocket();
            } catch (IOException e) {e.printStackTrace();}
        }
    }

    void removeClient(ConnectionHandler client) {
        if (client == null) return;
        clients.remove(client);
    }

    public static void main(String[] args) throws InterruptedException {
        ChatServer server = new ChatServer();
        server.start();
        server.join();
    }
}


/**
 * Thread dedicated to processing incoming messages
 * Message redirection to connected clients is done via a parallel stream over the clients List
 * Ensure that the client's list is thread safe, this class will not provide any synchronization guarantees over the iteration.
 */
class MessageHandler implements Runnable {
    private final PriorityBlockingQueue<Message> messageQueue;
    private final List<ConnectionHandler> clients;

    public MessageHandler(PriorityBlockingQueue<Message> messageQueue, List<ConnectionHandler> clients) {
        this.messageQueue = messageQueue;
        this.clients = clients;
    }
    public void run() {
            while (true) {
                try {
                    Message msg = messageQueue.take();
                    clients.parallelStream().forEach(client -> client.sendMessage(msg));
                } catch (InterruptedException _) {
                    break;
                }
            }

    }
}


/**
 * IO wrapper around an opened socket, will continuously read and submit any incoming messages from the socket to the queue.
 */
class ConnectionHandler implements Runnable {
    private final ChatServer server;
    private final Socket connectionSocket;
    private final String name;

    private final PrintWriter out;
    private final BufferedReader in;
    private final PriorityBlockingQueue<Message> messageQueue;



    ConnectionHandler(ChatServer server, Socket connectionSocket, PriorityBlockingQueue<Message> queue) throws IOException {
        this.server = server;
        this.connectionSocket = connectionSocket;
        this.messageQueue = queue;

        out = new PrintWriter(connectionSocket.getOutputStream(), true);
        in = new BufferedReader(new InputStreamReader(
                connectionSocket.getInputStream()
        ));

        out.println("Enter client name");
        this.name = in.readLine();
        out.println(name + " registered");

        System.out.println("Client " + name + " registered");
    }

    @Override
    public void run() {
        try {
            while (!connectionSocket.isClosed()) {
                String message = in.readLine();
                if(message == null) break; //end of stream reached, socket on the other side closed.

                long timestamp = System.nanoTime();
                Message msg = new Message(timestamp, name, message);
                System.out.println(msg);
                messageQueue.add(msg);
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            try {
                if (!connectionSocket.isClosed()) {
                    connectionSocket.close();
                    in.close();
                    out.close();
                }
            } catch (IOException e) {e.printStackTrace();}
            System.out.println("Disconnected from " + name);
            server.removeClient(this);
        }
    }

    public void sendMessage(Message msg) {
        if (connectionSocket.isClosed() || msg == null) return;
        out.println(msg);
    }

    public void closeSocket() throws IOException {
        connectionSocket.close();
    }
}


/**
 * Timestamped message received from an incoming socket.
 * <br>
 * Allows for better order guarantees when messages must be sent
 * in the proper order to other clients.
 *
 * @param timestamp
 * @param name
 * @param message
 */
record Message(long timestamp, String name, String message) implements Comparable<Message> {

    @Override
    public int compareTo(Message o) {
        if (this.timestamp == o.timestamp) {
            int nameComp = name.compareTo(o.name);
            if (nameComp == 0) return this.message.compareTo(o.message);
            else return nameComp;
        }
        return Long.compare(timestamp, o.timestamp);
    }

    @Override
    public String toString()
    {
        return name + ": " + message;
    }
}