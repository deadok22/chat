package ru.spbau.sluzhaev.chat.client.network;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

public class ConnectionPool implements Runnable {
    private Socket[] sockets;
    private BlockingQueue<Task> tasksQueue = new LinkedBlockingQueue<>();

    public ConnectionPool(Socket[] sockets) {
        this.sockets = sockets;
    }

    public void sendPackage(Package p, int socketIndex) {
        log("send package");
        tasksQueue.add(new Task(p, socketIndex));
    }

    @Override
    public void run() {
        final int timeout = 1000;

        BufferedInputStream[] streams = new BufferedInputStream[sockets.length];
        try {
            for (int i = 0; i < streams.length; ++i) {
                streams[i] = new BufferedInputStream(sockets[i].getInputStream());
            }
            while (true) {
                Task task = tasksQueue.poll(timeout, TimeUnit.MILLISECONDS);
                if (task != null) {
                    log("Sending package...");
                    int socketIndex = task.getSocketIndex();
                    sockets[socketIndex].getOutputStream().write(task.getPackage().getBytes());
                    log("Receiving package...");
                    process(streams[socketIndex]);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
            log("Disconnect");
        } finally {
            for (int i = 0; i < streams.length; ++i)
                if (streams[i] != null) {
                    try {
                        streams[i].close();
                    } catch (IOException e) {
                    }
                }
        }
    }

    private void process(InputStream inputStream) throws IOException {
        int length = BytesUtils.readInt(inputStream);
        log("Length = " + length);
        byte currentProtocolVersion = BytesUtils.readBytes(inputStream, 1)[0];
        if (currentProtocolVersion != Package.PROTOCOL_VERSION) {
            throw new UnsupportedOperationException();
        }
        int c = BytesUtils.readBytes(inputStream, 1)[0];
        log("CODE = " + c);
        Code code = Code.fromInt(c);
        byte flags = BytesUtils.readBytes(inputStream, 1)[0];
        log("FLAGS = " + flags);
        int total_bytes = 3;
        switch (code) {
            case LOGIN_RESPONSE:
                final long lastMessageId = BytesUtils.readLong(inputStream);
                break;
            case LOGIN_ERROR:
                break;
            case MESSAGE_LIST:
                final Message[] messages = Message.readMessageArray(inputStream);
                break;
            case USER_LIST_RESPONSE:
                final String[] users = BytesUtils.readTextArray(inputStream);
                break;
            case MESSAGE_RESPONSE:
                long messageId = BytesUtils.readLong(inputStream);
                break;
            case LOGOUT_RESPONSE:
                break;
            default:
                throw new UnsupportedOperationException();
        }
    }

    public class Task {
        private Package p;
        private int socketIndex;

        public Task(Package p, int socketIndex) {
            this.p = p;
            this.socketIndex = socketIndex;
        }

        public Package getPackage() {
            return p;
        }

        public int getSocketIndex() {
            return socketIndex;
        }
    }

    public void log(String text) {
//        System.out.println("[ConnectionThread]: " + text);
    }
}

