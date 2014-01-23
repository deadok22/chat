package ru.spbau.sluzhaev.chat.client.network;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.Socket;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

public class ConnectionThread implements Runnable {
    private Socket socket;
    private BlockingQueue<Task> tasksQueue = new LinkedBlockingQueue<>();
    private LoginResponseListener loginResponseListener;
    private LoginErrorListener loginErrorListener;
    private MessageListListener messageListListener;

    public ConnectionThread(Socket socket) {
        this.socket = socket;
    }

    public void sendPackage(Package p) {
        tasksQueue.add(new Task(p));
    }

    public void setLoginResponseListener(LoginResponseListener loginResponseListener) {
        this.loginResponseListener = loginResponseListener;
    }

    public void setLoginErrorListener(LoginErrorListener loginErrorListener) {
        this.loginErrorListener = loginErrorListener;
    }

    public void setMessageListListener(MessageListListener messageListListener) {
        this.messageListListener = messageListListener;
    }

    @Override
    public void run() {
        final int timeout = 1000;
        try (BufferedInputStream inputStream = new BufferedInputStream(socket.getInputStream())) {
            log("Connected...");
            while (true) {
                Task task = tasksQueue.poll(timeout, TimeUnit.MILLISECONDS);
                if (task != null) {
                    log("Sending package...");
                    socket.getOutputStream().write(task.getPackage().getBytes());
                    log("Receiving package...");
                    process(inputStream);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        } catch (InterruptedException e) {
        }
    }

    private void process(InputStream inputStream) throws IOException {
        int length = BytesUtils.readInt(inputStream);
        byte currentProtocolVersion = (byte) inputStream.read();
        if (currentProtocolVersion != ChatClient.PROTOCOL_VERSION) {
            throw new UnsupportedOperationException();
        }
        Code code = Code.fromInt(inputStream.read());
        byte flags = (byte) inputStream.read();
        length -= 3;
        switch (code) {
            case LOGIN_RESPONSE:
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        if (loginResponseListener != null) {
                            loginResponseListener.event();
                        }
                    }
                }).start();
                break;
            case LOGIN_ERROR:
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        if (loginErrorListener != null) {
                            loginErrorListener.event();
                        }
                    }
                }).start();
                break;
            case MESSAGE_LIST:
                final Message[] messages = Message.readMessageArray(inputStream);
                new Thread(new Runnable() {
                    @Override
                    public void run() {
                        if (messageListListener != null) {
                            messageListListener.event(messages);
                        }
                    }
                }).start();
                break;
            case USER_LIST_RESPONSE:
                // ToDo
                        /*final String[] users = new String[0];
                        new Thread(new Runnable() {
                            @Override
                            public void run() {
                                if (userListListener != null) {
                                    userListListener.event(users);
                                }
                            }
                   });*/
                break;
            default:
                throw new UnsupportedOperationException();
        }
    }

    public class Task {
        private Package p;

        public Task(Package p) {
            this.p = p;
        }

        public Package getPackage() {
            return p;
        }

    }

    public void log(String text) {
        System.out.println("[ConnectionThread]: " + text);
    }
}
