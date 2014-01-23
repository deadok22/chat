package ru.spbau.sluzhaev.chat.client.network;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;

public class ChatClient implements Runnable {
    private final int PROTOCOL_VERSION = 1;
    private Socket socket;
    private LoginResponseListener loginResponseListener;
    private LoginErrorListener loginErrorListener;
    private MessageListListener messageListListener;
    private UserListListener userListListener;

    public ChatClient(InetAddress address, int port) throws IOException {
        socket = new Socket(address, port);
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

    public void setUserListListener(UserListListener userListListener) {
        this.userListListener = userListListener;
    }

    @Override
    public void run() {
        try (BufferedInputStream inputStream = new BufferedInputStream(socket.getInputStream())) {
            while (true) {
                int length = BytesUtils.readInt(inputStream);
                byte currentProtocolVersion = (byte) inputStream.read();
                if (currentProtocolVersion != PROTOCOL_VERSION) {
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
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    enum Code {
        LOGIN(0), FETCH(1), SEND(2), USER_LIST(3), LOGOUT(4), LOGIN_RESPONSE(5),
        LOGIN_ERROR(6), MESSAGE_LIST(7), USER_LIST_RESPONSE(8);

        private final byte value;

        private Code(int value) {
            this.value = (byte) value;
        }

        static Code fromInt(int value) {
            for (Code code : Code.values()) {
                if (code.value == value) {
                    return code;
                }
            }

            return null;
        }


        public byte getValue() {
            return value;
        }
    }
}
