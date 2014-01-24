package ru.spbau.sluzhaev.chat.client.network;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;

public class ChatClient implements Runnable {
    private Socket socket;
    private ConnectionThread connectionThread;

    public ChatClient(InetAddress address, int port) throws IOException {
        socket = new Socket(address, port);
    }

    public void login(String name) {
        Package p = new Package(Code.LOGIN, (byte)0, BytesUtils.textToBytes(name));
        connectionThread.sendPackage(p);
    }

    public void fetch(long messageId) {
        Package p = new Package(Code.FETCH, (byte)0, BytesUtils.longToBytes(messageId));
        connectionThread.sendPackage(p);
    }

    public void send(String text) {
        Package p = new Package(Code.SEND, (byte)0, BytesUtils.textToBytes(text));
        connectionThread.sendPackage(p);
    }

    public void userList() {
        Package p = new Package(Code.USER_LIST, (byte)0, new byte[0]);
        connectionThread.sendPackage(p);
    }

    public void logout() {
        Package p = new Package(Code.LOGOUT, (byte)0, new byte[0]);
        connectionThread.sendPackage(p);
    }

    @Override
    public void run() {
        connectionThread = new ConnectionThread(socket);
        new Thread(connectionThread).start();
    }

    public void setLoginResponseListener(LoginResponseListener loginResponseListener) {
        connectionThread.setLoginResponseListener(loginResponseListener);
    }

    public void setLoginErrorListener(LoginErrorListener loginErrorListener) {
        connectionThread.setLoginErrorListener(loginErrorListener);
    }

    public void setMessageListListener(MessageListListener messageListListener) {
        connectionThread.setMessageListListener(messageListListener);
    }

    public void setUserListListener(UserListListener userListListener) {
        connectionThread.setUserListListener(userListListener);
    }

    public void setLogoutResponseListener(LogoutResponseListener logoutResponseListener) {
        connectionThread.setLogoutResponseListener(logoutResponseListener);
    }
}
