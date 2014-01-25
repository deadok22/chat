package ru.spbau.sluzhaev.chat.client.network;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.Socket;

public class ChatClientPool implements Runnable {
    private Socket[] sockets;
    private ConnectionPool connectionThread;

    public ChatClientPool(InetAddress address, int port, int count) throws IOException {
        sockets = new Socket[count];
        for (int i = 0; i < count; ++i) {
            sockets[i] = new Socket();
            sockets[i].connect(new InetSocketAddress(address, port), 300000);
        }
        connectionThread = new ConnectionPool(sockets);
    }

    public void login(String name, int socketIndex) {
        Package p = new Package(Code.LOGIN, (byte) 0, BytesUtils.textToBytes(name));
        connectionThread.sendPackage(p, socketIndex);
    }

    public void fetch(long messageId, int socketIndex) {
        Package p = new Package(Code.FETCH, (byte) 0, BytesUtils.longToBytes(messageId));
        connectionThread.sendPackage(p, socketIndex);
    }

    public void send(String text, int socketIndex) {
        Package p = new Package(Code.SEND, (byte) 0, BytesUtils.textToBytes(text));
        connectionThread.sendPackage(p, socketIndex);
    }

    public void userList(int socketIndex) {
        Package p = new Package(Code.USER_LIST, (byte) 0, new byte[0]);
        connectionThread.sendPackage(p, socketIndex);
    }

    public void logout(int socketIndex) {
        Package p = new Package(Code.LOGOUT, (byte) 0, new byte[0]);
        connectionThread.sendPackage(p, socketIndex);
    }

    @Override
    public void run() {
        new Thread(connectionThread).start();
    }

}
