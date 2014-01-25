package ru.spbau.sluzhaev.chat.client;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;

public class Test {
    static public void main(String[] args) {
        long start = System.currentTimeMillis();
        for (int i = 0; i < 1000; ++i) {
            new Thread(new Runnable() {
                @Override
                public void run() {
                    try {
                        Socket socket = new Socket(InetAddress.getByName("192.168.0.101"), 55896);
//                        OutputStream outputStream = socket.getOutputStream();
//                        outputStream
                    } catch (IOException e) {
                        e.printStackTrace();
                    }

//                    System.out.println("Ok");
                }
            }).start();
        }
        System.out.println((System.currentTimeMillis() - start));
    }
}
