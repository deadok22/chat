package ru.spbau.sluzhaev.chat.client;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Date;

public class Main {
    public static void usage() {
        System.err.println("Usage: client <address> <port>");
    }

    public static void main(String[] args) {
//        System.out.println((new Date(System.currentTimeMillis()).toString()));
        if (args.length != 2) {
            usage();
            return;
        }
        try {
            InetAddress address = InetAddress.getByName(args[0]);
            int port = Integer.parseInt(args[1]);
            Client client = new Client(address, port);
            new Thread(client).start();
        } catch (UnknownHostException | NumberFormatException e) {
            usage();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
