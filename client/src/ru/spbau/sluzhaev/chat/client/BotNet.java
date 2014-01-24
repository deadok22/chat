package ru.spbau.sluzhaev.chat.client;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Random;

public class BotNet {
    public static void usage() {
        System.out.println("Usage: botnet <address> <port> <count>");
    }

    public static void main(String[] args) {
        if (args.length != 3) {
            usage();
            return;
        }
        try {
            InetAddress address = InetAddress.getByName(args[0]);
            int port = Integer.parseInt(args[1]);
            int count = Integer.parseInt(args[2]);
            Random rnd = new Random();
            for (int i = 0; i < count; ++i) {
                ClientBot client = new ClientBot(address, port);
                Thread.sleep(rnd.nextInt(1000));
                new Thread(client).start();
            }
        } catch (UnknownHostException | NumberFormatException e) {
            usage();
        } catch (InterruptedException e) {

        }
    }
}