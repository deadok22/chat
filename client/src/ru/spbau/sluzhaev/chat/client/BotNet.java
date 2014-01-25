package ru.spbau.sluzhaev.chat.client;

import java.net.InetAddress;
import java.net.UnknownHostException;

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
//            Random rnd = new Random();
            final int SOCKETS_ON_POOL = 5;
            int bots = count / SOCKETS_ON_POOL;
            for (int i = 0; i < bots; ++i) {
                ClientBotPool client = new ClientBotPool(address, port, SOCKETS_ON_POOL);
                new Thread(client).start();
                Thread.sleep(50l);
            }
            ClientBotPool client = new ClientBotPool(address, port, count % SOCKETS_ON_POOL);
            new Thread(client).start();
        } catch (UnknownHostException | NumberFormatException e) {
            usage();
        } catch (InterruptedException e) {
        }
    }
}