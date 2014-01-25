package ru.spbau.sluzhaev.chat.client;

import ru.spbau.sluzhaev.chat.client.network.ChatClientPool;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

public class ClientBotPool implements Runnable {
    private final InetAddress inetAddress;
    private final int port;
    private final int count;
    private ChatClientPool chatClients;
    private AtomicLong[] lastMessageId;

    public ClientBotPool(InetAddress inetAddress, int port, int count) {
        this.inetAddress = inetAddress;
        this.port = port;
        this.count = count;
    }

    @Override
    public void run() {
        lastMessageId = new AtomicLong[count];
        for (int i = 0; i < count; ++i) {
            lastMessageId[i] = new AtomicLong();
        }
        try {
            chatClients = new ChatClientPool(inetAddress, port, count);
        } catch (IOException e) {
            System.err.println("Cannot connect to " + inetAddress + ":" + port);
            e.printStackTrace();
            return;
        }
        chatClients.run();
        for (int i = 0; i < count; ++i) {
            chatClients.login(randomString(10), i);
        }
        long counter = 0;
        while (true) {
            try {
                Thread.sleep(500l);
                for (int i = 0; i < count; ++i) {
                    chatClients.fetch(0, i);
                }
                if (counter % 10 == 0) {
                    for (int i = 0; i < count; ++i) {
                        String text = randomString(20);
                        chatClients.send(text, i);
                    }
                }
                ++counter;
//                System.out.println("[" + (new Date()).toString() + "] " + name + ": " + text);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    public static void usage() {
        System.err.println("Usage: client_bot_pool <address> <port> <count>");
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
            ClientBotPool client = new ClientBotPool(address, port, count);
            new Thread(client).start();
        } catch (UnknownHostException | NumberFormatException e) {
            usage();
        }
    }

    public static String randomString(int length) {
        StringBuilder builder = new StringBuilder();
        Random rnd = new Random();
        for (int i = 0; i < length; ++i) {
            builder.append((char) ('a' + rnd.nextInt(26)));
        }
        return builder.toString();
    }
}
