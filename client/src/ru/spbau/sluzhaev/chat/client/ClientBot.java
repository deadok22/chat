package ru.spbau.sluzhaev.chat.client;

import ru.spbau.sluzhaev.chat.client.network.*;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Date;
import java.util.Random;
import java.util.concurrent.atomic.AtomicLong;

public class ClientBot implements Runnable {
    private final InetAddress inetAddress;
    private final int port;
    private ChatClient chatClient;
    private AtomicLong lastMessageId = new AtomicLong();

    public ClientBot(InetAddress inetAddress, int port) {
        this.inetAddress = inetAddress;
        this.port = port;
    }

    @Override
    public void run() {
        try {
            chatClient = new ChatClient(inetAddress, port);
        } catch (IOException e) {
            System.err.println("Cannot connect to " + inetAddress + ":" + port);
            e.printStackTrace();
            return;
        }
        final String name = randomString(5);

        chatClient.setLoginResponseListener(new LoginResponseListener() {
            @Override
            public void event(long lastMessageId) {
                ClientBot.this.lastMessageId.set(lastMessageId);
                chatClient.fetch(lastMessageId);
            }
        });
        chatClient.setLoginErrorListener(new LoginErrorListener() {
            @Override
            public void event() {
                System.err.println("Login error!");
            }
        });
        chatClient.setMessageListListener(new MessageListListener() {
            @Override
            public void event(Message[] list) {
                for (Message message : list) {
                    if (!message.getAuthor().equals(name)) {
                        Date date = new Date(message.getTimestamp() / 1000l);
//                        System.out.println("[" + date.toString() + "] " + message.getAuthor() + ": " + message.getText());
                    }
                    lastMessageId.set(message.getId());
                }
                try {
                    Thread.sleep(5000l);
                    chatClient.fetch(lastMessageId.get());
                } catch (InterruptedException e) {
                }
            }
        });
        chatClient.setUserListListener(new UserListListener() {
            @Override
            public void event(String[] list) {
//                System.out.println("Users online:");
//                for (String user : list) {
//                    System.out.println(user);
//                }
                try {
                    Thread.sleep(10000l);
                    chatClient.userList();
                } catch (InterruptedException e) {
                }
            }
        });
        chatClient.run();
        chatClient.login(name);
        chatClient.userList();
        while (true) {
            try {
                Thread.sleep(5000l);
                String text = randomString(20);
                chatClient.send(text);
//                System.out.println("[" + (new Date()).toString() + "] " + name + ": " + text);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }

    public static void usage() {
        System.err.println("Usage: client_bot <address> <port>");
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            usage();
            return;
        }
        try {
            InetAddress address = InetAddress.getByName(args[0]);
            int port = Integer.parseInt(args[1]);
            ClientBot client = new ClientBot(address, port);
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
