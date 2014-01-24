package ru.spbau.sluzhaev.chat.client;

import ru.spbau.sluzhaev.chat.client.network.*;

import java.io.IOException;
import java.net.InetAddress;
import java.util.Date;
import java.util.Scanner;
import java.util.concurrent.atomic.AtomicLong;

public class Client implements Runnable {
    private InetAddress address;
    private int port;
    private ChatClient chatClient;
    private String name;
    private AtomicLong lastMessageId = new AtomicLong();

    public Client(InetAddress address, int port) {
        this.address = address;
        this.port = port;
    }

    @Override
    public void run() {
        try {
            this.chatClient = new ChatClient(address, port);
        } catch (IOException e) {
            e.printStackTrace();
            return;
        }
        new Thread(this.chatClient).start();
        System.out.print("login: ");
        Scanner scanner = new Scanner(System.in);
        name = scanner.nextLine();
        System.out.println("Name = " + name);
        chatClient.login(name);
        chatClient.setLoginResponseListener(new LoginResponseListener() {
            @Override
            public void event(long lastMessageId) {
                Client.this.lastMessageId.set(lastMessageId);
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
                        System.out.println("[" + date.toString() + "] " + message.getAuthor() + ": " + message.getText());
                    }
//                    messages.add(message);
                    lastMessageId.set(message.getId());
                }
            }
        });
        chatClient.setUserListListener(new UserListListener() {
            @Override
            public void event(String[] list) {
                System.out.println("Users online:");
                for (String user : list) {
                    System.out.println(user);
                }
            }
        });
        new Thread(new Fetcher()).start();
        while (true) {
            String text = scanner.nextLine();
            if (text.equals("\\logout")) {
                chatClient.logout();
                break;
            } else if (text.equals("\\userlist")) {
                chatClient.userList();
            } else {
                System.out.println("[" + (new Date()).toString() + "] " + name + ": " + text);
                chatClient.send(text);
            }
        }
    }

    public class Fetcher implements Runnable {
        @Override
        public void run() {
            try {
                while (true) {
                    /*long lastMessageId = 0;
                    synchronized (messages) {
                        if (!messages.isEmpty()) {
                            lastMessageId = messages.get(messages.size() - 1).getId();
                            System.out.println("LastMessageId = " +  messages.get(messages.size() - 1).getId());
                            System.out.println("LastMessageTimestamp = " +  messages.get(messages.size() - 1).getTimestamp());
                        }
                    }*/
                    chatClient.fetch(lastMessageId.get());
                    Thread.sleep(5000);
                }
            } catch (InterruptedException e) {

            }
        }
    }
}
