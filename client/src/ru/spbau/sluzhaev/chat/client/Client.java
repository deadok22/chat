package ru.spbau.sluzhaev.chat.client;

import ru.spbau.sluzhaev.chat.client.network.*;

import java.io.IOException;
import java.net.InetAddress;
import java.util.*;
import java.util.concurrent.atomic.AtomicLong;

public class Client implements Runnable {
    private InetAddress address;
    private int port;
    private ChatClient chatClient;
    private String name;
//    private List<Message> messages = Collections.synchronizedList(new ArrayList<Message>());
    private AtomicLong lastMessageId = new AtomicLong();

    public Client(InetAddress address, int port) throws IOException {
        this.address = address;
        this.port = port;
        this.chatClient = new ChatClient(address, port);
        new Thread(this.chatClient).start();
    }

    @Override
    public void run() {
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
        new Thread(new Fetcher()).start();
        while (true) {
            String cmd = scanner.nextLine();
//            System.out.println("CMD = " + cmd);
            if (cmd.equals("send")) {
                String text = scanner.nextLine();
                System.out.println("Message: " + text);
                chatClient.send(text);
            }
            if (cmd.equals("logout")) {
                chatClient.logout();
                break;
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
