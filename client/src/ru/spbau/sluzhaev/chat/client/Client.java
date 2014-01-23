package ru.spbau.sluzhaev.chat.client;

import ru.spbau.sluzhaev.chat.client.network.ChatClient;
import ru.spbau.sluzhaev.chat.client.network.Message;
import ru.spbau.sluzhaev.chat.client.network.MessageListListener;

import java.io.IOException;
import java.net.InetAddress;
import java.util.*;

public class Client implements Runnable {
    private InetAddress address;
    private int port;
    private ChatClient chatClient;
    private String name;
    private List<Message> messages = Collections.synchronizedList(new ArrayList<Message>());

    public Client(InetAddress address, int port) throws IOException {
        this.address = address;
        this.port = port;
        this.chatClient = new ChatClient(address, port);
    }

    @Override
    public void run() {
        Scanner scanner = new Scanner(System.in);
        System.out.print("login: ");
        name = scanner.next();
        chatClient.login(name);
        chatClient.setMessageListListener(new MessageListListener() {
            @Override
            public void event(Message[] list) {
                for (Message message : list) {
                    if (message.getAuthor().equals(name)) {
                        continue;
                    }
                    Date date = new Date(message.getTimestamp());
                    System.out.println("[" + date.toString() + "] " + message.getAuthor() + ": " + message.getText());
                    messages.add(message);
                }
            }
        });
        new Thread(new Fetcher()).start();
        while (true) {
            String cmd = scanner.next();
            if (cmd.equals("send")) {
                String text = scanner.nextLine();
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
                synchronized (messages) {
                    chatClient.fetch(messages.get(messages.size() - 1).getId());
                }
                Thread.sleep(1000);
            } catch (InterruptedException e) {

            }
        }
    }
}
