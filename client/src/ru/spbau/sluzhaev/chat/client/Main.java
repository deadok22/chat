package ru.spbau.sluzhaev.chat.client;

import ru.spbau.sluzhaev.chat.client.network.ChatClient;

import java.io.IOException;
import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Scanner;

public class Main {

    public static void usage() {
        System.err.println("Usage: client <address> <port>");
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            usage();
            return;
        }
        try {
            InetAddress address = InetAddress.getByName(args[0]);
            int port = Integer.parseInt(args[1]);
            ChatClient chatClient = new ChatClient(address, port);
            new Thread(chatClient).start();
            Scanner scanner = new Scanner(System.in);
            while (true) {
                // ToDo
                /*String cmd = scanner.next();
                if (cmd.equals("login")) {
                    String name = scanner.next();
                }
                if (cmd.equals("exit")) {
                    break;
                }*/
            }
        } catch (UnknownHostException | NumberFormatException e) {
            usage();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
