package ru.spbau.sluzhaev.chat.client.network;

import java.io.IOException;
import java.io.InputStream;

public class Message {
    private long id;
    private long timestamp;
    private String author;
    private String text;

    public static Message[] readMessageArray(InputStream inputStream) throws IOException {
        int length = BytesUtils.readInt(inputStream);
        Message[] result = new Message[length];
        for (int i = 0; i < length; ++i) {
            result[i] = Message.readMessage(inputStream);
        }
        return result;
    }

    public static Message readMessage(InputStream inputStream) throws IOException {
        long id = BytesUtils.readLong(inputStream);
        long timestamp = BytesUtils.readLong(inputStream);
        String author = BytesUtils.readText(inputStream);
        String text = BytesUtils.readText(inputStream);
        return new Message(id, timestamp, author, text);
    }

    public Message(long id, long timestamp, String author, String text) {
        this.id = id;
        this.timestamp = timestamp;
        this.author = author;
        this.text = text;
    }

    public long getId() {
        return id;
    }

    public long getTimestamp() {
        return timestamp;
    }

    public String getAuthor() {
        return author;
    }

    public String getText() {
        return text;
    }
}