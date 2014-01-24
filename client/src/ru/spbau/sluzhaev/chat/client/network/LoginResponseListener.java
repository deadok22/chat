package ru.spbau.sluzhaev.chat.client.network;

public interface LoginResponseListener {
    public void event(long lastMessageId);
}
