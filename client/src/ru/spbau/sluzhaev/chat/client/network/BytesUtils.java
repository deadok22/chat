package ru.spbau.sluzhaev.chat.client.network;

import java.io.IOException;
import java.io.InputStream;

public abstract class BytesUtils {
    public static int bytesToInt(byte[] bytes) {
        int res = 0;
        for (int i = 0; i < 4; ++i) {
            res = (res << 8) + bytes[i];
        }
        return res;
    }

    public static long bytesToLong(byte[] bytes) {
        long res = 0;
        for (int i = 0; i < 8; ++i) {
            res = (res << 8) + bytes[i];
        }
        return res;
    }

    public static int readInt(InputStream inputStream) throws IOException {
        byte[] buffer = new byte[4];
        inputStream.read(buffer);
        return bytesToInt(buffer);
    }

    public static long readLong(InputStream inputStream) throws IOException {
        byte[] buffer = new byte[8];
        inputStream.read(buffer);
        return bytesToLong(buffer);
    }

    public static String readText(InputStream inputStream) throws IOException {
        int length = readInt(inputStream);
        byte[] buffer = new byte[length];
        inputStream.read(buffer);
        return new String(buffer);
    }
}
