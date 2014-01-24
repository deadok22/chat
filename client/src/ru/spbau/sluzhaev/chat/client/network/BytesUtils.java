package ru.spbau.sluzhaev.chat.client.network;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

public abstract class BytesUtils {
    public static int bytesToInt(byte[] bytes) {
        int res = 0;
        for (int i = 0; i < 4; ++i) {
            res = (res << 8) + (((int)bytes[i]) & 0xFF);
        }
        return res;
    }

    public static long bytesToLong(byte[] bytes) {
        long res = 0;
        for (int i = 0; i < 8; ++i) {
            res = (res << 8) | (((long)bytes[i]) & 0xFF);
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
//        for (int i = 0; i < 8; ++i) {
//            System.out.print(buffer[i] + ",");
//        }
//        System.out.println();
        return bytesToLong(buffer);
    }

    public static String readText(InputStream inputStream) throws IOException {
        int length = readInt(inputStream);
        byte[] buffer = new byte[length];
        inputStream.read(buffer);
        return new String(buffer);
    }

    public static byte[] intToBytes(int value) {
        byte[] bytes = new byte[4];
        for (int i = 3; i >= 0; --i) {
            bytes[i] = (byte) ((value) & 0xFF);
            value >>= 8;
        }
        return bytes;
    }

    public static byte[] longToBytes(long value) {
        byte[] bytes = new byte[8];
        for (int i = 7; i >= 0; --i) {
            bytes[i] = (byte) ((value) & 0xFF);
            value >>= 8;
        }
        return bytes;
    }

    public static byte[] textToBytes(String text) {
        byte[] bytes = new byte[4 + text.length()];
        byte[] length = intToBytes(text.length());
        byte[] data = text.getBytes();
        for (int i = 0; i < 4; ++i) {
            bytes[i] = length[i];
        }
        for (int i = 0; i < data.length; ++i) {
            bytes[4 + i] = data[i];
        }
        return bytes;
    }

    public static void writeInt(OutputStream outputStream, int value) throws IOException {
        outputStream.write(intToBytes(value));
    }

    public static void writeLong(OutputStream outputStream, long value) throws IOException {
        outputStream.write(longToBytes(value));
    }

    public static void writeText(OutputStream outputStream, String text) throws IOException {
        writeInt(outputStream, text.length());
        outputStream.write(text.getBytes());
    }

    public static String[] readTextArray(InputStream inputStream) throws IOException {
        int size = readInt(inputStream);
        String[] result = new String[size];
        for (int i = 0; i < size; ++i) {
            result[i] = readText(inputStream);
        }
        return result;
    }
}
