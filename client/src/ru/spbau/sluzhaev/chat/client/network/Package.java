package ru.spbau.sluzhaev.chat.client.network;

public class Package {
    public static final byte PROTOCOL_VERSION = 0;
    private final Code code;
    private byte flags;
    private byte[] data;

    public Package(Code code, byte flags, byte[] data) {
        this.code = code;
        this.flags = flags;
        this.data = data;
    }

    public Package(byte[] bytes) {
        if (bytes.length < 5) {
            throw new UnsupportedOperationException();
        }
        byte currentProtocolVersion = bytes[4];
        if (currentProtocolVersion != PROTOCOL_VERSION) {
            throw new UnsupportedOperationException();
        }
        int length = (bytes[0] << 24) + (bytes[1] << 16) + (bytes[2] << 8) + bytes[3];
        code = Code.fromInt(bytes[5]);
        flags = bytes[6];
    }

    public int getLength() {
        return 3 + data.length;
    }

    public Code getCode() {
        return code;
    }

    public byte[] getData() {
        return data;
    }

    public byte[] getBytes() {
        int length = getLength();
        byte[] result = new byte[4 + length];
        result[0] = (byte) (length >>> 24);
        result[1] = (byte) (length >>> 16);
        result[2] = (byte) (length >>> 8);
        result[3] = (byte) length;
        result[4] = PROTOCOL_VERSION;
        result[5] = code.getValue();
        result[6] = flags;
        for (int i = 0; i < data.length; ++i) {
            result[7 + i] = data[i];
        }
        return result;
    }
}
