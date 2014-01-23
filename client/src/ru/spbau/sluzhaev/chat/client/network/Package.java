package ru.spbau.sluzhaev.chat.client.network;

public class Package {
    private final byte PROTOCOL_VERSION = 1;
    private final Code code;
    private byte flags;
    private byte[] data;

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

    enum Code {
        LOGIN(0), FETCH(1), SEND(2), USER_LIST(3), LOGOUT(4), LOGIN_RESPONSE(5),
        LOGIN_ERROR(6), MESSAGE_LIST(7), USER_LIST_RESPONSE(8);

        private final byte value;

        private Code(int value) {
            this.value = (byte) value;
        }

        static Code fromInt(int value) {
            for (Code code : Code.values()) {
                if (code.value == value) {
                    return code;
                }
            }

            return null;
        }


        public byte getValue() {
            return value;
        }
    }
}
