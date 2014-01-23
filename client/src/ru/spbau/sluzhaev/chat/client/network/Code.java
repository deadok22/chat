package ru.spbau.sluzhaev.chat.client.network;

public enum Code {
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
