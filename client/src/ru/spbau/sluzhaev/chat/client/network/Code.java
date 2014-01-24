package ru.spbau.sluzhaev.chat.client.network;

public enum Code {
    LOGIN(1), FETCH(2), SEND(3), USER_LIST(4), LOGOUT(5), LOGIN_RESPONSE(6), LOGIN_ERROR(7),
    MESSAGE_LIST(8), USER_LIST_RESPONSE(9), MESSAGE_RESPONSE(10), LOGOUT_RESPONSE(11);

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
