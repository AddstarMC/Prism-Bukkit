package com.helion3.prism.libs.elixr.commands.exceptions;

public class IllegalCommandSenderException extends Exception {
    private static final long serialVersionUID = 6925519042438136165L;
    public IllegalCommandSenderException(String message) {
        super(message);
    }
}