package com.helion3.prism.libs.elixr.commands.exceptions;

public class CommandPermissionException extends Exception {
    private static final long serialVersionUID = 6925419059438136165L;
    public CommandPermissionException(String message) {
        super(message);
    }
}