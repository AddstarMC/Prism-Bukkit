package com.helion3.prism.libs.elixr.commands.exceptions;

public class CommandArgumentException extends Exception {
    private static final long serialVersionUID = 6925419059441936165L;
    public CommandArgumentException(String message) {
        super(message);
    }
}