package com.helion3.prism.libs.elixr.commands.arguments;

import com.helion3.prism.libs.elixr.commands.exceptions.CommandArgumentException;

public interface ArgumentValidator {

    public void validate( String arg ) throws CommandArgumentException;
    
}