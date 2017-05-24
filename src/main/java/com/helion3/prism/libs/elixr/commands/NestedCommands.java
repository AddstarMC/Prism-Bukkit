package com.helion3.prism.libs.elixr.commands;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface NestedCommands {
    
    Class<?>[] value();
    
}