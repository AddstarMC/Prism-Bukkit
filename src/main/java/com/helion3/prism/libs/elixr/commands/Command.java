package com.helion3.prism.libs.elixr.commands;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import com.helion3.prism.libs.elixr.commands.arguments.Argument;
import com.helion3.prism.libs.elixr.commands.arguments.Flag;


@Retention(RetentionPolicy.RUNTIME)
public @interface Command {
    
    String[] aliases();

    String desc() default "";
    
    String[] permissions() default {};
    
    Argument[] arguments() default {};
    
    Flag[] flags() default {};
    
    boolean allowsConsole() default true;
    
    boolean playerRequired() default false;

}