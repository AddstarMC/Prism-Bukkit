package com.helion3.prism.libs.elixr.commands.arguments;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Retention(RetentionPolicy.RUNTIME)
public @interface Argument {
    
    String name();
    
    boolean required() default false;
    
    Class<? extends ArgumentValidator> validator() default ArgumentStringValidator.class;
    
    boolean joinsRemaining() default false;
    
    String defaultValue() default "";

}