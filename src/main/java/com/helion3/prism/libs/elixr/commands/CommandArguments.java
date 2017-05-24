package com.helion3.prism.libs.elixr.commands;

import java.util.HashMap;
import java.util.Map;

public class CommandArguments {
    
    protected final String[] rawArgs;
    protected final Map<String,Integer> named;
    protected final Map<String,String> flags;
    
    /**
     * 
     * @param rawArgs
     */
    public CommandArguments( String[] rawArgs, Map<String,Integer> named, Map<String,String> flags ){
        this.rawArgs = rawArgs;
        this.named = named;
        this.flags = flags;
    }
    
    /**
     * 
     * @param rawArgs
     */
    public CommandArguments( Map<String,String> flags ){
        this.rawArgs = new String[0];
        this.named = new HashMap<String,Integer>();
        this.flags = flags;
    }
    
    /**
     * Length of the arguments array
     * @return
     */
    public int length(){
        return rawArgs.length;
    }
    
    /**
     * 
     * @param namedArg
     * @return
     */
    public boolean exists( String namedArg ){
        return named.containsKey( namedArg );
    }
    
    /**
     * Returns the raw array of String arguments
     * @return
     */
    public String[] get(){
        return rawArgs;
    }
    
    /**
     * Returns a specific String argument
     * @param i
     * @return
     */
    public String get( int i ){
        return rawArgs[i];
    }
    
    /**
     * Returns a named String argument
     * @param namedArg
     * @return
     */
    public String get( String namedArg ){
        if( exists(namedArg) ){
            return get( named.get( namedArg ) );
        }
        return null;
    }
    
    /**
     * Returns an argument cast as a float
     * @param i
     * @return
     */
    public float getFloat( int i ){
        return Float.parseFloat( rawArgs[i] );
    }
    
    /**
     * Returns a named argument cast as a float
     * @param namedArg
     * @return
     */
    public float getFloat( String namedArg ){
        if( exists(namedArg) ){
            return getFloat( named.get( namedArg ) );
        }
        return 0;
    }
    
    /**
     * Returns an argument cast as an int
     * @param i
     * @return
     */
    public int getInt( int i ){
        return Integer.parseInt( rawArgs[i] );
    }
    
    /**
     * Returns a named argument cast as an int
     * @param namedArg
     * @return
     */
    public int getInt( String namedArg ){
        if( exists(namedArg) ){
            return getInt( named.get( namedArg ) );
        }
        return 0;
    }
    
    /**
     * 
     * @param flag
     * @return
     */
    public boolean hasFlag( String flag ){
        return flags.containsKey( flag );
    }
    
    /**
     * 
     * @param flag
     * @return
     */
    public String flag( String flag ){
        return flags.get( flag );
    }
}