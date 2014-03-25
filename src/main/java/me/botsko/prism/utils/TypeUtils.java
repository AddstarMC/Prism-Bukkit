package me.botsko.prism.utils;

public class TypeUtils extends me.botsko.elixr.TypeUtils {
    
    
    /**
     * Earlier versions of elixr didn't have this...
     * @param str
     * @param desiredLength
     * @return
     */
    public static String padStringRight( String str, int desiredLength ){
        if( str.length() >= desiredLength ) return str.substring(0,desiredLength);
        StringBuilder sb = new StringBuilder();
        int rest = desiredLength - str.length();
        for(int i = 1; i < rest; i++){
            sb.append(" ");
        }
        return str+sb.toString();
    }
}