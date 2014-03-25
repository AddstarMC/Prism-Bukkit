package me.botsko.prism;

import java.util.Hashtable;
import java.util.Map.Entry;

import org.bukkit.configuration.file.FileConfiguration;

public class Language {

    /**
	 * 
	 */
    protected final FileConfiguration lang;

    /**
     * 
     * @param lang
     */
    public Language(FileConfiguration lang) {
        this.lang = lang;
    }

    /**
     * 
     * @param key
     * @return
     */
    public String getString(String key) {
        if( lang != null ) {
            final String msg = lang.getString( key );
            if( msg != null ) {
                return colorize( msg );
            } else {
                // Prism.log("No language support found for " + key);
            }
        } else {
            // Prism.log("Language file configuration was not loaded correctly.");
        }
        return "";
    }

    /**
     * 
     * @param key
     * @param replacer
     * @return
     */
    public String getString(String key, Hashtable<String, String> replacer) {
        String msg = getString( key );
        if( !replacer.isEmpty() ) {
            for ( final Entry<String, String> entry : replacer.entrySet() ) {
                msg = msg.replace( "%(" + entry.getKey() + ")", entry.getValue() );
            }
        }
        return msg;
    }

    /**
     * Converts colors place-holders.
     * 
     * @param text
     * @return
     */
    protected String colorize(String text) {
        return text.replaceAll( "(&([a-f0-9A-F]))", "\u00A7$2" );
    }
}