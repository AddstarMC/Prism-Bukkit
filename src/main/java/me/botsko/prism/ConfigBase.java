package me.botsko.prism;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.plugin.Plugin;

public class ConfigBase {

    /**
	 * 
	 */
    protected final Plugin plugin;

    /**
	 * 
	 */
    protected FileConfiguration config;

    /**
     * 
     * @param plugin
     */
    public ConfigBase(Plugin plugin) {
        this.plugin = plugin;
    }

    /**
	 *
	 */
    public FileConfiguration getConfig() {
        config = plugin.getConfig();
        return config;
    }

    /**
     * Loads language configuration
     * 
     * @return
     */
    public FileConfiguration getLang(String lang_string) {

        String lang_file = lang_string;
        if( lang_file == null ) {
            lang_file = "en-us";
        }

        // Read the base config
        return loadConfig( "languages/", lang_file );

    }

    /**
     * Returns base directory for config
     * 
     * @return
     */
    protected File getDirectory() {
        return new File( plugin.getDataFolder() + "" );
    }

    /**
     * Returns chosen filename with directory
     * 
     * @return
     */
    protected File getFilename(String filename) {
        return new File( getDirectory(), filename + ".yml" );
    }

    /**
     * 
     * @param default_folder
     * @param filename
     * @return
     */
    protected FileConfiguration loadConfig(String default_folder, String filename) {
        final File file = getFilename( filename );
        if( file.exists() ) {
            return YamlConfiguration.loadConfiguration( file );
        } else {
            // Look for defaults in the jar
            final InputStream defConfigStream = plugin.getResource( default_folder + filename + ".yml" );
            if( defConfigStream != null ) { return YamlConfiguration.loadConfiguration( defConfigStream ); }
            return null;
        }
    }

    /**
     * 
     * @param config
     */
    protected void saveConfig(String filename, FileConfiguration config) {
        final File file = getFilename( filename );
        try {
            config.save( file );
        } catch ( final IOException e ) {
            // Prism.log("Could not save the configuration file to "+file);
            // Throw exception
        }
    }

    /**
	 * 
	 */
    protected void write(String filename, FileConfiguration config) {
        try {
            final BufferedWriter bw = new BufferedWriter( new FileWriter( getFilename( filename ), true ) );
            saveConfig( filename, config );
            bw.flush();
            bw.close();
        } catch ( final IOException ignored ) {

        }
    }
}
