package com.helion3.prism.libs.elixr;

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
	protected Plugin plugin;
	
	/**
	 * 
	 */
	protected FileConfiguration config;
	
	/**
	 * 
	 * @param plugin
	 */
	public ConfigBase( Plugin plugin ) {
		this.plugin = plugin;
	}
	
	/**
	 * 
	 * @param plugin
	 */
	public FileConfiguration getConfig(){
		config = plugin.getConfig();
		return config;
	}
	
	/**
	 * Returns base directory for config
	 * @return
	 */
	protected File getDirectory(){
		File dir = new File(plugin.getDataFolder()+"");
		return dir;
	}
	
	/**
	 * Returns chosen filename with directory
	 * @return
	 */
	protected File getFilename( String filename ){
		File file = new File(getDirectory(), filename + ".yml");
		return file;
	}
	
	/**
	 * 
	 * @param player
	 * @return
	 */
	protected FileConfiguration loadConfig( String default_folder, String filename ){
		File file = getFilename( filename );
		if(file.exists()){
			return YamlConfiguration.loadConfiguration(file);
		} else {
			// Look for defaults in the jar
		    InputStream defConfigStream = plugin.getResource(default_folder+filename+".yml");
		    if (defConfigStream != null){
		        return YamlConfiguration.loadConfiguration(defConfigStream);
		    }
		    return null;
		}
	}
	
	/**
	 * 
	 * @param config
	 */
	protected void saveConfig( String filename, FileConfiguration config ){
		File file = getFilename( filename );
		try {
			config.save(file);
		} catch (IOException e){
			e.printStackTrace();
		}
	}
	
	/**
	 * 
	 */
	protected void write( String filename, FileConfiguration config ){
		try {
			BufferedWriter bw = new BufferedWriter( new FileWriter( getFilename( filename ), true ) );
			saveConfig( filename, config );
			bw.flush();
			bw.close();
		} catch (IOException e){

        }
	}
}