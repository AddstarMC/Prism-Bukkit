package me.botsko.prism;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;

public class PrismConfig {
	
	/**
	 * 
	 */
	protected Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public PrismConfig( Prism plugin ) {
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param plugin
	 */
	public FileConfiguration getConfig(){
		
		FileConfiguration config = plugin.getConfig();
		
		// set defaults
		config.addDefault("prism.debug", false);
//		config.addDefault("prism.language", "en-us");
		
		config.addDefault("prism.mysql.hostname", "127.0.0.1");
		config.addDefault("prism.mysql.username", "root");
		config.addDefault("prism.mysql.password", "");
		config.addDefault("prism.mysql.database", "minecraft");
		config.addDefault("prism.mysql.port", "3306");
		
		config.addDefault("prism.default-radius", 10);
		config.addDefault("prism.max-radius-unless-overridden", 100);
		
		config.addDefault("prism.near.default-radius", 5);
		
		config.addDefault("prism.ignore.players-in-creative", true);
		config.addDefault("prism.ignore.players", new ArrayList<String>());
		config.addDefault("prism.ignore.worlds", new ArrayList<String>());
		
		config.addDefault("prism.clear-records-after", "8w");
		
		config.addDefault("prism.appliers.allow_rollback_items_removed_from_container", true);
		config.addDefault("prism.appliers.notify-nearby.enabled", true);
		config.addDefault("prism.appliers.notify-nearby.additional-radius", 20);
		config.addDefault("prism.appliers.remove-fire-on-burn-rollback", true);
		config.addDefault("prism.appliers.remove-drops-on-explode-rollback", true);
//		config.addDefault("prism.appliers.remove-liquid-on-flow-rollback", true);
		
		config.addDefault("prism.tracking.block-break", true);
		config.addDefault("prism.tracking.block-burn", true);
		config.addDefault("prism.tracking.block-fade", true);
		config.addDefault("prism.tracking.block-fall", true);
		config.addDefault("prism.tracking.block-form", true);
		config.addDefault("prism.tracking.block-place", true);
		config.addDefault("prism.tracking.block-shift", true);
		config.addDefault("prism.tracking.block-use", true);
		config.addDefault("prism.tracking.bonemeal-use", true);
		config.addDefault("prism.tracking.container-access", true);
		config.addDefault("prism.tracking.creeper-explode", true);
		config.addDefault("prism.tracking.crop-trample", true);
		config.addDefault("prism.tracking.enderman-pickup", true);
		config.addDefault("prism.tracking.enderman-place", true);
		config.addDefault("prism.tracking.entity-break", true);
		config.addDefault("prism.tracking.entity-explode", true);
		config.addDefault("prism.tracking.entity-follow", true);
		config.addDefault("prism.tracking.entity-kill", true);
		config.addDefault("prism.tracking.entity-shear", true);
		config.addDefault("prism.tracking.entity-spawn", true);
		config.addDefault("prism.tracking.fireball", true);
		config.addDefault("prism.tracking.hangingitem-break", true);
		config.addDefault("prism.tracking.hangingitem-place", true);
		config.addDefault("prism.tracking.item-drop", true);
		config.addDefault("prism.tracking.item-insert", true);
		config.addDefault("prism.tracking.item-pickup", true);
		config.addDefault("prism.tracking.item-remove", true);
		config.addDefault("prism.tracking.lava-break", true);
		config.addDefault("prism.tracking.lava-bucket", true);
		config.addDefault("prism.tracking.lava-flow", true);
		config.addDefault("prism.tracking.lava-ignite", true);
		config.addDefault("prism.tracking.leaf-decay", true);
		config.addDefault("prism.tracking.lighter", true);
		config.addDefault("prism.tracking.lightning", true);
		config.addDefault("prism.tracking.mushroom-grow", true);
		config.addDefault("prism.tracking.player-death", true);
		config.addDefault("prism.tracking.player-command", false);
		config.addDefault("prism.tracking.sheep-eat", true);
		config.addDefault("prism.tracking.sign-change", true);
		config.addDefault("prism.tracking.skull-break", true);
		config.addDefault("prism.tracking.skull-place", true);
		config.addDefault("prism.tracking.spawnegg-use", true);
		config.addDefault("prism.tracking.tnt-explode", true);
		config.addDefault("prism.tracking.tree-grow", true);
		config.addDefault("prism.tracking.water-break", true);
		config.addDefault("prism.tracking.water-bucket", true);
		config.addDefault("prism.tracking.water-flow", true);
		
		config.addDefault("prism.alerts.alert-staff-to-applied-process", true);
		config.addDefault("prism.alerts.ores.enabled", true);
		config.addDefault("prism.alerts.ores.log-to-console", true);
		config.addDefault("prism.alerts.ores.coal", false);
		config.addDefault("prism.alerts.ores.redstone", false);
		config.addDefault("prism.alerts.ores.lapis", true);
		config.addDefault("prism.alerts.ores.iron", true);
		config.addDefault("prism.alerts.ores.gold", true);
		config.addDefault("prism.alerts.ores.diamond", true);
		config.addDefault("prism.alerts.ores.emerald", true);
		
		// Enable monitoring of item use/placement
		config.addDefault("prism.alerts.uses.enabled", true);
		config.addDefault("prism.alerts.uses.ignore-staff", true);
		config.addDefault("prism.alerts.uses.lighter", true);
		config.addDefault("prism.alerts.uses.lava", true);
		
		List<String> monitorItems = new ArrayList<String>();
		monitorItems.add("7");
		monitorItems.add("29");
		monitorItems.add("46");
		monitorItems.add("10");
		monitorItems.add("11");
		config.addDefault("prism.alerts.uses.item-placement", monitorItems);

		// Copy defaults
		config.options().copyDefaults(true);
		
		// save the defaults/config
		plugin.saveConfig();
		
		return config;
		
	}
	
	
	/**
	 * Loads language configuration
	 * @return
	 */
	public FileConfiguration getLang(){

		String lang_file = plugin.config.getString("prism.language");
		if(lang_file == null){
			lang_file = "en-us";
		}
		
		// Read the base config
		FileConfiguration config = loadConfig( "languages/", lang_file );
		return config;
		
	}
	
	
	/**
	 * Loads item database
	 * @return
	 */
	public FileConfiguration getItems(){
		FileConfiguration config = loadConfig( "", "items" );
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
		    if (defConfigStream != null) {
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
		} catch (IOException e) {
			plugin.log("Could not save the configuration file to "+file);
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
