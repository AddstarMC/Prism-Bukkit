package me.botsko.prism;

import org.bukkit.configuration.file.FileConfiguration;

public class Language {

	/**
	 * 
	 */
	protected FileConfiguration lang;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public Language( FileConfiguration lang ) {
		this.lang = lang;
	}
	
	
	/**
	 * 
	 * @param key
	 * @return
	 */
	public String getString( String key ){
		if(lang != null){
			String msg = lang.getString(key);
			if(msg != null){
				return msg;
			} else {
//				plugin.log("No language support found for " + key);
			}
		} else {
//			plugin.log("Language file configuration was not loaded correctly.");
		}
		return "";
	}
}