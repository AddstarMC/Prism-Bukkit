package me.botsko.prism;

import java.util.ArrayList;

import org.bukkit.configuration.file.FileConfiguration;

public class Items {

	/**
	 * 
	 */
	protected FileConfiguration items;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public Items( FileConfiguration items ) {
		this.items = items;
	}
	
	
	/**
	 * 
	 * @param typeid
	 * @param subid
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public String getItemAlias( int typeid, byte subid ){
		String item_name = null;
		ArrayList<String> aliases = (ArrayList<String>) items.getList("items."+typeid+";"+subid);
		if(aliases != null && aliases.size() > 0){
			for(String s : aliases){
				return s;
			}
		}
		return item_name;
	}
}
