package me.botsko.prism;

import java.util.ArrayList;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.inventory.ItemStack;

public class MaterialAliases {

	/**
	 * 
	 */
	protected FileConfiguration items;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public MaterialAliases( FileConfiguration items ) {
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
		if(item_name == null){
			ItemStack i = new ItemStack( typeid,subid);
			item_name = i.getType().name().toLowerCase().replace("_", " ");
		}
		return item_name;
	}
	
	
	/**
	 * 
	 * @param i
	 * @return
	 */
	public String getItemAlias( ItemStack i ){
		return getItemAlias( i.getTypeId(), (byte) i.getDurability() );
	}
}
