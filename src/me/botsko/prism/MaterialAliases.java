package me.botsko.prism;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.inventory.ItemStack;

public class MaterialAliases {
	
	/**
	 * 
	 */
	protected HashMap<String,String> itemAliases = new HashMap<String,String>();
	
	
	/**
	 * 
	 * @param plugin
	 */
	public MaterialAliases( FileConfiguration items ) {

		// Load all item ids/aliases
		Map<String, Object> itemaliases = items.getConfigurationSection("items").getValues(false);
		
		// Cache the values for easier lookup
		if(itemaliases != null){
			for (String key : itemaliases.keySet()) {
				@SuppressWarnings("unchecked")
				ArrayList<String> aliases = (ArrayList<String>)itemaliases.get(key);
				if(aliases.size() > 0){
					for(String alias : aliases){
						itemAliases.put(key, alias);
					}
				}
			}
		}
	}
	
	
	/**
	 * 
	 * @param typeid
	 * @param subid
	 * @return
	 */
	public String getItemStackAliasById( int typeid, byte subid ){
		String item_name = null;
		if(!itemAliases.isEmpty()){
			String key = typeid+";"+subid;
			item_name = itemAliases.get(key);
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
	public String getItemStackAliasByItemStack( ItemStack i ){
		return getItemStackAliasById( i.getTypeId(), (byte) i.getDurability() );
	}
	
	
	/**
	 * 
	 * @param alias
	 * @return
	 */
	public int[] getItemIdsByAlias( String alias ){
		int[] ids = new int[2];
		if(!itemAliases.isEmpty()){
			for (Entry<String, String> entry : itemAliases.entrySet()){
			    if(entry.getValue().equals( alias )){
			    	String[] _tmp = entry.getKey().split(";");
			    	ids[0] = Integer.parseInt(_tmp[0]);
			    	ids[1] = Integer.parseInt(_tmp[1]);
			    }
			}
		}
		return ids;
	}
}