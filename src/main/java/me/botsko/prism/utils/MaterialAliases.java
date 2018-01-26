package me.botsko.prism.utils;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import org.bukkit.Material;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.inventory.ItemStack;
import org.bukkit.material.MaterialData;

public class MaterialAliases {
    
    protected static MaterialAliases instance;
    
    public static MaterialAliases getInstance(){
        if( instance == null ){
            instance = new MaterialAliases();
        }
        return instance;
    }
    
	
	/**
	 * Contains loaded item ids => aliases
	 */
	protected HashMap<String,String> itemAliases = new HashMap<String,String>();
	
	
	/**
	 * Load the yml file and save config to hashmap
	 * @param plugin
	 */
	public MaterialAliases(){
		
		FileConfiguration items = null;
		InputStream defConfigStream = this.getClass().getResourceAsStream("/items.yml");
	    if (defConfigStream != null){
	    	System.out.println("Elixr: Loaded items directory");
	    	items = YamlConfiguration.loadConfiguration(new InputStreamReader(defConfigStream));
	    }
	    
	    if( items != null ){

			// Load all item ids/aliases
			Map<String, Object> itemaliases = items.getConfigurationSection("items").getValues(false);
			
			// Cache the values for easier lookup
			if(itemaliases != null){
				for (String key : itemaliases.keySet()){
					@SuppressWarnings("unchecked")
					ArrayList<String> aliases = (ArrayList<String>)itemaliases.get(key);
					if(aliases.size() > 0){
						for(String alias : aliases){
							itemAliases.put(key.toLowerCase(), alias);
						}
					}
				}
			}
	    } else {
	    	System.out.println("ERROR: The Elixr library was unable to load an internal item alias list.");
	    }
	}
	
	
	/**
	 * Returns the loaded list of item aliases/ids;
	 * @return
	 */
	public HashMap<String,String> getItemAliases(){
		return itemAliases;
	}
	
	
	/**
	 * Returns the proper name given an item type id, data/durability
	 * @param typeid
	 * @param subid
	 * @return
	 */
	@Deprecated
	public String getAlias( int typeid, int subid ){
		return getAlias( Material.getMaterial( typeid ), subid );
	}
	
	@SuppressWarnings("deprecation")
	public String getAlias( Material material, int subid ){
		String item_name = null;
		if(!itemAliases.isEmpty()){
			String key = material.name().toLowerCase()+":"+subid;
			item_name = itemAliases.get(key);
			// TODO: Remove this for 1.13
			if( item_name == null )
				item_name = itemAliases.get( material.getId()+":"+subid );
		}
		if(item_name == null){
			ItemStack i = new ItemStack( material,subid);
			item_name = i.getType().name().toLowerCase().replace("_", " ");
		}
		return item_name;
	}
	
	/**
	 * Returns the proper name given an item stack
	 * @param i
	 * @return
	 */
	public String getAlias( ItemStack i ){
		return getAlias( i.getType(), i.getDurability() );
	}
	
	
	/**
	 * 
	 * @param alias
	 * @return
	 */
	@Deprecated
	public ArrayList<int[]> getIdsByAlias( String alias ){
		ArrayList<int[]> itemIds = new ArrayList<int[]>();
		if(!itemAliases.isEmpty()){
			for (Entry<String, String> entry : itemAliases.entrySet()){
				int[] ids = new int[2];
			    if(entry.getValue().equals( alias )){
			    	String[] _tmp = entry.getKey().split(":");
			    	ids[0] = Integer.parseInt(_tmp[0]);
			    	ids[1] = Integer.parseInt(_tmp[1]);
			    	itemIds.add(ids);
			    }
			}
		}
		return itemIds;
	}
	
	// TODO: Break this in 1.13. Woooo.
	@SuppressWarnings("deprecation")
	public ArrayList<MaterialData> getMaterialsByAlias( String alias ){
		ArrayList<MaterialData> itemIds = new ArrayList<>();
		if(!itemAliases.isEmpty()){
			for (Entry<String, String> entry : itemAliases.entrySet()){
			    if(entry.getValue().equals( alias )){
			    	String[] _tmp = entry.getKey().split(":");
			    	Material m = Material.matchMaterial(_tmp[0]);
			    	byte b = Byte.parseByte(_tmp[1]);
			    	itemIds.add(new MaterialData(m, b));
			    }
			}
		}
		return itemIds;
	}
}