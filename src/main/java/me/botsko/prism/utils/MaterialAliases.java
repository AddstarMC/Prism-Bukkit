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
	
	// TODO: org.bukkit.block.data.BlockData in 1.13
	public static class MaterialState {
		protected MaterialState() {
		}
		
		public MaterialState(Material material, String state) {
			this.material = material;
			this.state = state;
		}
		public Material material;
		public String state;
		
		@SuppressWarnings("deprecation")
		public MaterialData asData() {
			return new MaterialData(material, Byte.parseByte(state));
		}
		
		public ItemStack asItem() {
			return new ItemStack(material, 1, Short.parseShort(state));
		}
	}

	//private Map<String, String> matCache = new HashMap<>();
	//private Map<String, String> idCache = new HashMap<>();
	
	// TODO make it work
	@SuppressWarnings("deprecation")
	public MaterialState idsToMaterial(int block_id, int block_subid) {
		return new MaterialState(Material.getMaterial(block_id), String.valueOf(block_subid));
		/*String key = block_id + ":" + block_subid;
		String result = matCache.get(key);
		
		if(result != null) {
			String[] parts = result.split(":", 2);
			Prism.log("matCache: [" + block_id + ", " + block_subid + "] -> [" + parts[0] + ", " + parts[1] + "]");
			return new MaterialState(Material.matchMaterial(parts[0]), parts[1]);
		}
		
		MaterialState state = new MaterialState();
		
		IdMapQuery query = new IdMapQuery();
		
		query.findMaterial(block_id, block_subid, (m, s) -> {
			matCache.put(key, m + ":" + s);
			Prism.log("matQuery: [" + block_id + ", " + block_subid + "] -> [" + m + ", " + s + "]");
			state.material = Material.matchMaterial(m.toUpperCase(Locale.ENGLISH));
			state.state = s;
		},
		() -> {
			Material m = Material.matchMaterial(String.valueOf(block_id));
			if(m != null) {
				String matName = m.name().toLowerCase(Locale.ENGLISH);
				String matState = String.valueOf(block_subid);
				query.map(matName, matState, block_id, block_subid);
				Prism.log("matMatch: [" + block_id + ", " + block_subid + "] -> [" + matName + ", " + matState + "]");

				matCache.put(key, matName + ":" + matState);
				idCache.put(matName + ":" + matState, key);
				
				state.material = m;
				// TODO: Proper state handling 1.13
				state.state = matState;
			}
			else
				Prism.log("matError: [" + block_id + ", " + block_subid + "] -> ???");
		});
		
		if(state.material == null)
			return null;
		
		return state;*/
	}
	
	@SuppressWarnings("deprecation")
	public int[] materialToIds(Material material, String state) {
		int data = 0;
		try { data = Integer.parseInt(state); } catch(NumberFormatException e) {}
		return new int[] { material.getId(), data };
		/*String matName = material.name().toLowerCase(Locale.ENGLISH);
		
		String key = matName + ":" + state;
		String result = idCache.get(key);
		
		if(result != null) {
			String[] parts = result.split(":", 2);
			Prism.log("idCache: [" + matName + ", " + state + "] -> [" + parts[0] + ", " + parts[1] + "]");
			return new int[] { Integer.parseInt(parts[0]), Integer.parseInt(parts[1]) };
		}
		
		int[] ids = new int[2];
		
		IdMapQuery query = new IdMapQuery();
		
		query.findIds(matName, state, (i, d) -> {
			idCache.put(key, i + ":" + d);
			Prism.log("idQuery: [" + matName + ", " + state + "] -> [" + i + ", " + d + "]");
			ids[0] = i;
			ids[1] = d;
		},
		() -> {
			int id = 0;
			int data = 0;

			try { data = Integer.parseInt(state); } catch(NumberFormatException e) {}
			
			try {
				id = (Integer)Material.class.getMethod("getId").invoke(material);
				query.map(matName, state, id, data);
				Prism.log("idFunc: [" + matName + ", " + state + "] -> [" + id + ", " + data + "]");
			}
			catch(IllegalAccessException | IllegalArgumentException | InvocationTargetException | NoSuchMethodException | SecurityException e) {
				id = query.mapAutoId(matName, state);
				Prism.log("idAuto: [" + matName + ", " + state + "] -> [" + id + ", " + data + "]");
			}
			
			idCache.put(key, id + ":" + data);
			matCache.put(id + ":" + data, key);
			
			ids[0] = id;
			ids[1] = data;
		});
		
		return ids;*/
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