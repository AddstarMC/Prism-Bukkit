package me.botsko.prism.utils;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.block.data.BlockData;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.inventory.ItemStack;

import me.botsko.prism.Prism;
import me.botsko.prism.database.mysql.MySQLIdMapQuery;

public class MaterialAliases {
	/**
	 * Contains loaded item ids => aliases
	 */
	protected HashMap<String, String> itemAliases = new HashMap<String, String>();

	/**
	 * Load the yml file and save config to hashmap
	 */
	public MaterialAliases() {
		FileConfiguration items = null;
		InputStream defConfigStream = this.getClass().getResourceAsStream("/items.yml");
		if (defConfigStream != null) {
			System.out.println("Elixr: Loaded items directory");
			items = YamlConfiguration.loadConfiguration(new InputStreamReader(defConfigStream));
		}

		if (items != null) {

			// Load all item ids/aliases
			Map<String, Object> itemaliases = items.getConfigurationSection("items").getValues(false);

			// Cache the values for easier lookup
			if (itemaliases != null)
				for (Map.Entry<String, Object> entry : itemaliases.entrySet()) {
					@SuppressWarnings("unchecked")
					List<String> aliases = (List<String>) entry.getValue();

					for (String alias : aliases)
						itemAliases.put(entry.getKey().toLowerCase(), alias);
				}

		}
		else
			System.out.println("ERROR: The Elixr library was unable to load an internal item alias list.");
	}

	public void initAllMaterials() {
		initMaterials(Material.values());
	}

	public void initMaterials(Material... materials) {
		MySQLIdMapQuery query = new MySQLIdMapQuery();

		for (Material m : materials) {
			String matName = m.name().toLowerCase(Locale.ENGLISH);
			String dataString;
			
			try {
				dataString = BlockUtils.dataString(Bukkit.createBlockData(m));
			}
			catch(IllegalArgumentException e) {
				continue;
			}

			query.findIds(m.name().toLowerCase(Locale.ENGLISH), dataString, (i, d) -> {
				storeCache(m, dataString, i, d);
			}, () -> {
				int id = query.mapAutoId(matName, dataString);

				storeCache(m, dataString, id, 0);
			});
		}
	}

	public static class MaterialState {
		protected MaterialState() {
		}

		public MaterialState(Material material, String state) {
			this.material = material;
			this.state = state;
		}

		public Material material;
		public String state;

		public BlockData asBlockData() {
			try {
				BlockData data = Bukkit.createBlockData(material, state);

				// In the event that we tried to get BlockData for an item and it returned air
				if (data.getMaterial() == material) {
					return data;
				}
			}
			catch (IllegalArgumentException e) {
			}

			return null;
		}

		public ItemStack asItem() {
			ItemStack item = new ItemStack(material, 1);
			
			if (!state.isEmpty()) {
				try {
					ItemUtils.setItemDamage(item, Short.parseShort(state));
				}
				catch (NumberFormatException e) {
				}
			}

			return item;
		}

		@Override
		public String toString() {
			return material.name().toLowerCase(Locale.ENGLISH) + state;
		}

		@Override
		public int hashCode() {
			return toString().hashCode();
		}
	}

	private final Map<String, String> matCache = new HashMap<>();
	private final Map<String, String> idCache = new HashMap<>();
	private final Map<Material, Set<IntPair>> allIdsCache = new HashMap<>();

	private Set<IntPair> getIdsOf(Material material) {
		Set<IntPair> ids = allIdsCache.get(material);

		if (ids != null) {
			return ids;
		}

		MySQLIdMapQuery query = new MySQLIdMapQuery();

		query.findAllIds(material.name().toLowerCase(Locale.ENGLISH), list -> {
			allIdsCache.put(material, new HashSet<>(list));
		});

		return allIdsCache.get(material);
	}

	private void storeCache(Material material, String state, int block_id, int block_subid) {
		String matKey = material.name().toLowerCase(Locale.ENGLISH) + ":" + state;
		String idKey = block_id + ":" + block_subid;

		matCache.put(idKey, matKey);
		idCache.put(matKey, idKey);

		getIdsOf(material).add(new IntPair(block_id, block_subid));
	}

	private MaterialState fromCache(int block_id, int block_subid) {
		String value = matCache.get(block_id + ":" + block_subid);

		if (value != null) {
			String[] parts = value.split(":", 2);

			return new MaterialState(Material.matchMaterial(parts[0]), parts[1]);
		}

		return null;
	}

	public MaterialState idsToMaterial(int block_id, int block_subid, Boolean logMaterialErrors) {
		MaterialState cachedMaterial = fromCache(block_id, block_subid);

		if (cachedMaterial != null) {
			return cachedMaterial;
		}

		MaterialState result = new MaterialState();
		MySQLIdMapQuery query = new MySQLIdMapQuery();

		query.findMaterial(block_id, block_subid, (material, state) -> {
			result.material = Material.matchMaterial(material.toUpperCase(Locale.ENGLISH));
			result.state = state;

			storeCache(result.material, result.state, block_id, block_subid);
		}, () -> {
			if (logMaterialErrors)
				Prism.log("matError: [" + block_id + ", " + block_subid + "] -> ???");
		});

		if (result.material == null) {
			return null;
		}

		return result;
	}

	private IntPair fromCache(Material material, String state) {
		String value = idCache.get(material.name().toLowerCase(Locale.ENGLISH) + ":" + state);

		if (value != null) {
			String[] parts = value.split(":", 2);

			return new IntPair(Integer.parseInt(parts[0]), Integer.parseInt(parts[1]));
		}

		return null;
	}

	public IntPair materialToIds(Material material, String state) {
		int block_subid;

		// For tools, where durability doesn't mean a different item (different cached
		// value) but is still important
		int durability = 0;
		try {
			durability = Integer.parseInt(state);
		}
		catch (NumberFormatException e) {
		}

		if (material.getMaxDurability() > 0)
			block_subid = 0;
		else
			block_subid = durability;

		IntPair cachedIds = fromCache(material, block_subid == 0 ? state : String.valueOf(block_subid));

		if (cachedIds != null) {
			return cachedIds;
		}

		IntPair result = new IntPair(0, 0);
		MySQLIdMapQuery query = new MySQLIdMapQuery();
		String materialName = material.name().toLowerCase(Locale.ENGLISH);

		synchronized (this) {
			query.findIds(materialName, state, (query_id, query_subid) -> {
				result.first = query_id;
				result.second = query_subid;
	
				storeCache(material, state, query_id, query_subid);
			}, () -> {
				int block_id = query.mapAutoId(materialName, state);
	
				result.first = block_id;
	
				storeCache(material, state, block_id, 0);
			});
		}

		if (block_subid != durability)
			result.second = durability;

		return result;
	}

	public Set<IntPair> materialToAllIds(Material material) {
		return Collections.unmodifiableSet(getIdsOf(material));
	}

	public Set<IntPair> partialBlockDataIds(Material material, String partialBlockData)
			throws IllegalArgumentException {
		String fullBlockData = BlockUtils.dataString(Bukkit.createBlockData(material, partialBlockData));

		String[] parts = fullBlockData.substring(1, fullBlockData.length() - 1).toLowerCase(Locale.ENGLISH).split(",");

		StringBuilder likeString = new StringBuilder("%");

		for (String string : parts) {
			if (partialBlockData.contains(string)) {
				likeString.append(string).append('%');
			}
		}

		String stateLike = likeString.toString();

		MySQLIdMapQuery query = new MySQLIdMapQuery();

		Set<IntPair> ids = new HashSet<>();
		query.findAllIdsPartial(material.name().toLowerCase(Locale.ENGLISH), stateLike, list -> {
			ids.addAll(list);
		});

		return ids;
	}

	public static final int SUBID_WILDCARD = -1;

	@Deprecated
	public IntPair materialToIdsWildcard(Material material, String state) {
		IntPair ids = materialToIds(material, state);

		if (material.getMaxDurability() > 0)
			ids.second = SUBID_WILDCARD;

		return ids;
	}

	/**
	 * Returns the loaded list of item aliases/ids;
	 * 
	 * @return
	 */
	public HashMap<String, String> getItemAliases() {
		return itemAliases;
	}

	public String getAlias(Material material, BlockData data) {
		String dataString = BlockUtils.dataString(data);

		String item_name = null;
		if (!itemAliases.isEmpty()) {
			String key = material.name().toLowerCase() + dataString;
			item_name = itemAliases.get(key);
		}
		if (item_name == null) {
			item_name = material.name().toLowerCase().replace("_", " ");
		}
		return item_name;
	}

	/**
	 * Returns the proper name given an item stack
	 * 
	 * @param i
	 * @return
	 */
	public String getAlias(ItemStack i) {
		return i.getType().name().toLowerCase().replace("_", " ");
	}

	/**
	 * 
	 * @param alias
	 * @return
	 */
	// TODO: Break this in 1.13. Woooo.
	public ArrayList<Material> getMaterialsByAlias(String alias) {
		ArrayList<Material> itemIds = new ArrayList<>();
		if (!itemAliases.isEmpty()) {
			for (Entry<String, String> entry : itemAliases.entrySet()) {
				if (entry.getValue().equals(alias)) {
					itemIds.add(Material.matchMaterial(entry.getKey()));
				}
			}
		}
		return itemIds;
	}
}