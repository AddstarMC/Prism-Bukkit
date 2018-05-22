package me.botsko.prism.utils;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import org.bukkit.Material;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.inventory.ItemStack;
import org.bukkit.material.MaterialData;

import me.botsko.prism.Prism;
import me.botsko.prism.database.mysql.IdMapQuery;

public class MaterialAliases {
	/**
	 * Contains loaded item ids => aliases
	 */
	protected HashMap<String, String> itemAliases = new HashMap<String, String>();

	/**
	 * Load the yml file and save config to hashmap
	 * 
	 * @param plugin
	 */

	protected EnumSet<Material> nonData = EnumSet.noneOf(Material.class);

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
			if (itemaliases != null) {
				for (Map.Entry<String, Object> entry : itemaliases.entrySet()) {
					@SuppressWarnings("unchecked")
					ArrayList<String> aliases = (ArrayList<String>) entry.getValue();
					if (aliases.size() > 0) {
						for (String alias : aliases) {
							itemAliases.put(entry.getKey().toLowerCase(), alias);
						}
					}
				}
			}
		} else {
			System.out.println("ERROR: The Elixr library was unable to load an internal item alias list.");
		}

		for (Material material : Material.values()) {
			if (!material.isBlock())
				nonData.add(material);
		}
	}

	public void initAllMaterials() {
		initMaterials(Material.values());
	}

	public void initMaterials(Material... materials) {
		IdMapQuery query = new IdMapQuery();

		for (Material m : materials) {
			String matName = m.name().toLowerCase(Locale.ENGLISH);

			int max = 1;
			if (m.isBlock())
				max = 16;

			for (int x = 0; x < max; ++x) {
				int fx = x;
				String key = matName + ":" + x;

				query.findIds(m.name().toLowerCase(Locale.ENGLISH), String.valueOf(x), (i, d) -> {
					idCache.put(key, i + ":" + d);
					matCache.put(i + ":" + d, key);
				}, () -> {
					int id = 0;
					int data = fx;

					try {
						id = (Integer) Material.class.getMethod("getId").invoke(m);
						query.map(matName, String.valueOf(fx), id, data);
					} catch (Exception e) {
						id = query.mapAutoId(matName, String.valueOf(fx));
					}

					idCache.put(key, id + ":" + data);
					matCache.put(id + ":" + data, key);
				});
			}
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

	private Map<String, String> matCache = new HashMap<>();
	private Map<String, String> idCache = new HashMap<>();

	public MaterialState idsToMaterial(int block_id, int block_subid) {
		String key = block_id + ":" + block_subid;
		String result = matCache.get(key);

		if (result != null) {
			String[] parts = result.split(":", 2);
			// Prism.log("matCache: [" + block_id + ", " + block_subid + "] -> [" + parts[0]
			// + ", " + parts[1] + "]");
			return new MaterialState(Material.matchMaterial(parts[0]), parts[1]);
		}

		MaterialState state = new MaterialState();

		IdMapQuery query = new IdMapQuery();

		query.findMaterial(block_id, block_subid, (m, s) -> {
			matCache.put(key, m + ":" + s);
			matCache.put(m + ":" + s, key);
			// Prism.log("matQuery: [" + block_id + ", " + block_subid + "] -> [" + m + ", "
			// + s + "]");
			state.material = Material.matchMaterial(m.toUpperCase(Locale.ENGLISH));
			state.state = s;
		}, () -> {
			Material m;
			try {
				m = (Material) Material.class.getMethod("getMaterial", int.class).invoke(null, block_id);
			} catch (Exception e) {
				m = null;
				e.printStackTrace();
			}

			if (m != null) {
				String matName = m.name().toLowerCase(Locale.ENGLISH);
				String matState = String.valueOf(block_subid);
				query.map(matName, matState, block_id, block_subid);
				// Prism.log("matMatch: [" + block_id + ", " + block_subid + "] -> [" + matName
				// + ", " + matState + "]");

				matCache.put(key, matName + ":" + matState);
				idCache.put(matName + ":" + matState, key);

				state.material = m;
				// TODO: Proper state handling 1.13
				state.state = matState;
			} else
				Prism.log("matError: [" + block_id + ", " + block_subid + "] -> ???");
		});

		if (state.material == null) {
			// Prism.log("Error: Null material! input: [" + block_id + ", " + block_subid +
			// "]");
			return null;
		}

		/*
		 * MaterialState asdf = new MaterialState(Material.getMaterial(block_id),
		 * String.valueOf(block_subid));
		 * 
		 * 
		 * 
		 * if(asdf.material != state.material || !asdf.state.equals(state.state)) {
		 * Prism.log("materialToIds mismatch: [" + block_id + ", " + block_subid + "]");
		 * Prism.log("    expected: [" + asdf.material + ", " + asdf.state + "]");
		 * Prism.log("    recieved: [" + state.material + ", " + state.state + "]"); }
		 */

		return state;
	}

	public int[] materialToIds(Material material, String state) {
		String matName = material.name().toLowerCase(Locale.ENGLISH);

		String key = matName + ":" + state;
		String result = idCache.get(key);

		if (result != null) {
			String[] parts = result.split(":", 2);
			// Prism.log("idCache: [" + matName + ", " + state + "] -> [" + parts[0] + ", "
			// + parts[1] + "]");
			return new int[] { Integer.parseInt(parts[0]), Integer.parseInt(parts[1]) };
		}

		int[] ids = new int[2];

		IdMapQuery query = new IdMapQuery();

		query.findIds(matName, state, (i, d) -> {
			idCache.put(key, i + ":" + d);
			matCache.put(i + ":" + d, key);
			// Prism.log("idQuery: [" + matName + ", " + state + "] -> [" + i + ", " + d +
			// "]");
			ids[0] = i;
			ids[1] = d;
		}, () -> {
			int id = 0;
			int data = 0;

			try {
				data = Integer.parseInt(state);
			} catch (NumberFormatException e) {
			}

			try {
				id = (Integer) Material.class.getMethod("getId").invoke(material);
				query.map(matName, state, id, data);
				// Prism.log("idFunc: [" + matName + ", " + state + "] -> [" + id + ", " + data
				// + "]");
			} catch (Exception e) {
				id = query.mapAutoId(matName, state);
				// Prism.log("idAuto: [" + matName + ", " + state + "] -> [" + id + ", " + data
				// + "]");
			}

			idCache.put(key, id + ":" + data);
			matCache.put(id + ":" + data, key);

			ids[0] = id;
			ids[1] = data;
		});

		/*
		 * int data = 0; try { data = Integer.parseInt(state); }
		 * catch(NumberFormatException e) {} int[] asdf = { material.getId(), data };
		 * 
		 * if(ids[0] != asdf[0] || ids[1] != asdf[1]) {
		 * Prism.log("materialToIds mismatch: [" + material + ", " + state + "]");
		 * Prism.log("    expected: [" + asdf[0] + ", " + asdf[1] + "]");
		 * Prism.log("    recieved: [" + ids[0] + ", " + ids[1] + "]"); }
		 */

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

	/**
	 * Returns the proper name given an item type id, data/durability
	 * 
	 * @param typeid
	 * @param subid
	 * @return
	 */
	@Deprecated
	public String getAlias(int typeid, int subid) {
		return getAlias(Material.getMaterial(typeid), subid);
	}

	@SuppressWarnings("deprecation")
	public String getAlias(Material material, int subid) {
		String item_name = null;
		if (!itemAliases.isEmpty()) {
			String key = material.name().toLowerCase() + ":" + subid;
			item_name = itemAliases.get(key);
			// TODO: Remove this for 1.13
			if (item_name == null)
				item_name = itemAliases.get(material.getId() + ":" + subid);
		}
		if (item_name == null) {
			ItemStack i = new ItemStack(material, subid);
			item_name = i.getType().name().toLowerCase().replace("_", " ");
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
		return getAlias(i.getType(), i.getDurability());
	}

	/**
	 * 
	 * @param alias
	 * @return
	 */
	@Deprecated
	public ArrayList<int[]> getIdsByAlias(String alias) {
		ArrayList<int[]> itemIds = new ArrayList<int[]>();
		if (!itemAliases.isEmpty()) {
			for (Entry<String, String> entry : itemAliases.entrySet()) {
				int[] ids = new int[2];
				if (entry.getValue().equals(alias)) {
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
	public ArrayList<MaterialData> getMaterialsByAlias(String alias) {
		ArrayList<MaterialData> itemIds = new ArrayList<>();
		if (!itemAliases.isEmpty()) {
			for (Entry<String, String> entry : itemAliases.entrySet()) {
				if (entry.getValue().equals(alias)) {
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