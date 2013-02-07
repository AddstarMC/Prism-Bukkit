package me.botsko.prism.actions;

import java.util.Map.Entry;

import me.botsko.prism.utils.TypeUtils;

import org.bukkit.Color;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.EnchantmentStorageMeta;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.inventory.meta.LeatherArmorMeta;
import org.bukkit.inventory.meta.SkullMeta;

public class ItemStackAction extends GenericAction {
	
	public class ItemStackActionData {
		public int block_id;
		public int block_subid;
		public int amt;
		public String name;
		public int color;
		public String owner;
		public String[] enchs;
	}
	
	/**
	 * 
	 */
	protected ItemStack item;
	
	/**
	 * 
	 */
	protected ItemStackActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public ItemStackAction( ActionType action_type, ItemStack item, int quantity, Location loc, String player_name ){
		
		super(action_type, player_name);
		
		actionData = new ItemStackActionData();
		
		if(item != null){
			
			this.item = item;
			this.world_name = loc.getWorld().getName();
			this.x = loc.getX();
			this.y = loc.getY();
			this.z = loc.getZ();
			
			// Set basics
			actionData.block_id = item.getTypeId();
			actionData.block_subid = item.getDurability();
			actionData.amt = quantity;
			
			// Set additional data all items may have
			ItemMeta meta = item.getItemMeta();
			if(meta.getDisplayName() != null){
				actionData.name = meta.getDisplayName();
			}

			// Leather Coloring
			if(item.getType().name().contains("LEATHER_")){
				LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
				if(lam.getColor() != null){
					actionData.color = lam.getColor().asRGB();
				}
			}
			
			// Skull Owner
			else if(item.getType().equals(Material.SKULL_ITEM)){
				SkullMeta skull = (SkullMeta) item.getItemMeta();
				if(skull.hasOwner()){
					actionData.owner = skull.getOwner();
				}
			}
			
			// Enchantments
			if(!item.getEnchantments().isEmpty()){
				String[] enchs = new String[item.getEnchantments().size()];
				int i = 0;
				for(Entry<Enchantment, Integer> ench : item.getEnchantments().entrySet()){
					enchs[i] = ench.getKey().getId() + ":" + ench.getValue();
					i++;
				}
				actionData.enchs = enchs;
			}

			// Book enchantments
			else if(item.getType().equals( Material.ENCHANTED_BOOK )){
				EnchantmentStorageMeta bookEnchantments = (EnchantmentStorageMeta) item.getItemMeta();
				if(bookEnchantments.hasStoredEnchants()){
					if(bookEnchantments.getStoredEnchants().size() > 0){
						String[] enchs = new String[bookEnchantments.getStoredEnchants().size()];
						int i = 0;
						for(Entry<Enchantment, Integer> ench : bookEnchantments.getStoredEnchants().entrySet()){
							enchs[i] = ench.getKey().getId() + ":" + ench.getValue();
							i++;
						}
						actionData.enchs = enchs;
					}
				}
			}
		}
		
		setDataFromObject();
		
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		setItemStackFromData();
	}
	

	/**
	 * Prism began tracking very little data about an item stack and we felt
	 * that an object wasn't necessary. That soon became a bad decision
	 * because we kept piling on data to an existing string. Now we have a lot of old
	 * data that won't work with the new object, so we must keep around the
	 * old parsing methods.
	 */
	protected void setItemStackFromData(){
		if(item == null && data != null){
			
			// New format?
			if(data.startsWith("{")){
				setItemStackFromNewDataFormat();
			} 
			
			// Old format!
			else {
				setItemStackFromOldDataFormat();
			}
		}
	}
	
	
	/**
	 * 
	 */
	protected void setDataFromObject(){
		data = gson.toJson(actionData);
	}

	
	/**
	 * 
	 */
	protected void setObjectFromData(){
		if(data != null){
			actionData = gson.fromJson(data, ItemStackActionData.class);
		}
	}
	
	
	/**
	 * 
	 */
	protected void setItemStackFromNewDataFormat(){
		
		// Set json data back to object
		setObjectFromData();
		
		item = new ItemStack(actionData.block_id,actionData.amt,(short)actionData.block_subid);
		
		// Restore enchantment
		if(actionData.enchs != null && actionData.enchs.length > 0){
			for( String ench : actionData.enchs){
				String[] enchArgs = ench.split(":");
				Enchantment enchantment = Enchantment.getById(Integer.parseInt(enchArgs[0]));
				// Restore book enchantment
				if(item.getType().equals(Material.ENCHANTED_BOOK)){
					EnchantmentStorageMeta bookEnchantments = (EnchantmentStorageMeta) item.getItemMeta();
					bookEnchantments.addStoredEnchant(enchantment, Integer.parseInt(enchArgs[1]), false);
					item.setItemMeta(bookEnchantments);
				} 
				// Restore item enchantment
				else {
					item.addUnsafeEnchantment(enchantment, Integer.parseInt(enchArgs[1]));
				}
			}
		}
		
		// Leather color
		if( item.getType().name().contains("LEATHER_") && actionData.color > 0 ){
			LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
			lam.setColor(Color.fromRGB( actionData.color ));
			item.setItemMeta(lam);
		}
		// Skulls
		else if( item.getType().equals(Material.SKULL_ITEM) && actionData.owner != null ){
			SkullMeta meta = (SkullMeta) item.getItemMeta();
			meta.setOwner( actionData.owner );
			item.setItemMeta(meta);
		}
		
		// Item display names
		if( actionData.name != null ){
			ItemMeta meta = item.getItemMeta();
			meta.setDisplayName( actionData.name );
			item.setItemMeta(meta);
		}
	}
		
		
	/**
	 * This parsing method is only for item records from prior to 1.2.
	 * @deprecated
	 */
	protected void setItemStackFromOldDataFormat(){
		
		String[] blockArr = data.split(":");
		if (!TypeUtils.isNumeric(blockArr[0])) return;
		
		if (blockArr.length >= 3){
			
			// Parse item/sub/quant
			actionData.block_id = Integer.parseInt(blockArr[0]);
			actionData.block_subid = Integer.parseInt(blockArr[1]);
			actionData.amt = Integer.parseInt(blockArr[2]);
			
			item = new ItemStack(actionData.block_id,actionData.amt,(short)actionData.block_subid);
			
			// Restore enchantments
			for(int i = 3; i < blockArr.length; i++){
				if(blockArr[i].contains("~")){
					if(blockArr[i].contains("color~")){
						String rgb = blockArr[i].replace("color~", "");
						if (!TypeUtils.isNumeric(rgb)) return;
						int color = Integer.parseInt(rgb);
						LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
						lam.setColor(Color.fromRGB(color));
						item.setItemMeta(lam);
					} 
					else if(blockArr[i].contains("skullowner~")){
						String owner = blockArr[i].replace("skullowner~", "");
						SkullMeta meta = (SkullMeta) item.getItemMeta();
						meta.setOwner(owner);
						item.setItemMeta(meta);
					}
					else if(blockArr[i].contains("name~")){
						String name = blockArr[i].replace("name~", "");
						ItemMeta meta = item.getItemMeta();
						meta.setDisplayName(name);
						item.setItemMeta(meta);
					}
				} else {
					if(blockArr[i].contains(";")){
						if(item.getType().name().contains("LEATHER_")){
							LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
							lam.setColor(Color.fromRGB(Integer.parseInt(blockArr[i].replaceAll(";", ""))));
							item.setItemMeta(lam);
						}
						else if(item.getType().equals(Material.SKULL_ITEM)){
							SkullMeta meta = (SkullMeta) item.getItemMeta();
							meta.setOwner(blockArr[i].replaceAll(";", ""));
							item.setItemMeta(meta);
						}
						continue;
					}
					String[] enchArgs = blockArr[i].split(",");
					Enchantment enchantment = Enchantment.getById(Integer.parseInt(enchArgs[0]));
					
					// Restore book enchantments
					if(item.getType().equals(Material.ENCHANTED_BOOK)){
						EnchantmentStorageMeta bookEnchantments = (EnchantmentStorageMeta) item.getItemMeta();
						bookEnchantments.addStoredEnchant(enchantment, Integer.parseInt(enchArgs[1]), false);
						item.setItemMeta(bookEnchantments);
					} else {
					
						// Restore item enchants
						item.addUnsafeEnchantment(enchantment, Integer.parseInt(enchArgs[1]));
						
					}
				}
			}
		}
	}

	
	/**
	 * 
	 * @return
	 */
	public ItemStack getItem(){
		return item;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "";
		if(actionData != null){
			name = actionData.amt + " " + this.materialAliases.getItemStackAliasById( actionData.block_id, (byte)actionData.block_subid );
		}
		return name;
	}
}