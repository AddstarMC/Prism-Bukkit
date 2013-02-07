package me.botsko.prism.actions;

import java.util.Map;
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
	
	/**
	 * 
	 */
	protected ItemStack item;

	/**
	 * 
	 */
	protected int quantity;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public ItemStackAction( ActionType action_type, ItemStack item, int quantity, Location loc, String player_name ){
		
		super(action_type, player_name);
		
		if(item != null){
			this.item = item;
			this.quantity = quantity;
			this.world_name = loc.getWorld().getName();
			this.x = loc.getX();
			this.y = loc.getY();
			this.z = loc.getZ();
		}
		
		// Set data from current block
		setDataFromItem();
		setItemStackFromData();
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		setItemStackFromData();
	}
	
	
	/**
	 * 
	 */
	protected void setDataFromItem(){
		if(data == null && item != null){
			
			// Set item info/quant
			data = item.getTypeId() + ":" + item.getDurability() + ":" + quantity;
			
		
			
			addExtraData();
			
			// Enchanted book?
			if(item.getType().equals( Material.ENCHANTED_BOOK )){
				EnchantmentStorageMeta bookEnchantments = (EnchantmentStorageMeta) item.getItemMeta();
				if(bookEnchantments.hasStoredEnchants()){
					Map<Enchantment,Integer> enchs = bookEnchantments.getStoredEnchants();
					if(enchs.size() > 0){
						for (Map.Entry<Enchantment, Integer> ench : enchs.entrySet()){
							data += ":" + ench.getKey().getId() + "," + ench.getValue();
						}
					}
				}
			}
			
			// Enchanted weapons/armor
			else if(!item.getEnchantments().isEmpty()){
				for(Entry<Enchantment, Integer> ench : item.getEnchantments().entrySet()){
					data += ":" + ench.getKey().getId() + "," + ench.getValue();
				}
			}
		}
	}
	
	protected void addExtraData() {
		// Append meta info for specific item types
		if(item.getType().name().contains("LEATHER_")){
			data += addLeatherArmorColors();
		} else if(item.getType().equals(Material.SKULL_ITEM)){
			data += addSkullItemOwner();
		}
		data += addDisplayName();
	}


	/**
	 * We need to add the owner for a skull head if there is one.
	 * @return
	 */
	private String addSkullItemOwner() {
		SkullMeta meta = (SkullMeta) item.getItemMeta();
		if(meta.hasOwner()){
			return ":skullowner~" + meta.getOwner();
		}
		return "";
	}


	/**
	 * We need to add the colors for Leather Armor to the data.
	 * @return
	 */
	private String addLeatherArmorColors() {
		LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
		if(lam.getColor() != null){
			return ":color~" + lam.getColor().asRGB();
		}
		return "";
	}
	
	/**
   * We need to add the custom name for an item
   * @return
   */
	protected String addDisplayName() {
	  ItemMeta meta = item.getItemMeta();
	  if(meta.getDisplayName() != null){
		  return ":name~" + meta.getDisplayName();
	  }
	  return "";
	}


	/**
	 * 
	 */
	protected void setItemStackFromData(){
		if(item == null && data != null){
			
			String[] blockArr = data.split(":");
			if (!TypeUtils.isNumeric(blockArr[0])) return;
			
			if (blockArr.length >= 3){
				
				// Parse item/sub/quant
				int block_id = Integer.parseInt(blockArr[0]);
				int block_subid = Integer.parseInt(blockArr[1]);
				quantity = Integer.parseInt(blockArr[2]);
				
				item = new ItemStack(block_id,quantity,(short)block_subid);
				
					// Restore enchantments
					for(int i = 3; i < blockArr.length; i++){
						if(blockArr[i].contains("~")){
							handleExtraData(blockArr[i]);
						} else {
							if(blockArr[i].contains(";")){
								handleOldDataFormat(blockArr[i]);
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
		}
	
	  /**	
	   * Our old way of handling data used ;'s. We need to still interpret it, 
	   * but we won't use it. Hopefully at some point we can remove this.
	   */
	  protected void handleOldDataFormat(String data){
	    if(item.getType().name().contains("LEATHER_")){
	      LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
	      lam.setColor(Color.fromRGB(Integer.parseInt(data.replaceAll(";", ""))));
	      item.setItemMeta(lam);
	    } else if(item.getType().equals(Material.SKULL_ITEM)){
	      SkullMeta meta = (SkullMeta) item.getItemMeta();	
	      meta.setOwner(data.replaceAll(";", ""));
	      item.setItemMeta(meta);
	     }	
	   }
	
	  
		   protected void handleExtraData(String data) {
		     if(data.contains("color~")){
		       String rgb = data.replace("color~", "");
		       if (!TypeUtils.isNumeric(rgb)) return;
		       int color = Integer.parseInt(rgb);
		       LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
		       lam.setColor(Color.fromRGB(color));
		       item.setItemMeta(lam);
		     } else if(data.contains("skullowner~")){
		       String owner = data.replace("skullowner~", "");
		       SkullMeta meta = (SkullMeta) item.getItemMeta();
		       meta.setOwner(owner);
		       item.setItemMeta(meta);
		     } else if(data.contains("name~")){
		       String name = data.replace("name~", "");
		       ItemMeta meta = item.getItemMeta();
		       meta.setDisplayName(name);
		       item.setItemMeta(meta);
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
		if(item != null){
			name = quantity + " " + this.materialAliases.getItemStackAliasByItemStack(item);
		}
		return name;
	}
}