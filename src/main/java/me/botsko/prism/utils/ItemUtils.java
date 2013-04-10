package me.botsko.prism.utils;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;

import me.botsko.prism.MaterialAliases;

import org.bukkit.Material;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.entity.Player;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.PlayerInventory;
import org.bukkit.inventory.meta.BookMeta;
import org.bukkit.inventory.meta.EnchantmentStorageMeta;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.inventory.meta.LeatherArmorMeta;
import org.bukkit.inventory.meta.SkullMeta;

public class ItemUtils {
	
	
	/**
	 * 
	 * @param inv
	 * @param item_id
	 * @param sub_id
	 * @return
	 */
	public static int inventoryHasItem( Inventory inv, int item_id, int sub_id ){
		int currentSlot = 0;
		for(ItemStack item : inv.getContents()){
			if( item != null && item.getTypeId() == item_id && item.getDurability() == sub_id ){
				return currentSlot;
			}
			currentSlot++;
		}
		return -1;
	}
	
	
	/**
	 * 
	 * @param inv
	 * @param item_id
	 * @param sub_id
	 * @return
	 */
	public static boolean moveItemToHand( PlayerInventory inv, int item_id, byte sub_id ){
		int slot = inventoryHasItem( inv, item_id, sub_id );
		if( slot > -1 ){
			ItemStack item = inv.getItem(slot);
			inv.clear(slot);
			// If the player has an item in-hand, switch to a vacant spot
			if( !playerHasEmptyHand(inv) ){
				inv.setItem(slot, inv.getItemInHand());
			}
			inv.setItemInHand(item);
			return true;
		}
		return false;
	}
	
	
	/**
	 * 
	 * @param inv
	 * @return
	 */
	public static boolean playerHasEmptyHand( PlayerInventory inv ){
		return (inv.getItemInHand().getTypeId() == 0);
	}
	
	
	/**
	 * 
	 * @param player
	 */
	public static HashMap<Integer,ItemStack> addItemToInventory( Inventory inv, ItemStack item ){
		return inv.addItem(item);
	}
	
	
	/**
	 * 
	 * @param player
	 */
	public static boolean handItemToPlayer( PlayerInventory inv, ItemStack item ){
		// Ensure there's at least one empty inv spot
		if( inv.firstEmpty() != -1 ){
			ItemStack originalItem = inv.getItemInHand().clone();
			// If the player has an item in-hand, switch to a vacant spot
			if( !playerHasEmptyHand( inv ) ){
				// We need to manually add the item stack to a different
				// slot because by default, bukkit combines items with addItem
				// and that was causing items to be lost unless they were the max
				// stack size
				for(int i = 0; i <= inv.getSize(); i++){
					if( i == inv.getHeldItemSlot() ) continue;
					ItemStack current = inv.getItem(i);
					if( current == null ){
						inv.setItem(i, originalItem);
						break;
					}
				}
			}
			inv.setItemInHand(item);
			return true;
		}
		return false;
	}
	
	
	/**
	 * 
	 * @param inv
	 * @param slot
	 * @param quant
	 */
	public static void subtractAmountFromPlayerInvSlot( PlayerInventory inv, int slot, int quant ){
		ItemStack itemAtSlot = inv.getItem(slot);
		if( itemAtSlot != null && quant <= 64 ){
			itemAtSlot.setAmount( itemAtSlot.getAmount() - quant );
			if( itemAtSlot.getAmount() == 0 ){
				inv.clear(slot);
			}
		}
	}
	
	
	/**
	 * 
	 * @param leftovers
	 * @param player
	 */
	public static void dropLeftoverItems( HashMap<Integer,ItemStack> leftovers, Player player ){
		if(!leftovers.isEmpty()){
			for (Entry<Integer, ItemStack> entry : leftovers.entrySet()){
			    player.getWorld().dropItemNaturally(player.getLocation(), entry.getValue());
			}
		}
	}
	
	
	/**
	 * 
	 * @param item_id
	 * @param sub_id
	 * @return
	 */
	public static boolean isAcceptableWand( int item_id, byte sub_id ){
		
		// Water/lava
		if( item_id >=8 && item_id <= 11 ){
			return false;
		}
		// Fire
		if( item_id == 51 || item_id == 259 ){
			return false;
		}
		// Portal
		if( item_id == 90 || item_id == 119 ){
			return false;
		}
		// Monster
		if( item_id == 383 ){
			return false;
		}
		return true;
	}
	
	
	/**
	 * Returns a nice name for the item, along with enchantment names,
	 * etc.
	 * @return string
	 */
	public static String getItemFullNiceName( ItemStack item, MaterialAliases aliases ){
		
		String item_name = "";
		
		// Leather Coloring
		if(item.getType().name().contains("LEATHER_")){
			LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
			if(lam.getColor() != null){
				item_name += "dyed ";
			}
		}
		
		// Skull Owner
		else if(item.getType().equals(Material.SKULL_ITEM)){
			SkullMeta skull = (SkullMeta) item.getItemMeta();
			if(skull.hasOwner()){
				item_name += skull.getOwner() + "'s ";
			}
		}
		
		// Set the base item name
		if(BlockUtils.hasSubitems(item.getTypeId())){
			item_name += aliases.getItemStackAliasById(item.getTypeId(), (byte)item.getDurability());
		} else {
			item_name += aliases.getItemStackAliasById(item.getTypeId(), (byte)0);
		}
		if(item_name.isEmpty()){
			item_name += item.getType().toString().toLowerCase().replace("_", " ");
		}
		
		// Written books
		if(item.getType().equals( Material.WRITTEN_BOOK )){
	        BookMeta meta = (BookMeta) item.getItemMeta();
			if(meta != null){
				item_name += " '" + meta.getTitle() + "' by " + meta.getAuthor();
			}
		}
		
		// Enchanted books
		else if(item.getType().equals( Material.ENCHANTED_BOOK )){
			EnchantmentStorageMeta bookEnchantments = (EnchantmentStorageMeta) item.getItemMeta();
			if(bookEnchantments.hasStoredEnchants()){
				int i = 1;
				Map<Enchantment,Integer> enchs = bookEnchantments.getStoredEnchants();
				if(enchs.size() > 0){
					item_name += " with";
					for (Map.Entry<Enchantment, Integer> ench : enchs.entrySet()){
						item_name += " " + ItemUtils.getClientSideEnchantmentName( ench.getKey(), ench.getValue() );
						item_name += (i < enchs.size() ? ", " : "");
						i++;
					}
				}
			}
		}
		
		// Enchantments
		int i = 1;
		Map<Enchantment,Integer> enchs = item.getEnchantments();
		if(enchs.size() > 0){
			item_name += " with";
			for (Map.Entry<Enchantment, Integer> ench : enchs.entrySet()){
				item_name += " " + ItemUtils.getClientSideEnchantmentName( ench.getKey(), ench.getValue() );
				item_name += (i < enchs.size() ? ", " : "");
				i++;
			}
		}
		
		// Custom item names
		ItemMeta im = item.getItemMeta();
		if(im != null){
			String displayName = im.getDisplayName();
			if(displayName != null){
				item_name += ", named \"" + displayName + "\"";
			}
		}
		
		return item_name;
		
	}
	
	
	/**
	 * Determines a price for enchanting an item
	 * @param ench
	 * @param level
	 * @return
	 */
	public static String getClientSideEnchantmentName( Enchantment ench, int level ){
		
		String ench_name = "";
		
		if(ench.equals(Enchantment.ARROW_DAMAGE)){
			ench_name = "power";
		}
		else if(ench.equals(Enchantment.ARROW_FIRE)){
			ench_name = "flame";
		}
		else if(ench.equals(Enchantment.ARROW_INFINITE)){
			ench_name = "infinity";
		}
		else if(ench.equals(Enchantment.ARROW_KNOCKBACK)){
			ench_name = "punch";
		}
		else if(ench.equals(Enchantment.DAMAGE_ALL)){
			ench_name = "sharpness";
		}
		else if(ench.equals(Enchantment.DAMAGE_ARTHROPODS)){
			ench_name = "bane of anthropods";
		}
		else if(ench.equals(Enchantment.DAMAGE_UNDEAD)){
			ench_name = "damage undead";
		}
		else if(ench.equals(Enchantment.DIG_SPEED)){
			ench_name = "efficiency";
		}
		else if(ench.equals(Enchantment.DURABILITY)){
			ench_name = "unbreaking";
		}
		else if(ench.equals(Enchantment.LOOT_BONUS_BLOCKS)){
			ench_name = "fortune";
		}
		else if(ench.equals(Enchantment.LOOT_BONUS_MOBS)){
			ench_name = "looting";
		}
		else if(ench.equals(Enchantment.OXYGEN)){
			ench_name = "respiration";
		}
		else if(ench.equals(Enchantment.PROTECTION_ENVIRONMENTAL)){
			ench_name = "protection";
		}
		else if(ench.equals(Enchantment.PROTECTION_EXPLOSIONS)){
			ench_name = "blast protection";
		}
		else if(ench.equals(Enchantment.PROTECTION_FALL)){
			ench_name = "feather falling";
		}
		else if(ench.equals(Enchantment.PROTECTION_FIRE)){
			ench_name = "fire protection";
		}
		else if(ench.equals(Enchantment.PROTECTION_PROJECTILE)){
			ench_name = "projectile protection";
		}
		else if(ench.equals(Enchantment.WATER_WORKER)){
			ench_name = "aqua affinity";
		}
		else {
			// can leave as-is: SILK_TOUCH, FIRE_ASPECT, KNOCKBACK, THORNS
			ench_name = ench.getName().toLowerCase().replace("_", " ");
		}
		
		if(level == 1){
			ench_name += " I";
		}
		else if(level == 2){
			ench_name += " II";
		}
		else if(level == 3){
			ench_name += " III";
		}
		else if(level == 4){
			ench_name += " IV";
		}
		else if(level == 5){
			ench_name += " V";
		}
		
		return ench_name;

	}
}