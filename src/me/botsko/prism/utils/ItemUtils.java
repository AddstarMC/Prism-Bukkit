package me.botsko.prism.utils;

import java.util.Map;

import me.botsko.prism.MaterialAliases;

import org.bukkit.Material;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.BookMeta;
import org.bukkit.inventory.meta.EnchantmentStorageMeta;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.inventory.meta.LeatherArmorMeta;
import org.bukkit.inventory.meta.SkullMeta;

public class ItemUtils {
	
	
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
//				item_name += "dyed ("+lam.getColor().asRGB()+") ";
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
				item_name += "'" + meta.getTitle() + "' by " + meta.getAuthor();
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
		String displayName = im.getDisplayName();
		if(displayName != null){
			item_name += ", named \"" + displayName + "\"";
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