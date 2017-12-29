package com.helion3.prism.libs.elixr;

import java.util.Map;

import org.bukkit.enchantments.Enchantment;
import org.bukkit.inventory.ItemStack;

public class EnchantmentUtils {
	
	
	/**
	 * Given an enchantment, does the current item have any that conflict
	 * @return
	 */
	public static boolean hasConflictingEnchanment( ItemStack item, Enchantment ench ){
		Map<Enchantment, Integer> enchantments = item.getEnchantments();
		boolean conflict = false;
		for(Enchantment e : enchantments.keySet()){
			if(ench.conflictsWith(e)){
				conflict = true;
			}
		}
		return enchantments.containsKey(ench) || conflict;
	}
	
	
	/**
	 * Return the enchantment based on a common name
	 * @param name
	 * @return
	 */
	public static Enchantment getEnchantmentFromCommonName( String name ){
		if(name.equalsIgnoreCase("aquaaffinity")){
			return Enchantment.WATER_WORKER;
		}
		else if(name.equalsIgnoreCase("bane")){
			return Enchantment.DAMAGE_ARTHROPODS;
		}
		else if(name.equalsIgnoreCase("efficiency")){
			return Enchantment.DIG_SPEED;
		}
		else if(name.equalsIgnoreCase("explosion")){
			return Enchantment.PROTECTION_EXPLOSIONS;
		}
		else if(name.equalsIgnoreCase("fall")){
			return Enchantment.PROTECTION_FALL;
		}
		else if(name.equalsIgnoreCase("fire")){
			return Enchantment.PROTECTION_FIRE;
		}
		else if(name.equalsIgnoreCase("fireaspect")){
			return Enchantment.FIRE_ASPECT;
		}
		else if(name.equalsIgnoreCase("flame")){
			return Enchantment.ARROW_FIRE;
		}
		else if(name.equalsIgnoreCase("fortune")){
			return Enchantment.LOOT_BONUS_BLOCKS;
		}
		else if(name.equalsIgnoreCase("infinity")){
			return Enchantment.ARROW_INFINITE;
		}
		else if(name.equalsIgnoreCase("knockback")){
			return Enchantment.KNOCKBACK;
		}
		else if(name.equalsIgnoreCase("looting")){
			return Enchantment.LOOT_BONUS_MOBS;
		}
		else if(name.equalsIgnoreCase("lure")){
			return Enchantment.LURE;
		}
		else if(name.equalsIgnoreCase("luck")){
			return Enchantment.LUCK;
		}
		else if(name.equalsIgnoreCase("power")){
			return Enchantment.ARROW_DAMAGE;
		}
		else if(name.equalsIgnoreCase("projectile")){
			return Enchantment.PROTECTION_PROJECTILE;
		}
		else if(name.equalsIgnoreCase("protection")){
			return Enchantment.PROTECTION_ENVIRONMENTAL;
		}
		else if(name.equalsIgnoreCase("punch")){
			return Enchantment.ARROW_KNOCKBACK;
		}
		else if(name.equalsIgnoreCase("respiration")){
			return Enchantment.OXYGEN;
		}
		else if(name.equalsIgnoreCase("sharpness")){
			return Enchantment.DAMAGE_ALL;
		}
		else if(name.equalsIgnoreCase("silktouch")){
			return Enchantment.SILK_TOUCH;
		}
		else if(name.equalsIgnoreCase("smite")){
			return Enchantment.DAMAGE_UNDEAD;
		}
		else if(name.equalsIgnoreCase("unbreaking")){
			return Enchantment.DURABILITY;
		}
		return null;
	}


	/**
	 * Return the common name for an enchantment
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
			// can leave as-is: SILK_TOUCH, FIRE_ASPECT, KNOCKBACK, THORNS, LUCK, LURE
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