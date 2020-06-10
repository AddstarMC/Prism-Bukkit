package me.botsko.prism.utils;

import org.bukkit.Keyed;
import org.bukkit.NamespacedKey;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.inventory.ItemStack;

import java.util.Map;

public class EnchantmentUtils {

    /**
     * Given an enchantment, does the current item have any that conflict.
     *
     * @return bool
     */
    @SuppressWarnings("unused")
    public static boolean hasConflictingEnchanment(ItemStack item, Enchantment ench) {
        Map<Enchantment, Integer> enchantments = item.getEnchantments();
        boolean conflict = false;
        for (Enchantment e : enchantments.keySet()) {
            if (ench.conflictsWith(e)) {
                conflict = true;
            }
        }
        return (enchantments.containsKey(ench) || conflict);
    }

    /**
     * Return the enchantment based on a common name.
     *
     * @param name String
     * @return Enchantment
     */
    @SuppressWarnings("unused")
    public static Enchantment getEnchantmentFromCommonName(String name) {
        if (name.equalsIgnoreCase("aquaaffinity")) {
            return Enchantment.WATER_WORKER;
        } else if (name.equalsIgnoreCase("bane")) {
            return Enchantment.DAMAGE_ARTHROPODS;
        } else if (name.equalsIgnoreCase("efficiency")) {
            return Enchantment.DIG_SPEED;
        } else if (name.equalsIgnoreCase("explosion")) {
            return Enchantment.PROTECTION_EXPLOSIONS;
        } else if (name.equalsIgnoreCase("fall")) {
            return Enchantment.PROTECTION_FALL;
        } else if (name.equalsIgnoreCase("fire")) {
            return Enchantment.PROTECTION_FIRE;
        } else if (name.equalsIgnoreCase("fireaspect")) {
            return Enchantment.FIRE_ASPECT;
        } else if (name.equalsIgnoreCase("flame")) {
            return Enchantment.ARROW_FIRE;
        } else if (name.equalsIgnoreCase("fortune")) {
            return Enchantment.LOOT_BONUS_BLOCKS;
        } else if (name.equalsIgnoreCase("infinity")) {
            return Enchantment.ARROW_INFINITE;
        } else if (name.equalsIgnoreCase("knockback")) {
            return Enchantment.KNOCKBACK;
        } else if (name.equalsIgnoreCase("looting")) {
            return Enchantment.LOOT_BONUS_MOBS;
        } else if (name.equalsIgnoreCase("lure")) {
            return Enchantment.LURE;
        } else if (name.equalsIgnoreCase("luck")) {
            return Enchantment.LUCK;
        } else if (name.equalsIgnoreCase("power")) {
            return Enchantment.ARROW_DAMAGE;
        } else if (name.equalsIgnoreCase("projectile")) {
            return Enchantment.PROTECTION_PROJECTILE;
        } else if (name.equalsIgnoreCase("protection")) {
            return Enchantment.PROTECTION_ENVIRONMENTAL;
        } else if (name.equalsIgnoreCase("punch")) {
            return Enchantment.ARROW_KNOCKBACK;
        } else if (name.equalsIgnoreCase("respiration")) {
            return Enchantment.OXYGEN;
        } else if (name.equalsIgnoreCase("sharpness")) {
            return Enchantment.DAMAGE_ALL;
        } else if (name.equalsIgnoreCase("silktouch")) {
            return Enchantment.SILK_TOUCH;
        } else if (name.equalsIgnoreCase("smite")) {
            return Enchantment.DAMAGE_UNDEAD;
        } else if (name.equalsIgnoreCase("unbreaking")) {
            return Enchantment.DURABILITY;
        } else if (name.equals("vanishing curse")) {
            return Enchantment.VANISHING_CURSE;
        } else {
            String formattedName = name.replace(' ','_');
            NamespacedKey key = NamespacedKey.minecraft(formattedName);
            return Enchantment.getByKey(key);
        }
    }

    /**
     * Return the common name for an enchantment.
     *
     * @param ench Keyed
     * @param level int
     * @return String
     */
    public static String getClientSideEnchantmentName(Keyed ench, int level) {

        String enchName;

        if (ench.equals(Enchantment.ARROW_DAMAGE)) {
            enchName = "power";
        } else if (ench.equals(Enchantment.ARROW_FIRE)) {
            enchName = "flame";
        } else if (ench.equals(Enchantment.ARROW_INFINITE)) {
            enchName = "infinity";
        } else if (ench.equals(Enchantment.ARROW_KNOCKBACK)) {
            enchName = "punch";
        } else if (ench.equals(Enchantment.DAMAGE_ALL)) {
            enchName = "sharpness";
        } else if (ench.equals(Enchantment.DAMAGE_ARTHROPODS)) {
            enchName = "bane of anthropods";
        } else if (ench.equals(Enchantment.DAMAGE_UNDEAD)) {
            enchName = "damage undead";
        } else if (ench.equals(Enchantment.DIG_SPEED)) {
            enchName = "efficiency";
        } else if (ench.equals(Enchantment.DURABILITY)) {
            enchName = "unbreaking";
        } else if (ench.equals(Enchantment.LOOT_BONUS_BLOCKS)) {
            enchName = "fortune";
        } else if (ench.equals(Enchantment.LOOT_BONUS_MOBS)) {
            enchName = "looting";
        } else if (ench.equals(Enchantment.OXYGEN)) {
            enchName = "respiration";
        } else if (ench.equals(Enchantment.PROTECTION_ENVIRONMENTAL)) {
            enchName = "protection";
        } else if (ench.equals(Enchantment.PROTECTION_EXPLOSIONS)) {
            enchName = "blast protection";
        } else if (ench.equals(Enchantment.PROTECTION_FALL)) {
            enchName = "feather falling";
        } else if (ench.equals(Enchantment.PROTECTION_FIRE)) {
            enchName = "fire protection";
        } else if (ench.equals(Enchantment.PROTECTION_PROJECTILE)) {
            enchName = "projectile protection";
        } else if (ench.equals(Enchantment.WATER_WORKER)) {
            enchName = "aqua affinity";
        } else if (ench.equals(Enchantment.VANISHING_CURSE)) {
            enchName = "vanishing curse";
        }else {
            // can leave as-is: SILK_TOUCH, FIRE_ASPECT, KNOCKBACK, THORNS, LUCK, LURE
            enchName = ench.getKey().getKey().toLowerCase().replace("_", " ");
        }
        switch (level) {
            case 2:
                enchName += " II";
                break;
            case 3:
                enchName += " III";
                break;
            case 4:
                enchName += " IV";
                break;
            case 5:
                enchName += " V";
                break;
            case 1:
            default:
                enchName += " I";
                break;
        }
        return enchName;

    }
}