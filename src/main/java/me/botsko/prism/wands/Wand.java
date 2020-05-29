package me.botsko.prism.wands;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

public interface Wand {

    void playerLeftClick(Player player, Location loc);

    void playerRightClick(Player player, Location loc);

    void playerRightClick(Player player, Entity entity);

    /**
     * Mark the wand item was given to the player.
     */
    void setItemWasGiven(boolean given);

    /**
     * Check if the item has been given.
     */
    boolean itemWasGiven();

    /**
     * Get the mode of this Wand.
     * @return String
     */
    String getWandMode();

    /**
     * Set the mode of this wand.
     * @param mode String.
     */
    void setWandMode(String mode);

    /**
     * Get the material that will represent this wand.
     * @return Material
     */
    Material getItem();

    /**
     * Set the item.
     * @param material Material
     */
    void setItem(Material material);

    /**
     * Set the item from a String.
     * @param key String
     */
    void setItemFromKey(String key);

    /**
     * Set the original item held the wand replaces.
     * @param item ItemStack
     */
    void setOriginallyHeldItem(ItemStack item);

    /**
     * Disable the wand on this player.
     * @param player Player.
     */
    void disable(Player player);
}