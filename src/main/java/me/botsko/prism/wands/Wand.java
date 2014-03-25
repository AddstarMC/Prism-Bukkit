package me.botsko.prism.wands;

import org.bukkit.Location;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

public interface Wand {

    /**
	 * 
	 */
    public void playerLeftClick(Player player, Location loc);

    /**
	 * 
	 */
    public void playerRightClick(Player player, Location loc);

    /**
     * 
     * @param player
     * @param entity
     */
    public void playerRightClick(Player player, Entity entity);

    /**
     * 
     * @param given
     */
    public void setItemWasGiven(boolean given);

    /**
     * 
     * @return
     */
    public boolean itemWasGiven();

    /**
     * 
     * @param mode
     */
    public void setWandMode(String mode);

    /**
     * 
     * @param mode
     */
    public String getWandMode();

    /**
     * @return the item_id
     */
    public int getItemId();

    /**
     * @param item_id
     *            the item_id to set
     */
    public void setItemId(int item_id);

    /**
     * @return the item_subid
     */
    public byte getItemSubId();

    /**
     * @param item_subid
     *            the item_subid to set
     */
    public void setItemSubId(byte item_subid);

    /**
     * 
     * @param key
     */
    public void setItemFromKey(String key);

    /**
     * 
     * @param item
     */
    public void setOriginallyHeldItem(ItemStack item);

    /**
     * 
     * @param player
     */
    public void disable(Player player);

}