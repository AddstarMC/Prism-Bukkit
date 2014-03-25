package me.botsko.prism.wands;

import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.PlayerInventory;

import me.botsko.elixr.InventoryUtils;

public abstract class WandBase {

    /**
	 * 
	 */
    protected boolean item_given = false;

    /**
	 * 
	 */
    protected String wand_mode;

    /**
	 * 
	 */
    protected int item_id = 0;

    /**
	 * 
	 */
    protected byte item_subid = 0;

    /**
	 * 
	 */
    protected ItemStack original_item;

    /**
	 * 
	 */
    public void setItemWasGiven(boolean given) {
        this.item_given = given;
    }

    /**
	 * 
	 */
    public boolean itemWasGiven() {
        return item_given;
    }

    /**
     * 
     * @param mode
     */
    public void setWandMode(String mode) {
        wand_mode = mode;
    }

    /**
     * 
     * @param mode
     */
    public String getWandMode() {
        return wand_mode;
    }

    /**
     * @return the item_id
     */
    public int getItemId() {
        return item_id;
    }

    /**
     * @param item_id
     *            the item_id to set
     */
    public void setItemId(int item_id) {
        this.item_id = item_id;
    }

    /**
     * @return the item_subid
     */
    public byte getItemSubId() {
        return item_subid;
    }

    /**
     * @param item_subid
     *            the item_subid to set
     */
    public void setItemSubId(byte item_subid) {
        this.item_subid = item_subid;
    }

    /**
     * 
     * @param key
     */
    public void setItemFromKey(String key) {
        if( key.contains( ":" ) ) {
            final String[] toolKeys = key.split( ":" );
            item_id = Integer.parseInt( toolKeys[0] );
            item_subid = Byte.parseByte( toolKeys[1] );
        }
    }

    /**
     * 
     * @param item
     */
    public void setOriginallyHeldItem(ItemStack item) {
        if( item.getTypeId() > 0 ) {
            original_item = item;
        }
    }

    /**
	 * 
	 */
    public void disable(Player player) {
        final PlayerInventory inv = player.getInventory();
        if( itemWasGiven() ) {
            int itemSlot;
            // Likely is what they're holding
            if( inv.getItemInHand().getTypeId() == item_id && inv.getItemInHand().getDurability() == item_subid ) {
                itemSlot = inv.getHeldItemSlot();
            } else {
                itemSlot = InventoryUtils.inventoryHasItem( inv, item_id, item_subid );
            }
            if( itemSlot > -1 ) {
                InventoryUtils.subtractAmountFromPlayerInvSlot( inv, itemSlot, 1 );
                player.updateInventory();
            }
        }
        if( original_item != null ) {
            InventoryUtils.moveItemToHand( inv, original_item.getTypeId(), (byte) original_item.getDurability() );
        }
    }
}
