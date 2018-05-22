package me.botsko.prism.wands;

import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.PlayerInventory;

import me.botsko.prism.utils.InventoryUtils;

public abstract class WandBase implements Wand {

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
	@Deprecated
	protected int item_id = 0;
	protected Material item = Material.AIR;

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
	@Deprecated
	public int getItemId() {
		return item_id;
	}

	@Override
	public Material getItem() {
		return item;
	}

	/**
	 * @param item_id
	 *            the item_id to set
	 */
	@Deprecated
	public void setItemId(int item_id) {
		this.item_id = item_id;
	}

	public void setItem(Material material) {
		item = material;
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
		if (key.contains(":")) {
			final String[] toolKeys = key.split(":");
			item_id = Integer.parseInt(toolKeys[0]);
			item_subid = Byte.parseByte(toolKeys[1]);
		}
	}

	/**
	 * 
	 * @param item
	 */
	public void setOriginallyHeldItem(ItemStack item) {
		if (item.getType() != Material.AIR) {
			original_item = item;
		}
	}

	/**
	 * 
	 */
	public void disable(Player player) {
		final PlayerInventory inv = player.getInventory();
		if (itemWasGiven()) {
			int itemSlot;
			// Likely is what they're holding
			if (inv.getItemInMainHand().getType() == item && inv.getItemInMainHand().getDurability() == item_subid) {
				itemSlot = inv.getHeldItemSlot();
			} else {
				itemSlot = InventoryUtils.inventoryHasItem(inv, item, item_subid);
			}
			if (itemSlot > -1) {
				InventoryUtils.subtractAmountFromPlayerInvSlot(inv, itemSlot, 1);
				InventoryUtils.updateInventory(player);
			}
		}
		if (original_item != null) {
			InventoryUtils.moveItemToHand(inv, original_item.getType(), (byte) original_item.getDurability());
		}
	}
}
