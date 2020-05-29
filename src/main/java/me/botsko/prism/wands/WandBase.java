package me.botsko.prism.wands;

import me.botsko.prism.utils.InventoryUtils;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.PlayerInventory;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public abstract class WandBase implements Wand {

    protected boolean itemGiven = false;

    protected String wandMode;

    protected Material item = Material.AIR;

    protected ItemStack originalItem;

    /**
     * {@inheritDoc}
     */
    public void setItemWasGiven(boolean given) {
        this.itemGiven = given;
    }

    /**
     * {@inheritDoc}
     */
    public boolean itemWasGiven() {
        return itemGiven;
    }

    /**
     * {@inheritDoc}
     */
    public String getWandMode() {
        return wandMode;
    }

    /**
     * {@inheritDoc}
     */
    public void setWandMode(String mode) {
        wandMode = mode;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Material getItem() {
        return item;
    }

    /**
     * {@inheritDoc}
     */
    public void setItem(Material material) {
        item = material;
    }

    /**
     * {@inheritDoc}
     */
    public void setItemFromKey(String key) {
        item = Material.matchMaterial(key);
    }

    /**
     * {@inheritDoc}
     */
    public void setOriginallyHeldItem(ItemStack item) {
        if (item.getType() != Material.AIR) {
            originalItem = item;
        }
    }

    /**
     * {@inheritDoc}
     */
    public void disable(Player player) {
        final PlayerInventory inv = player.getInventory();
        if (itemWasGiven()) {
            int itemSlot;
            // Likely is what they're holding
            if (inv.getItemInMainHand().getType() == item) {
                itemSlot = inv.getHeldItemSlot();
            } else {
                itemSlot = InventoryUtils.inventoryHasItem(inv, item);
            }
            if (itemSlot > -1) {
                InventoryUtils.subtractAmountFromPlayerInvSlot(inv, itemSlot, 1);
                InventoryUtils.updateInventory(player);
            }
        }
        if (originalItem != null) {
            InventoryUtils.moveItemToHand(inv, originalItem.getType());
        }
    }
}
