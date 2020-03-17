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
    void setItemWasGiven(boolean given);

    boolean itemWasGiven();

    String getWandMode();

    void setWandMode(String mode);

    Material getItem();

    void setItem(Material material);

    void setItemFromKey(String key);

    void setOriginallyHeldItem(ItemStack item);

    void disable(Player player);
}