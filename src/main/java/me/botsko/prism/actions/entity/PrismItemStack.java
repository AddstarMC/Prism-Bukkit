package me.botsko.prism.actions.entity;

import com.google.common.base.Preconditions;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.ItemMeta;
import org.jetbrains.annotations.NotNull;

import java.io.Serializable;
import javax.annotation.Nonnull;

/**
 * Created for the Prism-Bukkit Project.
 * Created by Narimm on 20/10/2020.
 */
public class PrismItemStack implements Serializable {
    public Material material;
    public int count;
    public ItemMeta itemMeta;

    /**
     * Construct the Item from a Bukkit Item Stack.
     * @param item ItemStack
     * @return PrismItemStack.
     */
    public static @NotNull PrismItemStack fromBukkit(@NotNull ItemStack item) {
        Preconditions.checkNotNull(item);
        PrismItemStack result = new PrismItemStack();
        result.count = item.getAmount();
        result.material = item.getType();
        result.itemMeta = item.getItemMeta();
        return result;
    }

    /**
     * Convert back to bukkit.
     * @return ItemStack
     */
    public ItemStack toBukkit() {
        ItemStack item = new ItemStack(material,count);
        item.setItemMeta(itemMeta);
        return item;
    }
}
