package me.botsko.prism.serializers.items;

import me.botsko.prism.Prism;
import me.botsko.prism.serializers.SerializationHandler;
import me.botsko.prism.utils.EntityUtils;
import me.botsko.prism.utils.ItemUtils;
import org.bukkit.Bukkit;
import org.bukkit.Color;
import org.bukkit.DyeColor;
import org.bukkit.FireworkEffect;
import org.bukkit.Material;
import org.bukkit.NamespacedKey;
import org.bukkit.Tag;
import org.bukkit.block.BlockState;
import org.bukkit.block.Container;
import org.bukkit.block.banner.Pattern;
import org.bukkit.block.banner.PatternType;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.inventory.Inventory;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.BannerMeta;
import org.bukkit.inventory.meta.BlockStateMeta;
import org.bukkit.inventory.meta.BookMeta;
import org.bukkit.inventory.meta.EnchantmentStorageMeta;
import org.bukkit.inventory.meta.FireworkEffectMeta;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.inventory.meta.LeatherArmorMeta;
import org.bukkit.inventory.meta.SkullMeta;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * Created by Narimm on 29/10/2020.
 */
public class ItemStackSerializer {
    public int amt;
    public String name;
    public int color;
    public String owner;
    public String[] enchs;
    public String by;
    public String title;
    public String[] lore;
    public String[] content;
    public String slot = "-1";
    public int[] effectColors;
    public int[] fadeColors;
    public boolean hasFlicker;
    public boolean hasTrail;
    public short durability = 0;
    public Map<String, String> bannerMeta;
    public String material;

    public static ItemStackSerializer createItemStackSerialized(ItemStack item) {
        if (item == null || item.getAmount() <= 0) {
            return null;
        }
        ItemStackSerializer data;
        if (Tag.SHULKER_BOXES.isTagged(item.getType())) {
            data = new StorageItemStackSerializer();
            ItemMeta meta = item.getItemMeta();
            if (meta instanceof BlockStateMeta) {
                BlockState state = ((BlockStateMeta) meta).getBlockState();
                if (state instanceof Container) {
                    Inventory inv = ((Container) state).getInventory();
                    inv.forEach(itemStack -> ((StorageItemStackSerializer) data).inventoryContents
                            .add(createItemStackSerialized(itemStack)));
                }
            }
        } else {
            data = new ItemStackSerializer();
        }
        // Set basics
        data.durability = (short) ItemUtils.getItemDamage(item);
        data.amt = item.getAmount();
        data.material = item.getType().name();
        final ItemMeta meta = item.hasItemMeta() ? item.getItemMeta() : null;
        if (meta != null) {
            data.name = meta.getDisplayName();
        }
        if (meta instanceof LeatherArmorMeta) {
            final LeatherArmorMeta lam = (LeatherArmorMeta) meta;
            data.color = lam.getColor().asRGB();
        } else if (meta instanceof SkullMeta) {
            final SkullMeta skull = (SkullMeta) meta;
            if (skull.hasOwner()) {
                data.owner = Objects.requireNonNull(skull.getOwningPlayer()).getUniqueId().toString();
            }
        }

        // Written books
        if (meta instanceof BookMeta) {
            final BookMeta bookMeta = (BookMeta) meta;
            data.by = bookMeta.getAuthor();
            data.title = bookMeta.getTitle();
            data.content = bookMeta.getPages().toArray(new String[0]);
        }

        // Lore
        if (meta != null && meta.hasLore()) {
            data.lore = Objects.requireNonNull(meta.getLore()).toArray(new String[0]);
        }

        // Enchantments
        if (!item.getEnchantments().isEmpty()) {
            final String[] enchs = new String[item.getEnchantments().size()];
            int i = 0;
            for (final Map.Entry<Enchantment, Integer> ench : item.getEnchantments().entrySet()) {
                // This is silly
                enchs[i] = ench.getKey().getKey().getKey() + ":" + ench.getValue();
                i++;
            }
            data.enchs = enchs;
        } else if (meta instanceof EnchantmentStorageMeta) {
            final EnchantmentStorageMeta bookEnchantments = (EnchantmentStorageMeta) meta;
            if (bookEnchantments.hasStoredEnchants()) {
                if (bookEnchantments.getStoredEnchants().size() > 0) {
                    final String[] enchs = new String[bookEnchantments.getStoredEnchants().size()];
                    int i = 0;
                    for (final Map.Entry<Enchantment, Integer> ench : bookEnchantments.getStoredEnchants().entrySet()) {
                        // This is absolutely silly
                        enchs[i] = ench.getKey().getKey().getKey() + ":" + ench.getValue();
                        i++;
                    }
                    data.enchs = enchs;
                }
            }
        }
        if (meta instanceof FireworkEffectMeta) {
            applyFireWorkMetaToActionData(meta, data);
        }
        if (meta instanceof BannerMeta) {
            List<Pattern> patterns = ((BannerMeta) meta).getPatterns();
            Map<String, String> stringyPatterns = new HashMap<>();
            patterns.forEach(
                    pattern -> stringyPatterns.put(pattern.getPattern().getIdentifier(), pattern.getColor().name()));
            data.bannerMeta = stringyPatterns;
        }
        return data;
    }

    private static void applyFireWorkMetaToActionData(ItemMeta meta, ItemStackSerializer data) {
        final FireworkEffectMeta fireworkMeta = (FireworkEffectMeta) meta;
        if (fireworkMeta.hasEffect()) {
            final FireworkEffect effect = fireworkMeta.getEffect();
            if (effect != null) {
                if (!effect.getColors().isEmpty()) {
                    final int[] effectColors = new int[effect.getColors().size()];
                    int i = 0;
                    for (final Color effectColor : effect.getColors()) {
                        effectColors[i] = effectColor.asRGB();
                        i++;
                    }
                    data.effectColors = effectColors;
                }

                if (!effect.getFadeColors().isEmpty()) {
                    final int[] fadeColors = new int[effect.getColors().size()];
                    final int i = 0;
                    for (final Color fadeColor : effect.getFadeColors()) {
                        fadeColors[i] = fadeColor.asRGB();
                    }
                    data.fadeColors = fadeColors;
                }
                if (effect.hasFlicker()) {
                    data.hasFlicker = true;
                }
                if (effect.hasTrail()) {
                    data.hasTrail = true;
                }
            }
        }
    }

    public ItemStack toBukkit() {
        Material m = Material.matchMaterial(material);
        if (m == null)
            return null;
        ItemStack item = new ItemStack(m, amt);

        ItemUtils.setItemDamage(item, durability);

        // Restore enchantment
        if (enchs != null && enchs.length > 0) {
            for (final String ench : enchs) {
                final String[] enchArgs = ench.split(":");
                Enchantment enchantment = Enchantment.getByKey(NamespacedKey.minecraft(enchArgs[0]));

                // Restore book enchantment
                if (enchantment != null) {
                    if (item.getType() == Material.ENCHANTED_BOOK) {
                        final EnchantmentStorageMeta bookEnchantments = (EnchantmentStorageMeta) item.getItemMeta();
                        if (bookEnchantments != null) {
                            bookEnchantments.addStoredEnchant(enchantment, Integer.parseInt(enchArgs[1]), false);
                            item.setItemMeta(bookEnchantments);
                        }
                    } else {
                        item.addUnsafeEnchantment(enchantment, Integer.parseInt(enchArgs[1]));
                    }
                }
            }
        }

        ItemMeta meta = item.getItemMeta();
        if (name != null) {
            if (meta != null) {
                meta.setDisplayName(name);
            }
        }
        if (lore != null) {
            if (meta != null) {
                meta.setLore(Arrays.asList(lore));
            }
        }
        // Leather color
        if (meta instanceof LeatherArmorMeta && color > 0) {
            final LeatherArmorMeta lam = (LeatherArmorMeta) meta;
            lam.setColor(Color.fromRGB(color));
            item.setItemMeta(lam);
        } else if (meta instanceof SkullMeta && owner != null) {
            final SkullMeta skull = (SkullMeta) meta;
            skull.setOwningPlayer(Bukkit.getOfflinePlayer(EntityUtils.uuidOf(owner)));
            item.setItemMeta(skull);
        } else if (meta instanceof BookMeta) {
            final BookMeta bookMeta = (BookMeta) meta;
            bookMeta.setAuthor(by);
            bookMeta.setTitle(title);
            bookMeta.setPages(content);
            item.setItemMeta(bookMeta);
        } else if (meta instanceof FireworkEffectMeta && effectColors != null
                && effectColors.length > 0) {
            return applyFireWorksMeta(item, meta);
        } else if (meta instanceof BannerMeta && bannerMeta != null) {
            Map<String, String> stringStringMap = bannerMeta;
            List<Pattern> patterns = new ArrayList<>();
            stringStringMap.forEach((patternIdentifier, dyeName) -> {
                PatternType type = PatternType.getByIdentifier(patternIdentifier);
                try {
                    DyeColor color = DyeColor.valueOf(dyeName);
                    if (type != null) {
                        Pattern p = new Pattern(color, type);
                        patterns.add(p);
                    }
                } catch (IllegalArgumentException e){
                    Prism.debug("Found illegal DyeColor stored in serialized banner: "+e.getMessage());
                    Prism.debug("Data:  " +bannerMeta.toString());
                }
            });
            ((BannerMeta) meta).setPatterns(patterns);
        }
        if (meta != null) {
            item.setItemMeta(meta);
        }
        return item;
    }

    public static ItemStackSerializer deserialize(String data) {
        if (data == null || !data.startsWith("{")) {
            return null;
        }
        return SerializationHandler.gson().fromJson(data, ItemStackSerializer.class);
    }

    private ItemStack applyFireWorksMeta(ItemStack item, ItemMeta meta) {

        final FireworkEffectMeta fireworkMeta = (FireworkEffectMeta) meta;
        final FireworkEffect.Builder effect = FireworkEffect.builder();

        for (int effectColor : effectColors) {
            effect.withColor(Color.fromRGB(effectColor));
        }
        fireworkMeta.setEffect(effect.build());

        if (fadeColors != null) {
            for (int fadeColor : fadeColors) {
                effect.withFade(Color.fromRGB(fadeColor));
            }
            fireworkMeta.setEffect(effect.build());
        }
        if (hasFlicker) {
            effect.flicker(true);
        }
        if (hasTrail) {
            effect.trail(true);
        }
        fireworkMeta.setEffect(effect.build());
        item.setItemMeta(fireworkMeta);
        return item;
    }
}
