package me.botsko.prism.utils;

import java.util.EnumSet;
import java.util.Locale;
import java.util.Map;
import java.util.Map.Entry;

import org.bukkit.Bukkit;
import org.bukkit.FireworkEffect;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.BookMeta;
import org.bukkit.inventory.meta.Damageable;
import org.bukkit.inventory.meta.EnchantmentStorageMeta;
import org.bukkit.inventory.meta.FireworkEffectMeta;
import org.bukkit.inventory.meta.FireworkMeta;
import org.bukkit.inventory.meta.ItemMeta;
import org.bukkit.inventory.meta.LeatherArmorMeta;
import org.bukkit.inventory.meta.PotionMeta;
import org.bukkit.inventory.meta.SkullMeta;
import org.bukkit.potion.PotionEffect;

import me.botsko.prism.utils.EnchantmentUtils;

public class ItemUtils {

	/**
	 * 
	 * @param item_id
	 * @param sub_id
	 * @return
	 */
	private static EnumSet<Material> badWands = EnumSet.of(Material.WATER, Material.LAVA, Material.FIRE,
			Material.FLINT_AND_STEEL, Material.NETHER_PORTAL, Material.END_PORTAL);

	public static boolean isAcceptableWand(Material material) {
		return !badWands.contains(material);
	}

	public static String smallString(ItemStack stack) {
		if (stack != null) {
			String result = stack.getType().name().toLowerCase();

			short durability = (short) getItemDamage(stack);
			if (durability > 0)
				result += ":" + durability;
			return result;
		}
		return null;
	}
	
	public static void setItemDamage(ItemStack stack, int damage) {
		ItemMeta meta = Bukkit.getItemFactory().getItemMeta(stack.getType());
		
		if(meta instanceof Damageable) {
			Damageable d = (Damageable) meta;
			
			d.setDamage(damage);
			stack.setItemMeta(meta);
		}
	}
	
	public static int getItemDamage(ItemStack stack) {
		ItemMeta meta = stack.getItemMeta();
		
		if(meta instanceof Damageable) {
			Damageable d = (Damageable) meta;
			
			return d.getDamage();
		}
		
		return 0;
	}

	public static ItemStack itemOf(String smallString) {
		if (smallString != null) {
			String[] parts = smallString.split(":", 2);
			Material mat = Material.matchMaterial(parts[0].toUpperCase());

			if (mat != null) {
				if (parts.length > 1)
					try {
						ItemStack stack = new ItemStack(mat, 1);
						setItemDamage(stack, Short.valueOf(parts[1]));

						return stack;
					} catch (NumberFormatException ignored) {
					}

				return new ItemStack(mat, 1);
			}
		}
		return null;
	}

	/**
	 * 
	 * @param item
	 * @return
	 */
	public static boolean isValidItem(ItemStack item) {
		return (item != null && !item.getType().equals(Material.AIR));
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean isSameType(ItemStack a, ItemStack b) {
		return a.getType().equals(b.getType());
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	public static boolean equals(ItemStack a, ItemStack b) {

		ItemMeta metaA = a.getItemMeta();
		ItemMeta metaB = b.getItemMeta();

		// Type/dura
		if (!isSameType(a, b))
			return false;

		// Display name
		if (metaA.getDisplayName() != null) {
			if (!metaA.getDisplayName().equals(metaB.getDisplayName()))
				return false;
		}
		else {
			if (metaB.getDisplayName() != null)
				return false;
		}

		// Coloring
		if (metaA instanceof LeatherArmorMeta) {
			if (!(metaB instanceof LeatherArmorMeta))
				return false;
			LeatherArmorMeta colorA = (LeatherArmorMeta) metaA;
			LeatherArmorMeta colorB = (LeatherArmorMeta) metaB;
			if (!colorA.getColor().equals(colorB.getColor()))
				return false;
		}

		// Lore
		if (metaA.getLore() != null && metaA.getLore() != null) {
			for (String lore : metaA.getLore()) {
				if (!metaB.getLore().contains(lore))
					return false;
			}
		}
		else if (!(metaA.getLore() == null && metaB.getLore() == null))
			return false;

		// Enchants
		if (!enchantsEqual(a.getEnchantments(), b.getEnchantments()))
			return false;

		// Books
		if (metaA instanceof BookMeta) {
			if (!(metaB instanceof BookMeta))
				return false;

			BookMeta bookA = (BookMeta) metaA;
			BookMeta bookB = (BookMeta) metaB;

			// Author
			if (bookA.getAuthor() != null) {
				if (!bookA.getAuthor().equals(bookB.getAuthor()))
					return false;
			}

			if (bookA.getTitle() != null) {
				if (!bookA.getTitle().equals(bookB.getTitle()))
					return false;
			}

			// Pages
			if (bookA.getPageCount() != bookB.getPageCount())
				return false;

			for (int page = 0; page < bookA.getPages().size(); page++) {
				String pageContentA = bookA.getPages().get(page);
				if (pageContentA != null) {
					if (!pageContentA.equals(bookB.getPages().get(page)))
						return false;
				}
			}
		}

		// Enchanted books
		if (metaA instanceof EnchantmentStorageMeta) {

			if (!(metaB instanceof EnchantmentStorageMeta))
				return false;

			EnchantmentStorageMeta enchA = (EnchantmentStorageMeta) metaA;
			EnchantmentStorageMeta enchB = (EnchantmentStorageMeta) metaB;

			if (enchA.hasStoredEnchants() != enchB.hasStoredEnchants())
				return false;

			if (!enchantsEqual(enchA.getStoredEnchants(), enchB.getStoredEnchants()))
				return false;

		}

		// Skulls
		if (metaA instanceof SkullMeta) {
			if (!(metaB instanceof SkullMeta))
				return false;

			SkullMeta skullA = (SkullMeta) metaA;
			SkullMeta skullB = (SkullMeta) metaB;

			if (skullA.hasOwner() != skullB.hasOwner())
				return false;

			return skullA.hasOwner()
					&& skullA.getOwningPlayer().getUniqueId().equals(skullB.getOwningPlayer().getUniqueId());
		}

		// Potions
		if (metaA instanceof PotionMeta) {
			if (!(metaB instanceof PotionMeta))
				return false;

			PotionMeta potA = (PotionMeta) metaA;
			PotionMeta potB = (PotionMeta) metaB;

			for (int c = 0; c < potA.getCustomEffects().size(); c++) {
				PotionEffect e = potA.getCustomEffects().get(c);
				if (!e.equals(potB.getCustomEffects().get(c)))
					return true;
			}
		}

		// Fireworks
		if (metaA instanceof FireworkMeta) {
			if (!(metaB instanceof FireworkMeta))
				return false;

			FireworkMeta fwA = (FireworkMeta) metaA;
			FireworkMeta fwB = (FireworkMeta) metaB;

			if (fwA.getPower() != fwB.getPower())
				return false;

			for (int e = 0; e < fwA.getEffects().size(); e++) {
				if (!fwA.getEffects().get(e).equals(fwB.getEffects().get(e)))
					return false;
			}
		}

		// Firework Effects
		if (metaA instanceof FireworkEffectMeta) {
			if (!(metaB instanceof FireworkEffectMeta))
				return false;

			FireworkEffectMeta fwA = (FireworkEffectMeta) metaA;
			FireworkEffectMeta fwB = (FireworkEffectMeta) metaB;

			FireworkEffect effectA = fwA.getEffect();
			FireworkEffect effectB = fwB.getEffect();

			if (!effectA.getType().equals(effectB.getType()))
				return false;

			if (effectA.getColors().size() != effectB.getColors().size())
				return false;

			// Colors
			for (int c = 0; c < effectA.getColors().size(); c++) {
				if (!effectA.getColors().get(c).equals(effectB.getColors().get(c)))
					return false;
			}

			if (effectA.getFadeColors().size() != effectB.getFadeColors().size())
				return false;

			// Fade colors
			for (int c = 0; c < effectA.getFadeColors().size(); c++) {
				if (!effectA.getFadeColors().get(c).equals(effectB.getFadeColors().get(c)))
					return false;
			}

			if (effectA.hasFlicker() != effectB.hasFlicker())
				return false;
			return effectA.hasTrail() == effectB.hasTrail();

		}

		return true;

	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 */
	protected static boolean enchantsEqual(Map<Enchantment, Integer> a, Map<Enchantment, Integer> b) {

		// Enchants
		if (a.size() != b.size())
			return false;

		// Match enchantments and levels
		for (Entry<Enchantment, Integer> entryA : a.entrySet()) {

			// If enchantment not present
			if (!b.containsKey(entryA.getKey()))
				return false;

			// If levels don't match
			if (!b.get(entryA.getKey()).equals(entryA.getValue()))
				return false;

		}

		return true;

	}

	/**
	 * @todo this is buggy, wth?
	 * @return
	 */
	public static String getUsedDurabilityPercentage(ItemStack item) {

		short dura = (short) getItemDamage(item);
		short max_dura = item.getType().getMaxDurability();
		if (dura > 0 && max_dura > 0 && dura != max_dura) {
			double diff = ((dura / max_dura) * 100);
			if (diff > 0) {
				return Math.floor(diff) + "%";
			}
		}

		return "";
	}

	/**
	 * Returns the durability remaining
	 * 
	 * @return
	 */
	public static String getDurabilityPercentage(ItemStack item) {

		short dura = (short) getItemDamage(item);
		short max_dura = item.getType().getMaxDurability();
		if (dura > 0 && max_dura > 0 && dura != max_dura) {
			double diff = max_dura - dura;
			diff = ((diff / max_dura) * 100);
			if (diff > 0) {
				return Math.floor(diff) + "%";
			}
			return "0%";
		}

		return "";
	}

	/**
	 * Returns a proper full name for an item, which includes meta content as well.
	 * 
	 * @return string
	 */
	public static String getItemFullNiceName(ItemStack item) {

		StringBuilder item_name = new StringBuilder(item.getType().name().toLowerCase(Locale.ENGLISH).replace('_', ' '));

		ItemMeta meta = null;

		if (item.hasItemMeta()) {
			meta = item.getItemMeta();
		}

		// Leather Coloring
		if (meta instanceof LeatherArmorMeta) {
			LeatherArmorMeta lam = (LeatherArmorMeta) meta;
			if (lam.getColor() != Bukkit.getItemFactory().getDefaultLeatherColor()) {
				item_name.append(" dyed");
			}
		}

		// Skull Owner
		else if (meta instanceof SkullMeta) {
			SkullMeta skull = (SkullMeta) meta;
			if (skull.hasOwner()) {
				item_name.append(skull.getOwningPlayer().getName()).append("'s ");
			}
		}

		// Written books
		if (meta instanceof BookMeta) {
			BookMeta book = (BookMeta) meta;
			item_name.append(" '").append(book.getTitle()).append("' by ").append(book.getAuthor());
		}

		// Enchanted books
		else if (meta instanceof EnchantmentStorageMeta) {
			EnchantmentStorageMeta bookEnchantments = (EnchantmentStorageMeta) meta;
			if (bookEnchantments.hasStoredEnchants()) {
				int i = 1;
				Map<Enchantment, Integer> enchs = bookEnchantments.getStoredEnchants();
				if (enchs.size() > 0) {
					item_name.append(" with");
					for (Map.Entry<Enchantment, Integer> ench : enchs.entrySet()) {
						item_name.append(" ").append(EnchantmentUtils.getClientSideEnchantmentName(ench.getKey(), ench.getValue()));
						item_name.append(i < enchs.size() ? ", " : "");
						i++;
					}
				}
			}
		}

		// Enchantments
		int i = 1;
		Map<Enchantment, Integer> enchs = item.getEnchantments();
		if (enchs.size() > 0) {
			item_name.append(" with");
			for (Map.Entry<Enchantment, Integer> ench : enchs.entrySet()) {
				item_name.append(" ").append(EnchantmentUtils.getClientSideEnchantmentName(ench.getKey(), ench.getValue()));
				item_name.append(i < enchs.size() ? ", " : "");
				i++;
			}
		}

		// Fireworks
		if (meta instanceof FireworkEffectMeta) {
			FireworkEffectMeta fireworkMeta = (FireworkEffectMeta) meta;
			if (fireworkMeta.hasEffect()) {
				FireworkEffect effect = fireworkMeta.getEffect();
				if (!effect.getColors().isEmpty()) {
					item_name.append(" ").append(effect.getColors().size()).append(" colors");
					// int[] effectColors = new int[ effect.getColors().size() ];
					// for (Color effectColor : effect.getColors()){
					//// item_name += effectColor.
					// }
				}
				if (!effect.getFadeColors().isEmpty()) {
					item_name.append(" ").append(effect.getFadeColors().size()).append(" fade colors");
					// int[] fadeColors = new int[ effect.getColors().size() ];
					// for (Color fadeColor : effect.getFadeColors()){
					//// item_name += fadeColor.asRGB();
					// };
				}
				if (effect.hasFlicker()) {
					item_name.append(" flickering");
				}
				if (effect.hasTrail()) {
					item_name.append(" with trail");
				}
			}
		}

		// Custom item names
		if (meta != null) {
			// TODO: API fail here, report and check later
			if (meta.hasDisplayName() && meta.getDisplayName().length() > 0) {
				item_name.append(" named \"").append(meta.getDisplayName()).append("\"");
			}
		}

		return item_name.toString();

	}

	/**
	 * Returns true if an item uses its damage value for something other than
	 * durability.
	 *
	 * @return
	 */
	// THANK YOU 1.13!
	public static boolean dataValueUsedForSubitems(Material material) {
		return false;
	}

	/**
	 * Determines if an itemstack can be stacked. Maz stack size, meta data, and
	 * more taken into account.
	 * 
	 * @param item
	 */
	public static boolean canSafelyStack(ItemStack item) {
		// Can't stack
		if (item.getMaxStackSize() == 1) {
			return false;
		}
		// Has meta
		ItemMeta im = item.getItemMeta();
		return !im.hasDisplayName() && !im.hasEnchants() && !im.hasLore();
	}

	/**
	 * Drop an item at a given location.
	 *
	 * @param location The location to drop the item at
	 * @param itemStack The item to drop
	 */
	public static void dropItem(Location location, ItemStack itemStack) {
		location.getWorld().dropItemNaturally(location, itemStack);
	}

	/**
	 * Drop items at a given location.
	 *
	 * @param location The location to drop the items at
	 * @param is The items to drop
	 * @param quantity The amount of items to drop
	 */
	public static void dropItem(Location location, ItemStack is, int quantity) {
		for (int i = 0; i < quantity; i++) {
			dropItem(location, is);
		}
	}
}
