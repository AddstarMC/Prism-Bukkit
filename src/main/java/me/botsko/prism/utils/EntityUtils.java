package me.botsko.prism.utils;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Entity;
import org.bukkit.entity.ExperienceOrb;
import org.bukkit.entity.Item;
import org.bukkit.entity.Player;

import java.util.EnumSet;
import java.util.List;
import java.util.UUID;

public class EntityUtils {

	public static OfflinePlayer offlineOf(String uuidOrName) {
		if (uuidOrName != null) {
			OfflinePlayer result;
			try {
				result = Bukkit.getOfflinePlayer(UUID.fromString(uuidOrName));
			} catch (IllegalArgumentException e) {
				@SuppressWarnings("deprecation")
				OfflinePlayer player = Bukkit.getOfflinePlayer(uuidOrName);
				result = player;
			}

			return result.hasPlayedBefore() ? result : null;
		}

		return null;
	}

	public static UUID uuidOf(String uuidOrName) {
		if (uuidOrName != null) {
			try {
				return UUID.fromString(uuidOrName);
			} catch (IllegalArgumentException e) {
			}

			@SuppressWarnings("deprecation")
			OfflinePlayer player = Bukkit.getOfflinePlayer(uuidOrName);
			return player.getUniqueId();
		}

		return null;
	}

	// TODO: 1.13
	@SuppressWarnings("deprecation")
	public static void sendBlockChange(Player p, Location loc, Material mat, int data) {
		p.sendBlockChange(loc, mat, (byte) data);
	}

	/**
	 * Removes item drops near an entity
	 * 
	 * @param player
	 * @param radius
	 * @return
	 */
	public static int removeNearbyItemDrops(Player player, int radius) {
		int removed = 0;
		List<Entity> nearby = player.getNearbyEntities(radius, radius, radius);
		for (Entity e : nearby) {
			if (e instanceof Item || e instanceof ExperienceOrb) {
				e.remove();
				removed++;
			}
		}
		return removed;
	}

	/**
	 * Whether or not an entity is within a cube radius
	 * 
	 * @param loc1
	 * @param radius
	 * @param loc2
	 * @return
	 */
	public static boolean inCube(Location loc1, int radius, Location loc2) {
		if (loc1 == null || loc2 == null)
			return false;
		return (loc1.getBlockX() + radius > loc2.getBlockX() && loc1.getBlockX() - radius < loc2.getBlockX()
				&& loc1.getBlockY() + radius > loc2.getBlockY() && loc1.getBlockY() - radius < loc2.getBlockY()
				&& loc1.getBlockZ() + radius > loc2.getBlockZ() && loc1.getBlockZ() - radius < loc2.getBlockZ());
	}

	/**
	 * Determines which blocks a player my "co-exist" with.
	 * 
	 * @param m
	 * @return
	 * @todo doesn't bukkit have this already?
	 */
	
	public static boolean playerMayPassThrough(Material m) {
		// Close enough, pray you don't land in a portal
		return m.isTransparent();
		/*switch (m) {
		case AIR:
		case CARROT:
		case DEAD_BUSH:
		case DETECTOR_RAIL:
		case POTATO:
		case SUGAR_CANE:
		case REPEATER:
		case FLOWER_POT:
		case LEVER:
		case TALL_GRASS:
		case MELON_STEM:
		case NETHER_WART:
		case POWERED_RAIL:
		case PUMPKIN_STEM:
		case RAIL:
		case RED_MUSHROOM:
		case REDSTONE:
		case REDSTONE_TORCH:
		case REDSTONE_WIRE:
		case SIGN:
		case SNOW:
		case STONE_PRESSURE_PLATE:
		case TORCH:
		case TRIPWIRE:
		case LILY_PAD:
		case WHEAT:
		case OAK_SAPLING:
		case BIR
			// flowers
			//saplings
			// pressure plates
			// doors
			// standing and wall banners
			// skulls
			return true;
		default:
			return false;
		}*/
	}
}