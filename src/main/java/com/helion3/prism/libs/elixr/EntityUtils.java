package com.helion3.prism.libs.elixr;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.entity.Entity;
import org.bukkit.entity.ExperienceOrb;
import org.bukkit.entity.Item;
import org.bukkit.entity.Player;

import java.util.List;

public class EntityUtils {
	
	
	/**
	 * Removes item drops near an entity
	 * @param player
	 * @param radius
	 * @return
	 */
	public static int removeNearbyItemDrops( Player player, int radius ){
		int removed = 0;
		List<Entity> nearby = player.getNearbyEntities(radius, radius, radius);
		for(Entity e : nearby){
			if(e instanceof Item || e instanceof ExperienceOrb){
                e.remove();
                removed++;
            }
		}
		return removed;
	}

	
	/**
	 * Whether or not an entity is within a cube radius
	 * @param loc1
	 * @param radius
	 * @param loc2
	 * @return
	 */
	public static boolean inCube(Location loc1, int radius, Location loc2) {
		if(loc1 == null || loc2 == null) return false;
		return
				loc1.getBlockX() + radius > loc2.getBlockX()
				&& loc1.getBlockX() - radius < loc2.getBlockX()
				&& loc1.getBlockY() + radius > loc2.getBlockY()
				&& loc1.getBlockY() - radius < loc2.getBlockY()
				&& loc1.getBlockZ() + radius > loc2.getBlockZ()
				&& loc1.getBlockZ() - radius < loc2.getBlockZ()
				;
	}
	
	
	/**
	 * Determines which blocks a player my "co-exist" with.
	 * 
	 * @param m
	 * @return
	 * @todo doesn't bukkit have this already?
	 */
	public static boolean playerMayPassThrough( Material m ){
		switch(m){
			case AIR:
			case CARROT:
			case DEAD_BUSH:
			case DETECTOR_RAIL:
			case POTATO:
			case CROPS:
			case DIODE:
			case DIODE_BLOCK_OFF:
			case DIODE_BLOCK_ON:
			case FIRE:
			case FLOWER_POT:
			case LEVER:
			case LONG_GRASS:
			case MELON_STEM:
			case NETHER_WARTS:
			case POWERED_RAIL:
			case PUMPKIN_STEM:
			case RAILS:
			case RED_MUSHROOM:
			case RED_ROSE:
			case REDSTONE:
			case REDSTONE_TORCH_OFF:
			case REDSTONE_TORCH_ON:
			case REDSTONE_WIRE:
			case SAPLING:
			case SIGN:
			case SIGN_POST:
			case SKULL:
			case SNOW:
			case SUGAR_CANE_BLOCK:
            case STANDING_BANNER:
			case STONE_PLATE:
			case TORCH:
			case TRIPWIRE:
            case WALL_BANNER:
			case WATER_LILY:
			case WHEAT:
			case WOOD_PLATE:
			case WOODEN_DOOR:
			case YELLOW_FLOWER:
				return true;
			default:
				return false;
		}
	}
}