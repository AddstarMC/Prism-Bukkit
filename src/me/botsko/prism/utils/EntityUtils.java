package me.botsko.prism.utils;

import java.util.List;

import org.bukkit.Location;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.ExperienceOrb;
import org.bukkit.entity.Item;
import org.bukkit.entity.Player;

public class EntityUtils {
	
	
	/**
	 * Returns whether or not the plugin may spawn an entity,
	 * so that we can avoid including dangerous mobs with an
	 * applier.
	 * 
	 * @param block
	 * @return
	 */
	public static boolean mayEverSpawn( EntityType e ){
		if( e.equals(EntityType.CREEPER) ){
			return false;
		}
		return true;
	}
	
	
	/**
	 * 
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


	public static boolean inCube(Location loc1, int radius, Location loc2) {
		return (
				loc1.getBlockX() + radius > loc2.getBlockX()
				&& loc1.getBlockX() - radius < loc2.getBlockX()
				&& loc1.getBlockY() + radius > loc2.getBlockY()
				&& loc1.getBlockY() - radius < loc2.getBlockY()
				&& loc1.getBlockZ() + radius > loc2.getBlockZ()
				&& loc1.getBlockZ() - radius < loc2.getBlockZ()
				);
	}
}