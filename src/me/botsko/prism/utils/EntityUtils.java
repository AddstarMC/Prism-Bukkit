package me.botsko.prism.utils;

import java.util.List;

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
}