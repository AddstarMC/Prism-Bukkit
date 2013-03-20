package me.botsko.prism.utils;

import org.bukkit.entity.EntityType;

public class EntityUtils extends me.botsko.elixr.EntityUtils {
	
	
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
}