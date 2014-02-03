package me.botsko.prism.utils;

import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.appliers.PrismProcessType;

public class MiscUtils {
	
	
	/**
	 * Placing this here so it's easier to share the logic
	 * @param player
	 * @param desiredRadius
	 * @param processType
	 * @param config
	 * @return
	 */
	public static int clampRadius( Player player, int desiredRadius, PrismProcessType processType, FileConfiguration config ){
		
		int radius = 0;
		
		// Safety checks for max lookup radius
		int max_lookup_radius = config.getInt("prism.queries.max-lookup-radius");
		if( max_lookup_radius <= 0 ){
			max_lookup_radius = 5;
			Prism.log("Max lookup radius may not be lower than one. Using safe inputue of five.");
		}
		
		// Safety checks for max applier radius
		int max_applier_radius = config.getInt("prism.queries.max-applier-radius");
		if( max_applier_radius <= 0 ){
			max_applier_radius = 5;
			Prism.log("Max applier radius may not be lower than one. Using safe inputue of five.");
		}
		
		// Does the radius exceed the configured max?
		if( processType.equals(PrismProcessType.LOOKUP) && desiredRadius > max_lookup_radius ){
			// If player does not have permission to override the max
			if ( player != null && !player.hasPermission("prism.override-max-lookup-radius") ){
				return max_lookup_radius;
			}
			return desiredRadius;
		}
		if( !processType.equals(PrismProcessType.LOOKUP) && desiredRadius > max_applier_radius ){
			// If player does not have permission to override the max
			if ( player != null && !player.hasPermission("prism.override-max-applier-radius") ){
				return max_applier_radius;
			}
			return desiredRadius;
		}
		
		return config.getInt("prism.near.default-radius");
		
	}

}
