package me.botsko.prism.utils;

import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Player;

import com.helion3.pste.api.Paste;
import com.helion3.pste.api.PsteApi;
import com.helion3.pste.api.Results;

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
		
		if( desiredRadius <= 0 ){
			return config.getInt("prism.near.default-radius");
		}

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
			// Otherwise non-player
			return desiredRadius;
		}
		else if( !processType.equals(PrismProcessType.LOOKUP) && desiredRadius > max_applier_radius ){
			// If player does not have permission to override the max
			if ( player != null && !player.hasPermission("prism.override-max-applier-radius") ){
				return max_applier_radius;
			}
			// Otherwise non-player
			return desiredRadius;
		} else {
			// Otherwise, the radius is valid and is not exceeding max
			return desiredRadius;
		}
	}
	
	
	/**
	 * 
	 * @param prism
	 * @param results
	 * @return
	 */
	public static String paste_results(Prism prism, String results){
		
		String prismWebUrl = "https://pste.me/";

		if( !prism.getConfig().getBoolean("prism.paste.enable") ){
			return Prism.messenger.playerError("PSTE.me paste bin support is currently disabled by config.");
		}
		
		String apiUsername = prism.getConfig().getString("prism.paste.username");
		String apiKey = prism.getConfig().getString("prism.paste.api-key");
		
		if( !apiKey.matches("[0-9a-z]+") ){
			return Prism.messenger.playerError("Invalid API key.");
		}
		
		PsteApi api = new PsteApi( apiUsername, apiKey );

		try {
			
			Paste paste = new Paste();
			paste.setPaste(results);
			
			Results response = api.createPaste(paste);
			return Prism.messenger.playerSuccess("Successfully pasted results: " + prismWebUrl + "#/" + response.getResults().getSlug());
			
		} catch (Exception up){
			Prism.debug(up.toString());
			return Prism.messenger.playerError("Unable to paste results (" + ChatColor.YELLOW + up.getMessage() + ChatColor.RED + ").");
		}
	}
}