package me.botsko.prism.utils;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;
import java.net.URLEncoder;

import javax.xml.bind.DatatypeConverter;

import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.craftbukkit.libs.com.google.gson.Gson;
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
		
		if(prism.getConfig().get("prism.paste.api_key").equals("")){
			return Prism.messenger.playerError("Sending information to a pastebin is not configured yet.");
		}
		try {
			String params = "paste=" + URLEncoder.encode(results, "UTF-8");
			URL url = new URL("http://pste.me/api/v1/paste/"); // http://pste.me/md/api/
			HttpURLConnection connection = (HttpURLConnection) url.openConnection();
			connection.setRequestMethod("POST");
			connection.setRequestProperty("Content-Type", "application/x-www-form-urlencoded");
			connection.setRequestProperty("Content-Length", Integer.toString(params.getBytes().length));
			connection.setRequestProperty("Content-Language", "en-US");
			connection.setRequestProperty("Authorization", "Basic " + 
						DatatypeConverter.printBase64Binary((prism.getConfig().get("prism.paste.username") + ":" 
						+ prism.getConfig().get("prism.paste.api_key")).getBytes())); // prism.paste. "username"/"api_key"
			connection.setUseCaches(false);
			connection.setDoInput(true);
			connection.setDoOutput(true);
			DataOutputStream wr = new DataOutputStream(connection.getOutputStream());
			wr.writeBytes(params);
			wr.flush();
			wr.close();
			InputStream is = connection.getInputStream();
			BufferedReader rd = new BufferedReader(new InputStreamReader(is));
			String json = rd.readLine();  // one line
			Gson gson = new Gson();
			PasteResponse response = gson.fromJson(json, PasteResponse.class);
			return Prism.messenger.playerSuccess("Successfully pasted results: http://pste.me/" + response.slug + "/");
		} catch (IOException up) {
			Prism.debug(up.toString());
			return Prism.messenger.playerError("Unable to paste results to pste.me (" + ChatColor.YELLOW + up.getMessage() + ChatColor.RED + ").");
		}
	}
	
	
	/**
	 * 
	 * @author botskonet
	 *
	 */
	public static class PasteResponse{
		public String slug = "";
	}
}