package me.botsko.prism.utils;

import java.io.BufferedReader;
import java.io.DataOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.HttpURLConnection;
import java.net.URL;

import javax.xml.bind.DatatypeConverter;

import org.bukkit.ChatColor;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.craftbukkit.libs.com.google.gson.Gson;
import org.bukkit.entity.Player;
import org.json.simple.JSONObject;

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
	@SuppressWarnings("unchecked")
	public static String paste_results(Prism prism, String results){
		
		String prismApiUrl = "http://dev.pste.me/api/v1/";
		String prismWebUrl = "http://dev.pste.me/";
		
		if( !prism.getConfig().getBoolean("prism.paste.enable") ){
			return Prism.messenger.playerError("PSTE.me paste bin support is currently disabled by config.");
		}
		
		String apiUsername = prism.getConfig().getString("prism.paste.username");
		String apiKey = prism.getConfig().getString("prism.paste.api-key");
		
		if( !apiKey.matches("[0-9a-z]+") ){
			return Prism.messenger.playerError("Invalid API key.");
		}

		try {
			
			JSONObject j = new JSONObject();
			j.put("paste", results);
			String jsonPayload = j.toJSONString();
			
			URL url = new URL( prismApiUrl+"/paste/" );
			
			HttpURLConnection connection = (HttpURLConnection) url.openConnection();
			connection.setRequestMethod("POST");
			connection.setRequestProperty("Content-Length", Integer.toString(jsonPayload.getBytes().length));
			connection.setRequestProperty("Content-Language", "en-US");
			connection.setRequestProperty("Authorization", "Basic " + 
					DatatypeConverter.printBase64Binary((apiUsername + ":" + apiKey).getBytes()));
			connection.setUseCaches(false);
			connection.setDoInput(true);
			connection.setDoOutput(true);
			DataOutputStream wr = new DataOutputStream(connection.getOutputStream());
			wr.writeBytes( j.toJSONString() );
			wr.flush();
			wr.close();
			
			// Read response
			InputStream is = connection.getInputStream();
			BufferedReader rd = new BufferedReader(new InputStreamReader(is));
			String json = readAll(rd);
			
			Gson gson = new Gson();
			Results response = gson.fromJson(json, Results.class);
			return Prism.messenger.playerSuccess("Successfully pasted results: " + prismWebUrl + "#/" + response.getResults().getSlug());
			
		} catch (Exception up){
			Prism.debug(up.toString());
			return Prism.messenger.playerError("Unable to paste results (" + ChatColor.YELLOW + up.getMessage() + ChatColor.RED + ").");
		}
	}
	
	
	/**
	 * 
	 * @param rd
	 * @return
	 * @throws IOException
	 */
	private static String readAll(Reader rd) throws IOException {
	    StringBuilder sb = new StringBuilder();
	    int cp;
	    while ((cp = rd.read()) != -1) {
	      sb.append((char) cp);
	    }
	    return sb.toString();
	}

}