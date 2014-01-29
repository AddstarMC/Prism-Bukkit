package me.botsko.prism.parameters;

import org.bukkit.Bukkit;
import org.bukkit.Chunk;
import org.bukkit.Location;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.file.FileConfiguration;
import org.bukkit.entity.Player;

import me.botsko.elixr.ChunkUtils;
import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.bridge.WorldEditBridge;

public class RadiusParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, String input, CommandSender sender ){
		
		Player player = null;
		if( sender instanceof Player ){
			player = (Player)sender;
		}
		
		FileConfiguration config = Bukkit.getPluginManager().getPlugin("Prism").getConfig();
		
		if(TypeUtils.isNumeric(input) || (input.contains(":") && input.split(":").length >= 1 && TypeUtils.isNumeric(input.split(":")[1]))){
			int radius;
			Location coordsLoc = null;
			if(input.contains(":")){
				radius = Integer.parseInt(input.split(":")[1]);
				String radiusLocOrPlayer = input.split(":")[0];
				if(radiusLocOrPlayer.contains(",")){ // Cooridinates; x,y,z
					String[] coordinates = radiusLocOrPlayer.split(",");
					if(coordinates.length != 3){
						throw new IllegalArgumentException("Couldn't parse the coordinates '" + radiusLocOrPlayer + "'. Perhaps you have more than two commas?");
					}
					for(String s : coordinates){
						if(!TypeUtils.isNumeric(s)){
							throw new IllegalArgumentException("The coordinate '" + s + "' is not a number.");
						}
					}
					coordsLoc = (new Location(
							player != null ? player.getWorld() : 
							(query.getWorld() != null ? Bukkit.getServer().getWorld(query.getWorld()) : 
							Bukkit.getServer().getWorlds().get(0)), 
							Integer.parseInt(coordinates[0]), 
							Integer.parseInt(coordinates[1]), 
							Integer.parseInt(coordinates[2])));
					
				}
				else if(Bukkit.getServer().getPlayer(radiusLocOrPlayer) != null){
					player = Bukkit.getServer().getPlayer(radiusLocOrPlayer);
				} else {
					throw new IllegalArgumentException("Couldn't find the player named '" + radiusLocOrPlayer + "'. Perhaps they are not online or you misspelled their name?");
				}
			} else {
				radius = Integer.parseInt(input);
			}
			if(radius <= 0){
				throw new IllegalArgumentException("Radius must be greater than zero. Or leave it off to use the default. Use /prism ? for help.");
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
			if( query.getProcessType().equals(PrismProcessType.LOOKUP) && radius > max_lookup_radius ){
				// If player does not have permission to override the max
				if ( player != null && !player.hasPermission("prism.override-max-lookup-radius") ){
					radius = max_lookup_radius;
					throw new IllegalArgumentException("Forcing radius to " + radius + " as allowed by config.");
				}
			}
			if( !query.getProcessType().equals(PrismProcessType.LOOKUP) && radius > max_applier_radius ){
				// If player does not have permission to override the max
				if ( player != null && !player.hasPermission("prism.override-max-applier-radius") ){
					radius = max_applier_radius;
					throw new IllegalArgumentException("Forcing radius to " + radius + " as allowed by config.");
				}
			}
			if(radius > 0){
				query.setRadius( radius );
				if(coordsLoc != null){
					query.setMinMaxVectorsFromPlayerLocation(coordsLoc); // We need to set this *after* the radius has been set or it won't work.
				} else {
					if( player != null ){
						query.setMinMaxVectorsFromPlayerLocation( player.getLocation() );
					}
				}
			}
		} else {
			
			// User wants an area inside of a worldedit selection
			if(input.equals("we")){
				
				if (Prism.plugin_worldEdit == null) {
					throw new IllegalArgumentException("This feature is disabled because Prism couldn't find WorldEdit.");
				} else {
				
					// Load a selection from world edit as our area.
					if(player != null){
						Prism prism = (Prism) Bukkit.getPluginManager().getPlugin("Prism");
						query = WorldEditBridge.getSelectedArea(prism, player, query);
					}
				}
			}
			
			// Confine to the chunk
			else if(input.equals("c") || input.equals("chunk")){
					
				if( player == null ){
					throw new IllegalArgumentException("Chunks cannot be used as a radius without a player.");
				}

				Chunk ch = player.getLocation().getChunk();
				query.setWorld(ch.getWorld().getName());
				query.setMinLocation( ChunkUtils.getChunkMinVector( ch ) );
				query.setMaxLocation( ChunkUtils.getChunkMaxVector( ch ) );
				
			}
			
			// User wants no radius, but contained within the current world
			else if(input.equals("world")){
				// Do they have permission to override the global lookup radius
				if( query.getProcessType().equals(PrismProcessType.LOOKUP) && player != null && !player.hasPermission("prism.override-max-lookup-radius") ){
					throw new IllegalArgumentException("You do not have permission to override the max radius.");
				}
				// Do they have permission to override the global applier radius
				if( !query.getProcessType().equals(PrismProcessType.LOOKUP) && player != null && !player.hasPermission("prism.override-max-applier-radius") ){
					throw new IllegalArgumentException("You do not have permission to override the max radius.");
				}
				// Use the world defined in the w: param
				if( query.getWorld() != null ){
					input = query.getWorld();
				}
				// Use the current world
				else if(player != null){
					input = player.getWorld().getName();
				} 
				// Use the default world
				else {
					sender.sendMessage(Prism.messenger.playerError( "Can't use the current world since you're not a player. Using default world." ));
					input = Bukkit.getServer().getWorlds().get(0).getName();
				}
				query.setWorld( input );
				query.setAllowNoRadius(true);
			}
			
			// User has asked for a global radius
			else if(input.equals("global")){
				// Do they have permission to override the global lookup radius
				if( query.getProcessType().equals(PrismProcessType.LOOKUP) && player != null && !player.hasPermission("prism.override-max-lookup-radius") ){
					throw new IllegalArgumentException("You do not have permission to override the max radius.");
				}
				// Do they have permission to override the global applier radius
				if( !query.getProcessType().equals(PrismProcessType.LOOKUP) && player != null && !player.hasPermission("prism.override-max-applier-radius") ){
					throw new IllegalArgumentException("You do not have permission to override the max radius.");
				}
				// Either they have permission or player is null
				query.setWorld(null);
				query.setAllowNoRadius(true);
				
			} else {
				throw new IllegalArgumentException("Radius is invalid. There's a bunch of choice, so use /prism actions for assistance.");
			}
		}
	}
}