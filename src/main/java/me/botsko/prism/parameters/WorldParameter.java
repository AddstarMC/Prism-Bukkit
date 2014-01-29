package me.botsko.prism.parameters;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;

public class WorldParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, String input, CommandSender sender ){
		if(input.equalsIgnoreCase("current")){
			if( sender != null && sender instanceof Player ){
				input = ((Player)sender).getWorld().getName();
			} else {
				sender.sendMessage(Prism.messenger.playerError( "Can't use the current world since you're not a player. Using default world." ));
				input = Bukkit.getServer().getWorlds().get(0).getName();
			}
		}
		query.setWorld( input );
	}
}