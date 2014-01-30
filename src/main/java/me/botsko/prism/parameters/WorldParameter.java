package me.botsko.prism.parameters;

import java.util.regex.Matcher;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;

public class WorldParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, Matcher input, CommandSender sender ){
		String worldName = input.group(2);
		if(worldName.equalsIgnoreCase("current")){
			if( sender != null && sender instanceof Player ){
				worldName = ((Player)sender).getWorld().getName();
			} else {
				sender.sendMessage(Prism.messenger.playerError( "Can't use the current world since you're not a player. Using default world." ));
				worldName = Bukkit.getServer().getWorlds().get(0).getName();
			}
		}
		query.setWorld( worldName );
	}
}