package me.botsko.prism.parameters;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;

public class WorldParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 * @return
	 */
	public String getName(){
		return "World";
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String[] getHelp(){
		return new String[]{};
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Pattern getArgumentPattern(){
		return Pattern.compile("(w):([^\\s]+)");
	}
	
	
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
	
	
	/**
	 * 
	 */
	public void defaultTo( QueryParameters query, CommandSender sender ){
		if( query.getProcessType().equals(PrismProcessType.DELETE) ) return;
		if( sender != null && sender instanceof Player && !query.allowsNoRadius()){
			query.setWorld( ((Player)sender).getWorld().getName() );
		}
	}
}