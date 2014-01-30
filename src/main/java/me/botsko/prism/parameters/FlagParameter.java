package me.botsko.prism.parameters;

import java.util.regex.Matcher;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.commandlibs.Flag;

public class FlagParameter implements PrismParameterHandler {
	
	
	
	public void process( QueryParameters query, Matcher input, CommandSender sender ){
		
		String[] flagComponents = input.group(2).split("=");
		Flag flag = Flag.valueOf( flagComponents[0].replace("-", "_").toUpperCase() );
		if(!(query.hasFlag(flag))){
				
			query.addFlag(flag);
			
			// Flag has a value
			if( flagComponents.length > 1 ){
				if(flag.equals(Flag.PER_PAGE)){
					if(TypeUtils.isNumeric(flagComponents[1])){
						query.setPerPage( Integer.parseInt(flagComponents[1]) );
					} else {
						throw new IllegalArgumentException("Per-page flag value must be a number. Use /prism ? for help.");
					}
				} else if (flag.equals(Flag.SHARE)){
					for(String sharePlayer : flagComponents[1].split(",")){
						if(sharePlayer.equals(sender.getName())){
							throw new IllegalArgumentException("You can't share lookup results with yourself!");
						}
						Player shareWith = Bukkit.getServer().getPlayer(sharePlayer);
						if( shareWith != null ){
							query.addSharedPlayer( (CommandSender)shareWith );
						} else {
							throw new IllegalArgumentException( "Can't share with " + sharePlayer + ". Are they online?" );
						}
					}
				}
			}
		}
	}
}