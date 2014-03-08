package me.botsko.prism.parameters;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.commandlibs.Flag;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import java.util.regex.Pattern;

public class FlagParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 */
	@Override
	public String getName() {
		return "Flag";
	}

	
	/**
	 * 
	 */
	@Override
	public String[] getHelp() {
		return new String[0];
	}

	
	/**
	 * 
	 */
	@Override
	public boolean applicable(QueryParameters query, String parameter, CommandSender sender) {
		return Pattern.compile("(-)([^\\s]+)").matcher(parameter).matches();
	}

	
	/**
	 * 
	 */
	@Override
	public void process(QueryParameters query, String parameter, CommandSender sender) {
		String[] flagComponents = parameter.substring(1).split("=");
		Flag flag;
		try {
			flag = Flag.valueOf( flagComponents[0].replace("-", "_").toUpperCase() );
		} catch (IllegalArgumentException ex) {
			throw new IllegalArgumentException("Flag -" + flagComponents[0] + " not found", ex);
		}
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
							query.addSharedPlayer(shareWith);
						} else {
							throw new IllegalArgumentException( "Can't share with " + sharePlayer + ". Are they online?" );
						}
					}
				}
			}
		}
	}

	
	/**
	 * 
	 */
	@Override
	public void defaultTo(QueryParameters query, CommandSender sender) {

	}
}