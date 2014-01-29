package me.botsko.prism.parameters;

import org.bukkit.command.CommandSender;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;

public class IdParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, String input, CommandSender sender ){
		
		if( !TypeUtils.isNumeric( input ) ){
			throw new IllegalArgumentException(Prism.messenger.playerError("ID must be a number. Use /prism ? for help.") );
		}
		query.setId( Integer.parseInt(input) );
	}
}