package me.botsko.prism.parameters;

import java.util.regex.Matcher;

import org.bukkit.command.CommandSender;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.actionlibs.QueryParameters;

public class IdParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, Matcher input, CommandSender sender ){
		
		if( !TypeUtils.isNumeric( input.group(2) ) ){
			throw new IllegalArgumentException("ID must be a number. Use /prism ? for help.");
		}
		query.setId( Integer.parseInt(input.group(2)) );
	}
}