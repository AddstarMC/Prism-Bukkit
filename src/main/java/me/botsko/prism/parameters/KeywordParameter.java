package me.botsko.prism.parameters;

import org.bukkit.command.CommandSender;

import me.botsko.prism.actionlibs.QueryParameters;

public class KeywordParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, String input, CommandSender sender ){
		query.setKeyword( input );
	}
}