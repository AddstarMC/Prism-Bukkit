package me.botsko.prism.parameters;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.bukkit.command.CommandSender;

import me.botsko.prism.actionlibs.QueryParameters;

public class KeywordParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 * @return
	 */
	public String getName(){
		return "Keyword";
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
		return Pattern.compile("(k):([^\\s]+)");
	}
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, Matcher input, CommandSender sender ){
		query.setKeyword( input.group(2) );
	}
	
	
	/**
	 * 
	 */
	public void defaultTo( QueryParameters query, CommandSender sender ){
		return;
	}
}