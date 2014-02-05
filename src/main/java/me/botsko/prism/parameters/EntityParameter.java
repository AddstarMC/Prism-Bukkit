package me.botsko.prism.parameters;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.bukkit.command.CommandSender;

import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;

public class EntityParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 * @return
	 */
	public String getName(){
		return "Entity";
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
		return Pattern.compile("(e):([~|!]?[\\w,]+)");
	}
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, Matcher input, CommandSender sender ){
		String[] entityNames = input.group(2).split(",");
		if(entityNames.length > 0){
			for(String entityName : entityNames){
				MatchRule match = MatchRule.INCLUDE;
				if(entityName.startsWith("!")){
					match = MatchRule.EXCLUDE;
				}
				query.addEntity( entityName.replace("!", ""), match );
			}
		}
	}
	
	
	/**
	 * 
	 */
	public void defaultTo( QueryParameters query, CommandSender sender ){
		return;
	}
}