package me.botsko.prism.parameters;

import java.util.regex.Matcher;

import org.bukkit.command.CommandSender;

import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;

public class EntityParameter implements PrismParameterHandler {
	
	
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
}