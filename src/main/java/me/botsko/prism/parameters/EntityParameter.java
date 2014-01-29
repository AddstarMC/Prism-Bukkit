package me.botsko.prism.parameters;

import org.bukkit.command.CommandSender;

import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;

public class EntityParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, String input, CommandSender sender ){
		String[] entityNames = input.split(",");
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