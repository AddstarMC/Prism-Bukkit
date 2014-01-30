package me.botsko.prism.parameters;

import java.util.regex.Matcher;

import org.bukkit.command.CommandSender;

import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;

public class PlayerParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, Matcher input, CommandSender sender ){
		String[] playerNames = input.group(2).split(",");
		if(playerNames.length > 0){
			for(String playerName : playerNames){
				MatchRule match = MatchRule.INCLUDE;
				if(playerName.startsWith("!")){
					match = MatchRule.EXCLUDE;
					playerName = playerName.replace("!", "");
				}
				else if(playerName.startsWith("~")){
					match = MatchRule.PARTIAL;
					playerName = playerName.replace("~", "");
				}
				query.addPlayerName( playerName, match );
			}
		}
	}
}