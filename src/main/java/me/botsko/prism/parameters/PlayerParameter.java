package me.botsko.prism.parameters;

import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import org.bukkit.command.CommandSender;

import java.util.regex.Pattern;

public class PlayerParameter extends SimplePrismParameterHandler {
	
	
	/**
	 * 
	 */
	public PlayerParameter() {
		super("Player", Pattern.compile("[~|!]?[\\w,]+"), "p");
	}

	
	/**
	 * 
	 */
	public void process(QueryParameters query, String alias, String input, CommandSender sender) {
		String[] playerNames = input.split(",");
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