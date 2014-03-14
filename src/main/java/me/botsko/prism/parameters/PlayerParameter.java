package me.botsko.prism.parameters;

import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import java.util.ArrayList;
import java.util.List;
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

    @Override
    protected List<String> tabComplete(String alias, String partialParameter, CommandSender sender) {
        String prefix = "";
        String partialName = partialParameter;
        if(partialParameter.startsWith("!") || partialParameter.startsWith("~")) {
            prefix = partialParameter.substring(0, 1);
            partialName = partialParameter.substring(1);
        }
        int end = partialName.lastIndexOf(',');
        if (end != -1) {
            prefix = prefix + partialName.substring(0, end) + ",";
            partialName = partialName.substring(end + 1);
        }
        partialName = partialName.toLowerCase();
        List<String> completions = new ArrayList<String>();
        for (Player player : Bukkit.getOnlinePlayers()) {
            if(player.getName().toLowerCase().startsWith(partialName))
                completions.add(prefix + player.getName());
        }
        return completions;
    }
}