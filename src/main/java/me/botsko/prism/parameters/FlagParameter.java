package me.botsko.prism.parameters;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.commandlibs.Flag;
import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class FlagParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 */
	@Override
	public String getName() {
		return "Flag";
	}

	
	/**
	 * 
	 */
	@Override
	public String[] getHelp() {
		return new String[0];
	}

	
	/**
	 * 
	 */
	@Override
	public boolean applicable(String parameter, CommandSender sender) {
		return Pattern.compile("(-)([^\\s]+)?").matcher(parameter).matches();
	}

	
	/**
	 * 
	 */
	@Override
	public void process(QueryParameters query, String parameter, CommandSender sender) {
		String[] flagComponents = parameter.substring(1).split("=");
		Flag flag;
		try {
			flag = Flag.valueOf( flagComponents[0].replace("-", "_").toUpperCase() );
		} catch (IllegalArgumentException ex) {
			throw new IllegalArgumentException("Flag -" + flagComponents[0] + " not found", ex);
		}
		if(!(query.hasFlag(flag))){

			query.addFlag(flag);

			// Flag has a value
			if( flagComponents.length > 1 ){
				if(flag.equals(Flag.PER_PAGE)){
					if(TypeUtils.isNumeric(flagComponents[1])){
						query.setPerPage( Integer.parseInt(flagComponents[1]) );
					} else {
						throw new IllegalArgumentException("Per-page flag value must be a number. Use /prism ? for help.");
					}
				} else if (flag.equals(Flag.SHARE)){
					for(String sharePlayer : flagComponents[1].split(",")){
						if(sharePlayer.equals(sender.getName())){
							throw new IllegalArgumentException("You can't share lookup results with yourself!");
						}
						Player shareWith = Bukkit.getServer().getPlayer(sharePlayer);
						if( shareWith != null ){
							query.addSharedPlayer(shareWith);
						} else {
							throw new IllegalArgumentException( "Can't share with " + sharePlayer + ". Are they online?" );
						}
					}
				}
			}
		}
	}

	
	/**
	 * 
	 */
	@Override
	public void defaultTo(QueryParameters query, CommandSender sender) {

	}

    @Override
    public List<String> tabComplete(String partialParameter, CommandSender sender) {
        String[] flagComponents = partialParameter.substring(1).split("=", 2);
        Flag flag;
        String name = flagComponents[0].replace("-", "_").toUpperCase();
        try {
            flag = Flag.valueOf(name);
        } catch (IllegalArgumentException ex) {
            List<String> completions = new ArrayList<String>();
            for (Flag possibleFlag : Flag.values()) {
                String flagName = possibleFlag.toString();
                if(flagName.startsWith(name)) {
                    completions.add("-" + flagName.replace('_', '-').toLowerCase());
                }
            }
            return completions;
        }

        // Flag has a value
        if (flagComponents.length <= 1) {
            return null;
        }

        String prefix = "-" + flag.toString().replace('_', '-').toLowerCase() + "=";
        if (flag.equals(Flag.SHARE)){
            String value = flagComponents[1];
            int end = value.lastIndexOf(',');
            String partialName = value;
            if (end != -1) {
                partialName = value.substring(end + 1);
                prefix = prefix + value.substring(0, end) + ",";
            }
            partialName = partialName.toLowerCase();
            List<String> completions = new ArrayList<String>();
            for (Player player : Bukkit.getOnlinePlayers()) {
                if(player.getName().toLowerCase().startsWith(partialName))
                    completions.add(prefix + player.getName());
            }
            return completions;
        }
        return null;
    }
}