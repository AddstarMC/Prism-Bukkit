package me.botsko.prism.commands;

import org.bukkit.ChatColor;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;

public class ParamsCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public ParamsCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		help(call.getPlayer());
	}
	
	
	/**
	 * Display param help
	 * @param player
	 */
	private void help(Player player) {
		
		player.sendMessage( plugin.playerHeaderMsg( ChatColor.GOLD + "--- Parameters Help ---" ) );

		player.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "a:[action]" + ChatColor.WHITE + " Like 'block-break' (See below for full list). No default."));
		player.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "r:[radius]" + ChatColor.WHITE + " i.e. 20, or 100. Defaults to default-radius defined in config. Use r:global to force an all-world search, for lookups only.") );
		player.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "p:[player]" + ChatColor.WHITE + " Like 'viveleroi'. No default.") );
		player.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "b:[block]" + ChatColor.WHITE + " Like 'grass' or '2' or '2:0'. No default.") );
		player.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "e:[entity]" + ChatColor.WHITE + " Like 'pig'. No default.") );
		player.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "t:[time]" + ChatColor.WHITE + " Events after x long ago. Like 1s(seconds), 20m(minutes), 1h(hour), 7d(days), 2w(weeks). No default.") );
		player.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "w:[world]" + ChatColor.WHITE + " Defaults to your current world.") );
		
		String actions = "";
		int i = 1;
		for(ActionType ac : ActionType.values()){
			if(ac.name().contains("PRISM")) continue;
			actions += ac.getActionShortType() + (i < ActionType.values().length ? ", " : "");
			i++;
		}
		player.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "Action Aliases:" + ChatColor.WHITE + " " + actions) );
		
		
		actions = "";
		i = 1;
		for(ActionType ac : ActionType.values()){
			if(ac.name().contains("PRISM")) continue;
			actions += ac.getActionType() + (i < ActionType.values().length ? ", " : "");
			i++;
		}
		player.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "Full Actions:" + ChatColor.GRAY + " " + actions) );
		
	}
}