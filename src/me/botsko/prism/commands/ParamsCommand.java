package me.botsko.prism.commands;

import java.util.ArrayList;
import java.util.Collections;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

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
		help(call.getSender());
	}
	
	
	/**
	 * Display param help
	 * @param player
	 */
	private void help( CommandSender sender ) {
		
		sender.sendMessage( plugin.playerHeaderMsg( ChatColor.GOLD + "--- Parameters Help ---" ) );

		sender.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "a:[action]" + ChatColor.WHITE + " Like 'block-break' (See below for full list). No default."));
		sender.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "r:[radius]" + ChatColor.WHITE + " i.e. 20, or 100. Defaults to default-radius defined in config. Use r:global to force an all-world search, for lookups only.") );
		sender.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "p:[player]" + ChatColor.WHITE + " Like 'viveleroi'. No default.") );
		sender.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "b:[block]" + ChatColor.WHITE + " Like 'grass' or '2' or '2:0'. No default.") );
		sender.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "e:[entity]" + ChatColor.WHITE + " Like 'pig'. No default.") );
		sender.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "t:[time]" + ChatColor.WHITE + " Events since x long ago. Like 1s(seconds), 20m(minutes), 1h(hour), 7d(days), 2w(weeks). Default based on config.") );
		sender.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "since:[time]" + ChatColor.WHITE + " Events since to x long ago (same as t:).") );
		sender.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "before:[time]" + ChatColor.WHITE + " Events prior to x long ago.") );
		sender.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "w:[world]" + ChatColor.WHITE + " Defaults to your current world.") );
		sender.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "k:[text]" + ChatColor.WHITE + " Keyword search. Mainly for command/chat logging.") );
		sender.sendMessage( plugin.playerMsg( "Prefix action, player, entity names with ! to exclude. Like p:!viveleroi") );
		
		// Build short list
		ArrayList<String> shortNames = new ArrayList<String>();
		for(ActionType ac : ActionType.values()){
			if(ac.name().contains("PRISM")) continue;
			if(shortNames.contains(ac.getActionShortType())) continue;
			shortNames.add( ac.getActionShortType() );
		}
		// Sort alphabetically
		Collections.sort(shortNames);
		
		// Build display of shortname list
		String actions = "";
		int i = 1;
		for(String shortName : shortNames){
			actions += shortName + (i < ActionType.values().length ? ", " : "");
			i++;
		}
		sender.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "Action Aliases:" + ChatColor.WHITE + " " + actions) );
		
		// Build display of full actions
		actions = "";
		i = 1;
		for(ActionType ac : ActionType.values()){
			if(ac.name().contains("PRISM")) continue;
			actions += ac.getActionType() + (i < ActionType.values().length ? ", " : "");
			i++;
		}
		sender.sendMessage( plugin.playerMsg( ChatColor.LIGHT_PURPLE + "Full Actions:" + ChatColor.GRAY + " " + actions) );
		
	}
}