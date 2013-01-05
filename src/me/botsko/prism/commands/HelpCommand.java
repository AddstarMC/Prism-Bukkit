package me.botsko.prism.commands;

import org.bukkit.ChatColor;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;

public class HelpCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public HelpCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		help(call.getPlayer());
	}
	
	
//	/**
//	 * Display param help
//	 * @param player
//	 */
//	private void helpParams(Player player) {
//		player.sendMessage(plugin.playerHeaderMsg("Prism parameters"));
//		String actions = "";
//		for(ActionType ac : ActionType.values()){
//			actions += ac.name().replace("_", "-").toLowerCase() + ", ";
//		}
//		actions = actions.substring(0, actions.length() - 2);
//		player.sendMessage( plugin.playerHelp("a:[action]", "Restrict a rollback or search to an action. Actions supported: " + actions));
//		player.sendMessage( plugin.playerHelp("r:[radius]", "Restrict a rollback or search to a radius around you.") );
//		player.sendMessage( plugin.playerHelp("w:[world]", "Restrict a rollback or search to a world.") );
//		player.sendMessage( plugin.playerHelp("t:[time]", "Restrict a rollback to a certain amount of time ago. In the format 0s0m0h0d0w") );
//		player.sendMessage( plugin.playerHelp("p:[player]", "Restrict a rollback to a certain player.") );
//		player.sendMessage( plugin.playerHelp("e:[entity]", "Restrict a rollback to a certain entity's name.") );
//		player.sendMessage( plugin.playerHelp("b:[block-id:sub-id]", "Restrict a rollback to a certain block.") );
//		
//		
//	}


	/**
	 * Displays help
	 * @param player
	 */
	protected void help( Player player ){
		
		player.sendMessage( plugin.playerHeaderMsg( ChatColor.GOLD + "--- Basic Usage ---" ) );
		
		player.sendMessage( plugin.playerHelp("i", "Toggles the inspector onto your hand."));
		player.sendMessage( plugin.playerHelp("l (params)", "Search the database"));
		player.sendMessage( plugin.playerHelp("near", "Find all changes nearby."));
//		player.sendMessage( plugin.playerHelp("params", "Lists parameter help."));
		player.sendMessage( plugin.playerHelp("preview rollback (params)", "Preview a rollback."));
		player.sendMessage( plugin.playerHelp("preview restore (params)", "Preview a restoration."));
		player.sendMessage( plugin.playerHelp("preview apply", "Applies the last preview."));
		player.sendMessage( plugin.playerHelp("preview cancel", "Cancels the last preview."));
		player.sendMessage( plugin.playerHelp("rollback (params)", "Rollback changes."));
		player.sendMessage( plugin.playerHelp("restore (params)", "Re-applies changes."));
		player.sendMessage( plugin.playerHelp("ex (r)", "Extinguish fires within a (r)adius."));
		player.sendMessage( plugin.playerHelp("drain (r)", "Drain water/lava within a (r)adius."));
		player.sendMessage( plugin.playerHelp("delete (t)", "Purge records before (t)ime."));
		player.sendMessage( plugin.playerHelp("reload", "Reloads config/language files."));
		player.sendMessage( plugin.playerHeaderMsg("http://dhmc.us/wiki/view/prism/"));
		
	}
}