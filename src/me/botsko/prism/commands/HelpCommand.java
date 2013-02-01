package me.botsko.prism.commands;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

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
		help(call.getSender());
	}
	

	/**
	 * Displays help
	 * @param player
	 */
	protected void help( CommandSender sender ){
		
		sender.sendMessage( plugin.playerHeaderMsg( ChatColor.GOLD + "--- Basic Usage ---" ) );
		
		sender.sendMessage( plugin.playerHelp("i", "Toggles the inspector wand."));
		sender.sendMessage( plugin.playerHelp("(l|lookup) (params)", "Search the database"));
		sender.sendMessage( plugin.playerHelp("near", "Find all changes nearby."));
		sender.sendMessage( plugin.playerHelp("params", "Lists parameter help."));
		sender.sendMessage( plugin.playerHelp("preview (rollback|rb) (params)", "Preview a rollback."));
		sender.sendMessage( plugin.playerHelp("preview (restore|rs) (params)", "Preview a restoration."));
		sender.sendMessage( plugin.playerHelp("preview apply", "Applies the last preview."));
		sender.sendMessage( plugin.playerHelp("preview cancel", "Cancels the last preview."));
		sender.sendMessage( plugin.playerHelp("rollback (params)", "Rollback changes."));
		sender.sendMessage( plugin.playerHelp("restore (params)", "Re-applies changes."));
		sender.sendMessage( plugin.playerHelp("wand rollback", "Toggles the rollback wand."));
		sender.sendMessage( plugin.playerHelp("ex (r)", "Extinguish fires within a (r)adius."));
		sender.sendMessage( plugin.playerHelp("drain (r)", "Drain water/lava within a (r)adius."));
		sender.sendMessage( plugin.playerHelp("delete (params)", "Purge records based on (params). No defaults!"));
		sender.sendMessage( plugin.playerHelp("(rp|report) queue", "Display statistics on current queues."));
		sender.sendMessage( plugin.playerHelp("about", "Basic credits"));
		sender.sendMessage( plugin.playerHelp("reload", "Reloads config/language files."));
		sender.sendMessage( plugin.playerHeaderMsg("http://discover-prism.com"));
		
	}
}