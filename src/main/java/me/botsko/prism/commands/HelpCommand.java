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
		
		sender.sendMessage( plugin.messenger.playerHeaderMsg( ChatColor.GOLD + "--- Basic Usage ---" ) );
		
		sender.sendMessage( plugin.messenger.playerHelp("i", "Toggles the inspector wand."));
		sender.sendMessage( plugin.messenger.playerHelp("(l|lookup) (params)", "Search the database"));
		sender.sendMessage( plugin.messenger.playerHelp("near", "Find all changes nearby."));
		sender.sendMessage( plugin.messenger.playerHelp("params", "Lists parameter help."));
		sender.sendMessage( plugin.messenger.playerHelp("(preview|pv) (rollback|rb) (params)", "Preview a rollback."));
		sender.sendMessage( plugin.messenger.playerHelp("(preview|pv) (restore|rs) (params)", "Preview a restoration."));
		sender.sendMessage( plugin.messenger.playerHelp("(preview|pv) apply", "Applies the last preview."));
		sender.sendMessage( plugin.messenger.playerHelp("(preview|pv) cancel", "Cancels the last preview."));
		sender.sendMessage( plugin.messenger.playerHelp("(rollback|rb) (params)", "Rollback changes."));
		sender.sendMessage( plugin.messenger.playerHelp("(restore|rs) (params)", "Re-applies changes."));
		sender.sendMessage( plugin.messenger.playerHelp("(w|wand) profile", "Toggles the profile wand."));
		sender.sendMessage( plugin.messenger.playerHelp("(w|wand) rollback", "Toggles the rollback wand."));
		sender.sendMessage( plugin.messenger.playerHelp("(w|wand) restore", "Toggles the restore wand."));
		sender.sendMessage( plugin.messenger.playerHelp("(w|wand) off", "Disables current wand"));
		sender.sendMessage( plugin.messenger.playerHelp("ex (r)", "Extinguish fires within a (r)adius."));
		sender.sendMessage( plugin.messenger.playerHelp("drain (r)", "Drain water/lava within a (r)adius."));
		sender.sendMessage( plugin.messenger.playerHelp("delete (params)", "Purge records based on (params). No defaults!"));
		sender.sendMessage( plugin.messenger.playerHelp("setmy wand mode (hand|item|block)", "Set your personal wand mode."));
		sender.sendMessage( plugin.messenger.playerHelp("setmy wand item (item id)", "Set your personal wand item/block id:subid."));
		sender.sendMessage( plugin.messenger.playerHelp("resetmy (wand)", "Reset your custom wand settings to server defaults."));
		sender.sendMessage( plugin.messenger.playerHelp("(rp|report) queue", "Display statistics on current queues."));
		sender.sendMessage( plugin.messenger.playerHelp("about", "Prism credits."));
		sender.sendMessage( plugin.messenger.playerHelp("reload", "Reloads config/language files."));
		
	}
}