package me.botsko.prism.commands;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;

public class HelpCommand implements SubHandler {

	protected Prism prism;

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
		
		sender.sendMessage( Prism.messenger.playerHeaderMsg( ChatColor.GOLD + "---" + (prism.getConfig().getBoolean("prism.messages.branding", true) ? null : " Prism:") + " Basic Usage ---" ) );
		
		sender.sendMessage( Prism.messenger.playerHelp("i", "Toggles the inspector wand."));
		sender.sendMessage( Prism.messenger.playerHelp("(l|lookup) (params)", "Search the database"));
		sender.sendMessage( Prism.messenger.playerHelp("near", "Find all changes nearby."));
		sender.sendMessage( Prism.messenger.playerHelp("params", "Lists parameter help."));
		sender.sendMessage( Prism.messenger.playerHelp("actions", "Lists actions."));
		sender.sendMessage( Prism.messenger.playerHelp("flags", "Lists possible applier flags."));
		sender.sendMessage( Prism.messenger.playerHelp("(preview|pv) (rollback|rb) (params)", "Preview a rollback."));
		sender.sendMessage( Prism.messenger.playerHelp("(preview|pv) (restore|rs) (params)", "Preview a restoration."));
		sender.sendMessage( Prism.messenger.playerHelp("(preview|pv) apply", "Applies the last preview."));
		sender.sendMessage( Prism.messenger.playerHelp("(preview|pv) cancel", "Cancels the last preview."));
		sender.sendMessage( Prism.messenger.playerHelp("(rollback|rb) (params)", "Rollback changes."));
		sender.sendMessage( Prism.messenger.playerHelp("(restore|rs) (params)", "Re-applies changes."));
		sender.sendMessage( Prism.messenger.playerHelp("(w|wand) profile", "Toggles the profile wand."));
		sender.sendMessage( Prism.messenger.playerHelp("(w|wand) rollback", "Toggles the rollback wand."));
		sender.sendMessage( Prism.messenger.playerHelp("(w|wand) restore", "Toggles the restore wand."));
		sender.sendMessage( Prism.messenger.playerHelp("(w|wand) off", "Disables current wand"));
		sender.sendMessage( Prism.messenger.playerHelp("ex (r)", "Extinguish fires within a (r)adius."));
		sender.sendMessage( Prism.messenger.playerHelp("drain (r)", "Drain water/lava within a (r)adius."));
		sender.sendMessage( Prism.messenger.playerHelp("delete (params)", "Purge records based on (params). No defaults!"));
		sender.sendMessage( Prism.messenger.playerHelp("setmy wand mode (hand|item|block)", "Set your personal wand mode."));
		sender.sendMessage( Prism.messenger.playerHelp("setmy wand item (item id)", "Set your personal wand item/block id:subid."));
		sender.sendMessage( Prism.messenger.playerHelp("resetmy (wand)", "Reset your custom wand settings to server defaults."));
		sender.sendMessage( Prism.messenger.playerHelp("(rp|report) queue", "Display statistics on current queues."));
		sender.sendMessage( Prism.messenger.playerHelp("about", "Prism credits."));
		sender.sendMessage( Prism.messenger.playerHelp("reload", "Reloads config/language files."));
		
	}
}