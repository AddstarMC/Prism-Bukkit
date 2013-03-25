package me.botsko.prism.commands;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;

public class FlagsCommand implements SubHandler {
	
	
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
		
		sender.sendMessage( Prism.messenger.playerHeaderMsg( ChatColor.GOLD + "--- Flags Help ---" ) );

		sender.sendMessage( Prism.messenger.playerMsg( ChatColor.GRAY + "Flags control how Prism applies a rollback/restore, or formats lookup results." ) );
		sender.sendMessage( Prism.messenger.playerMsg( ChatColor.GRAY + "Use them after parameters, like /pr l p:viveleroi -extended" ) );
		
		sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "-drain" + ChatColor.WHITE + " Drain liquid along with a rollback.") );
		sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "-drain-lava" + ChatColor.WHITE + " Drain only lava along with a rollback.") );
		sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "-drain-water" + ChatColor.WHITE + " Drain only water along with a rollback.") );
		sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "-extended" + ChatColor.WHITE + " Shows the extended lookup results (timestamp, coords, id, etc).") );
		sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "-no-ext" + ChatColor.WHITE + " Do not extinguish fires on burn rollbacks.") );
		sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "-no-group" + ChatColor.WHITE + " Disables grouping of related actions.") );
		sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "-no-item-clear" + ChatColor.WHITE + " Do not clear drops on explosion rollback.") );
		sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "-overwrite" + ChatColor.WHITE + " Forces rb/rs to not skip blocks if something unexpected is at location.") );
		sender.sendMessage( Prism.messenger.playerMsg( ChatColor.LIGHT_PURPLE + "-per-page=#" + ChatColor.WHITE + " Set results per-page for current lookup.") );
		
	}
}