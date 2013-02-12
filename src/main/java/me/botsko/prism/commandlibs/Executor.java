package me.botsko.prism.commandlibs;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import me.botsko.prism.Prism;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

public class Executor implements CommandExecutor {
		
	/**
	 * 
	 */
	public Prism plugin;
	
	/**
	 * 
	 */
	public static java.util.Map<String, SubCommand> subcommands = new LinkedHashMap<String, SubCommand>();

	
	/**
	 * 
	 * @param prism
	 */
	public Executor(Prism prism) {
		this.plugin = prism;
	}

	
	/**
	 * 
	 */
	public boolean onCommand(CommandSender sender, Command cmd, String commandLabel, String[] args) {
		
		plugin.eventTimer.resetEventList();
		plugin.eventTimer.recordTimedEvent("command entered");
		
		Player player = null;
		if (sender instanceof Player) {
			player = (Player) sender;
		}
		
		// If no subcommand given
		if (args.length == 0) {
			args = new String[1];
			args[0] = "about";
		}
		
		// Find subcommand
		String subcommandName = args[0].toLowerCase();
		SubCommand sub = subcommands.get(subcommandName);
		if (sub == null) {
			sender.sendMessage( plugin.msgInvalidSubcommand() );
			return true;
		}
		// Ensure they have permission
		else if ( player != null && !(player.hasPermission( "prism.*" ) || player.hasPermission( sub.getPermNode() )) ) {
			sender.sendMessage( plugin.msgNoPermission() );
			return true;
		}
		// Ensure min number of arguments
		else if ((args.length - 1 ) < sub.getMinArgs()) {
			sender.sendMessage( plugin.msgMissingArguments() );
			return true;
		}
		// Ensure command allows console
		if(!(sender instanceof Player)){
			if(!sub.isConsoleAllowed()){
				sender.sendMessage( plugin.playerError("This command may not be executed via console.") );
				return true;
			}
		}
		
		// Pass along call to handler
		CallInfo call = new CallInfo(sender, player, args);
		sub.getHandler().handle(call);
	
		return true;
		
	}
	
	
	/**
	 * 
	 * @param name
	 * @param permission
	 * @param handler
	 * @return
	 */
	protected SubCommand addSub(String name, String permission, SubHandler handler) {
		SubCommand cmd = new SubCommand(name, permission, handler);
		subcommands.put(name, cmd);
		return cmd;
	}
	
	
	/**
	 * 
	 * @param name
	 * @param permission
	 * @return
	 */
	protected SubCommand addSub(String name, String permission) {
		return addSub(name, permission, null);
	}
	
	
	/**
	 * 
	 * @param sender
	 * @param player
	 * @return
	 */
	protected List<SubCommand> availableCommands(CommandSender sender, Player player) {
		ArrayList<SubCommand> items = new ArrayList<SubCommand>();
		boolean has_player = (player != null);
		for (SubCommand sub: subcommands.values()) {
			if ((has_player || sub.isConsoleAllowed()) && (sender.hasPermission( "prism.*" ) || sender.hasPermission( sub.getPermNode() ) )) {
				items.add(sub);
			}
		}
		return items;
	}
}