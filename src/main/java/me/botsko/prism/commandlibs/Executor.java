package me.botsko.prism.commandlibs;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubCommand;
import me.botsko.prism.commandlibs.SubHandler;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.plugin.Plugin;

public class Executor implements CommandExecutor {
		
	/**
	 * 
	 */
	public Plugin plugin;
	
	/**
	 * Setting the executor to command mode
	 * allows it to handle all commands the plugin
	 * watches for. Subcommand mode allows it to
	 * watch for commands that are secondary
	 * to the primary command it's assigned to.
	 */
	public String mode = "command";
	
	/**
	 * 
	 */
	protected String perm_base = "";
	
	/**
	 * 
	 */
	public static java.util.Map<String, SubCommand> subcommands = new LinkedHashMap<String, SubCommand>();

	
	/**
	 * 
	 * @param prism
	 */
	public Executor( Plugin plugin, String mode, String perm_base ) {
		this.mode = (mode == null ? "command" : mode);
		this.perm_base = (perm_base == null ? "" : perm_base);
		this.plugin = plugin;
	}

	
	/**
	 * 
	 */
	public boolean onCommand(CommandSender sender, Command cmd, String commandLabel, String[] args) {

		// Set player
		Player player = null;
		if (sender instanceof Player) {
			player = (Player) sender;
		}


		// Find command
		String subcommandName = "about";
		if(mode.equals("subcommand")){
			subcommandName = args[0].toLowerCase();
			// Standardize the args
			String[] new_args = new String[ (args.length - 1) ];
			for(int i = 1; i < args.length; i++){
				new_args[ (i - 1) ] = args[i];
			}
			args = new_args;
		} else {
			subcommandName = cmd.getName();
		}
		

		SubCommand sub = subcommands.get(subcommandName);
		if (sub == null) {
			sender.sendMessage( "Invalid command" );
			return true;
		}
		// Ensure they have permission
		else if ( player != null && !(player.hasPermission( perm_base+".*" ) || player.hasPermission( sub.getPermNode() )) ) {
			sender.sendMessage( "You do not have permission to use this command" );
			return true;
		}
		// Ensure min number of arguments
		else if ( (mode.equals("subcommand") && (args.length - 1 ) < sub.getMinArgs()) || (mode.equals("command") && (args.length ) < sub.getMinArgs()) ) {
			sender.sendMessage( "You're missing arguments for this command" );
			return true;
		}
		// Ensure command allows console
		if(!(sender instanceof Player)){
			if(!sub.isConsoleAllowed()){
				sender.sendMessage( "You must be in-game to use this command" );
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
			if ((has_player || sub.isConsoleAllowed()) && (sender.hasPermission( perm_base+".*" ) || sender.hasPermission( sub.getPermNode() ) )) {
				items.add(sub);
			}
		}
		return items;
	}
}