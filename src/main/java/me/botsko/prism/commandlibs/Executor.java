package me.botsko.prism.commandlibs;

import java.util.LinkedHashMap;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;
import org.bukkit.plugin.Plugin;

public class Executor implements CommandExecutor {
		
	/**
	 * 
	 */
	public final Plugin plugin;
	
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
	public String defaultSubcommand = "default";
	
	/**
	 * 
	 */
	public final java.util.Map<String, SubCommand> subcommands = new LinkedHashMap<String, SubCommand>();

	
	/**
	 * 
	 * @param plugin
     * @param mode
     * @param perm_base
	 */
	public Executor( Plugin plugin, String mode, String perm_base ) {
		this.mode = (mode == null ? "command" : mode);
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
		String subcommandName;
		if(mode.equals("subcommand") && args.length > 0){
			subcommandName = args[0].toLowerCase();
		} else {
			subcommandName = cmd.getName();
		}
		
		String currentMode = mode;

		SubCommand sub = subcommands.get(subcommandName);
		if (sub == null) {
			sub = subcommands.get(defaultSubcommand);
			if (sub == null) {
				sender.sendMessage( "Invalid command" );
				return true;
			} else {
				// The default is used, we must switch back to command mode
				currentMode = "command";
			}
		}
		
//		System.out.print("MODE: " + currentMode);
//		System.out.print("SUBCMD: " + subcommandName);
//		System.out.print("MIN ARGS: " + sub.getMinArgs());
//		System.out.print("ARGS: " + args.length);
		
		// Ensure they have permission
		if ( player != null && !(sub.playerHasPermission(player))) {
			sender.sendMessage( "You do not have permission to use this command" );
			return true;
		}
		// Ensure min number of arguments
		else if ( (currentMode.equals("subcommand") && (args.length - 1 ) < sub.getMinArgs()) || (currentMode.equals("command") && (args.length ) < sub.getMinArgs()) ) {
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
	 * @param commandAliases
	 * @param permissionNodes
	 * @param handler
	 * @return
	 */
	protected SubCommand addSub( String[] commandAliases, String[] permissionNodes, SubHandler handler) {
		SubCommand cmd = new SubCommand(commandAliases, permissionNodes, handler);
		for(String alias : commandAliases){
			subcommands.put(alias, cmd);
		}
		return cmd;
	}
	
	
	/**
	 * 
	 * @param commandAliases
	 * @param permissionNodes
	 * @return
	 */
	protected SubCommand addSub( String[] commandAliases, String[] permissionNodes ) {
		return addSub(commandAliases, permissionNodes, null);
	}
	
	
	/**
	 * 
	 * @param commandAliases
	 * @param permissionNode
	 * @return
	 */
	protected SubCommand addSub( String[] commandAliases, String permissionNode ) {
		return addSub( commandAliases, new String[]{permissionNode}, null);
	}
	
	
	/**
	 * 
	 * @param commandAlias
	 * @param permissionNodes
	 * @return
	 */
	protected SubCommand addSub( String commandAlias, String[] permissionNodes ) {
		return addSub( new String[]{commandAlias}, permissionNodes, null);
	}
	
	
	/**
	 * 
	 * @param commandAlias
	 * @param permissionNode
	 * @return
	 */
	protected SubCommand addSub( String commandAlias, String permissionNode ) {
		return addSub( new String[]{commandAlias}, new String[]{permissionNode}, null);
	}
}