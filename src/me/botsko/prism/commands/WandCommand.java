package me.botsko.prism.commands;

import org.bukkit.ChatColor;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.wands.InspectorWand;
import me.botsko.prism.wands.ProfileWand;
import me.botsko.prism.wands.RollbackWand;
import me.botsko.prism.wands.Wand;

public class WandCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public WandCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		String type = "i";
		if(call.getArgs().length == 2){
			type = call.getArg(1);
		}
		
		Wand oldwand = null;
		if(plugin.playersWithActiveTools.containsKey(call.getPlayer().getName())){
			// Pull the wand in use
			oldwand = plugin.playersWithActiveTools.get(call.getPlayer().getName());
		}
		
		// Always remove the old one
		plugin.playersWithActiveTools.remove(call.getPlayer().getName());
		
			
		/**
		 * Inspector wand
		 */
		if( type.equalsIgnoreCase("i") ){
			if(oldwand != null){
				// If disabling this one
				if( oldwand instanceof InspectorWand ){
					call.getPlayer().sendMessage( plugin.playerHeaderMsg("Inspection wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
					return;
				}
			}
			InspectorWand wand = new InspectorWand( plugin );
			plugin.playersWithActiveTools.put(call.getPlayer().getName(), wand);
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Inspection wand " + ChatColor.GREEN + "enabled"+ChatColor.WHITE+".") );
		}
		
		/**
		 * Profile wand
		 */
		else if( type.equalsIgnoreCase("p") ){
			if(oldwand != null){
				// If disabling this one
				if( oldwand instanceof ProfileWand ){
					call.getPlayer().sendMessage( plugin.playerHeaderMsg("Profile wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
					return;
				}
			}
			ProfileWand wand = new ProfileWand( plugin );
			plugin.playersWithActiveTools.put(call.getPlayer().getName(), wand);
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Profile wand " + ChatColor.GREEN + "enabled"+ChatColor.WHITE+".") );
		}

		
		/**
		 * Rollback wand
		 */
		else if( type.equalsIgnoreCase("rollback") ){
			if(oldwand != null){
				// If disabling this one
				if( oldwand instanceof RollbackWand ){
					call.getPlayer().sendMessage( plugin.playerHeaderMsg("Rollback wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
					return;
				}
			}
			RollbackWand wand = new RollbackWand( plugin );
			plugin.playersWithActiveTools.put(call.getPlayer().getName(), wand);
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Rollback wand " + ChatColor.GREEN + "enabled"+ChatColor.WHITE+".") );
		}
		
		/**
		 * Restore wand
		 * @todo
		 */
		else if(type.equalsIgnoreCase("rollback")){
		}
		
		/**
		 * Off
		 */
		else if(type.equalsIgnoreCase("off")){
			call.getPlayer().sendMessage( plugin.playerHeaderMsg("Current wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
		}
		
		// Not a valid wand
		else {
			call.getPlayer().sendMessage( plugin.playerError("Invalid wand type. Use /prism ? for help.") );
		}
	}
}