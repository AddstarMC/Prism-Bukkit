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
		
		if(plugin.playersWithActiveTools.containsKey(call.getPlayer().getName())){
			// Pull the wand in use
			Wand wand = plugin.playersWithActiveTools.get(call.getPlayer().getName());
			if(wand != null){
				if( type.equalsIgnoreCase("rollback") && wand instanceof RollbackWand ){
					plugin.playersWithActiveTools.remove(call.getPlayer().getName());
					call.getPlayer().sendMessage( plugin.playerHeaderMsg("Rollback wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
				}
				else if( type.equalsIgnoreCase("i") && wand instanceof InspectorWand ){
					plugin.playersWithActiveTools.remove(call.getPlayer().getName());
					call.getPlayer().sendMessage( plugin.playerHeaderMsg("Inspector wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
				}
				else if( type.equalsIgnoreCase("p") && wand instanceof ProfileWand ){
					plugin.playersWithActiveTools.remove(call.getPlayer().getName());
					call.getPlayer().sendMessage( plugin.playerHeaderMsg("Profile wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
				}
				else {
					call.getPlayer().sendMessage( plugin.playerError("You have another wand active, please disable it first.") );
				}
			}
		} else {
			
			/**
			 * Inspector wand
			 */
			if(type.equalsIgnoreCase("i")){
				InspectorWand wand = new InspectorWand( plugin );
				plugin.playersWithActiveTools.put(call.getPlayer().getName(), wand);
				call.getPlayer().sendMessage( plugin.playerHeaderMsg("Inspection wand " + ChatColor.GREEN + "enabled"+ChatColor.WHITE+".") );
			}
			
			/**
			 * Profile wand
			 */
			else if(type.equalsIgnoreCase("p")){
				ProfileWand wand = new ProfileWand( plugin );
				plugin.playersWithActiveTools.put(call.getPlayer().getName(), wand);
				call.getPlayer().sendMessage( plugin.playerHeaderMsg("Profile wand " + ChatColor.GREEN + "enabled"+ChatColor.WHITE+".") );
			}
			
			/**
			 * Rollback wand
			 */
			else if(type.equalsIgnoreCase("rollback")){
				RollbackWand wand = new RollbackWand( plugin );
				plugin.playersWithActiveTools.put(call.getPlayer().getName(), wand);
				call.getPlayer().sendMessage( plugin.playerHeaderMsg("Rollback wand " + ChatColor.GREEN + "enabled"+ChatColor.WHITE+".") );
			}
			
			/**
			 * Restore wand
			 */
			else if(type.equalsIgnoreCase("rollback")){
			}
			
			// Not a valid wand
			else {
				call.getPlayer().sendMessage( plugin.playerError("Invalid wand type. Use /prism ? for help.") );
			}
		}
	}
}