package me.botsko.prism.commands;

import org.bukkit.ChatColor;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.settings.Settings;

public class SetmyCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public SetmyCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		String setType = null;
		if(call.getArgs().length >= 2){
			setType = call.getArg(1);
		}
		
			
		/**
		 * Inspector wand
		 */
		if( setType.equalsIgnoreCase("wand") ){
			
			// Ensure it's enabled
			if( !plugin.getConfig().getBoolean("prism.wands.allow-user-override") ){
				call.getPlayer().sendMessage( plugin.messenger.playerError("Sorry, but personalizing the wand is currently not allowed.") );
			}
			
			// Check for any wand permissions. @todo There should be some central way to handle this - some way to centralize it at least
			if( !call.getPlayer().hasPermission("prism.rollback") 
				&& !call.getPlayer().hasPermission("prism.restore")
				&& !call.getPlayer().hasPermission("prism.wand.*")
				&& !call.getPlayer().hasPermission("prism.wand.inspect")
				&& !call.getPlayer().hasPermission("prism.wand.profile")
				&& !call.getPlayer().hasPermission("prism.wand.rollback")
				&& !call.getPlayer().hasPermission("prism.wand.restore")){
				call.getPlayer().sendMessage( plugin.messenger.playerError("You do not have permission for this.") );
				return;
			}
			
			String setSubType = null;
			if(call.getArgs().length >= 3){
				setSubType = call.getArg(2);
			}
			
			
			/**
			 * Set your custom wand mode to "hand", "item", or "block"
			 */
			if( setSubType.equalsIgnoreCase("mode") ){
				
				String setWandMode = null;
				if(call.getArgs().length >= 4){
					setWandMode = call.getArg(3);
				}
				if( setWandMode != null && (setWandMode.equals("hand") || setWandMode.equals("item") || setWandMode.equals("block")) ){
					Settings.saveSetting("wand.mode", setWandMode, call.getPlayer());
					call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("Changed your personal wand to " + ChatColor.GREEN + setWandMode + ChatColor.WHITE + " mode.") );
					return;
				}
				call.getPlayer().sendMessage( plugin.messenger.playerError("Invalid arguments. Use /prism ? for help.") );
				return;
			}
			
			
			/**
			 * Set your custom wand item for either "item"
			 * or "block" modes
			 */
			if( setSubType.equalsIgnoreCase("item") ){
				String setWandItem = null;
				if(call.getArgs().length >= 4){
					setWandItem = call.getArg(3);
				}
				if( setWandItem != null && setWandItem.contains(":") ){

					int item_id = -1;
					byte item_subid = 0;
					
					String[] itemIds = setWandItem.split(":");
					if(itemIds.length == 2){
						item_id = Integer.parseInt(itemIds[0]);
						item_subid = Byte.parseByte(itemIds[1]);
					}
					if( item_id > -1 ){
						String itemName = plugin.getItems().getItemStackAliasById(item_id, item_subid);
						if( itemName != null ){
							Settings.saveSetting("wand.item", setWandItem, call.getPlayer());
							call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("Changed your personal wand item to " + ChatColor.GREEN + itemName + ChatColor.WHITE + ".") );
							return;
						}
					}
				}
				call.getPlayer().sendMessage( plugin.messenger.playerError("Invalid arguments. Use /prism ? for help.") );
				return;
			}
			
			/**
			 * Reset all custom wand configs
			 */
			if( setSubType.equalsIgnoreCase("reset") ){
				Settings.deleteSetting( "wand.item", call.getPlayer() );
				Settings.deleteSetting( "wand.mode", call.getPlayer() );
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("Your personal wand settings have been reset to server defaults.") );
				return;
			}
			
		} else {
			call.getPlayer().sendMessage( plugin.messenger.playerError("Invalid arguments. Use /prism ? for help.") );
		}
	}
}