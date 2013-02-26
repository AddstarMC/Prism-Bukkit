package me.botsko.prism.commands;

import java.util.HashMap;

import org.bukkit.ChatColor;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.PlayerInventory;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.utils.ItemUtils;
import me.botsko.prism.wands.InspectorWand;
import me.botsko.prism.wands.ProfileWand;
import me.botsko.prism.wands.RestoreWand;
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
		
		
		// Determine mode
		String mode = plugin.getConfig().getString("prism.wands.default-mode");
		
		// Item to use
		int item_id = 0;
		byte item_subid = 0;
		if( mode.equals("item") ){
			String toolKey = plugin.getConfig().getString("prism.wands.default-item-mode-id");
			String[] toolKeys = toolKey.split(":");
			item_id = Integer.parseInt(toolKeys[0]);
			item_subid = Byte.parseByte(toolKeys[1]);
		}
		if( mode.equals("block") ){
			String toolKey = plugin.getConfig().getString("prism.wands.default-block-mode-id");
			String[] toolKeys = toolKey.split(":");
			item_id = Integer.parseInt(toolKeys[0]);
			item_subid = Byte.parseByte(toolKeys[1]);
		}
		
		String wandOn = "";
		if( item_id != 0 ){
			String item_name = plugin.getItems().getItemStackAliasById(item_id, item_subid);
			wandOn += " on a " + item_name;
		}
		
		boolean enabled = false;
		Wand wand = null;
			
		/**
		 * Inspector wand
		 */
		if( type.equalsIgnoreCase("i") ){
			if( !call.getPlayer().hasPermission("prism.lookup") && !call.getPlayer().hasPermission("prism.wand.inspect") ){
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("You do not have permission for this.") );
				return;
			}
			if(oldwand != null && oldwand instanceof InspectorWand){
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("Inspection wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
			} else {
				wand = new InspectorWand( plugin );
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("Inspection wand " + ChatColor.GREEN + "enabled"+ChatColor.WHITE+wandOn+".") );
				enabled = true;
			}
		}
		
		/**
		 * Profile wand
		 */
		else if( type.equalsIgnoreCase("p") ){
			if( !call.getPlayer().hasPermission("prism.lookup") && !call.getPlayer().hasPermission("prism.wand.profile") ){
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("You do not have permission for this.") );
				return;
			}
			if( oldwand != null && oldwand instanceof ProfileWand ){
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("Profile wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
			} else {
				wand = new ProfileWand( plugin );
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("Profile wand " + ChatColor.GREEN + "enabled"+ChatColor.WHITE+wandOn+".") );
				enabled = true;
			}
		}

		
		/**
		 * Rollback wand
		 */
		else if( type.equalsIgnoreCase("rollback") ){
			if( !call.getPlayer().hasPermission("prism.rollback") && !call.getPlayer().hasPermission("prism.wand.rollback") ){
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("You do not have permission for this.") );
				return;
			}
			if(oldwand != null && oldwand instanceof RollbackWand){
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("Rollback wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
			} else {
				wand = new RollbackWand( plugin );
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("Rollback wand " + ChatColor.GREEN + "enabled"+ChatColor.WHITE+wandOn+".") );
				enabled = true;
			}
		}
		
		/**
		 * Restore wand
		 */
		else if(type.equalsIgnoreCase("restore")){
			if( !call.getPlayer().hasPermission("prism.restore") && !call.getPlayer().hasPermission("prism.wand.restore") ){
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("You do not have permission for this.") );
				return;
			}
			// If disabling this one
			if(oldwand != null && oldwand instanceof RestoreWand){
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("Restore wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
			} else {
				wand = new RestoreWand( plugin );
				call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("Restore wand " + ChatColor.GREEN + "enabled"+ChatColor.WHITE+wandOn+".") );
				enabled = true;
			}
		}
		
		/**
		 * Off
		 */
		else if(type.equalsIgnoreCase("off")){
			call.getPlayer().sendMessage( plugin.messenger.playerHeaderMsg("Current wand " + ChatColor.RED + "disabled"+ChatColor.WHITE+".") );
		}
		
		// Not a valid wand
		else {
			call.getPlayer().sendMessage( plugin.messenger.playerError("Invalid wand type. Use /prism ? for help.") );
		}
		
		PlayerInventory inv = call.getPlayer().getInventory();
		if( enabled ){
			
			// Move any existing item to the hand, otherwise give it to them
			if( plugin.getConfig().getBoolean("prism.wands.auto-equip") ){
				if( !ItemUtils.moveItemToHand( inv, item_id, item_subid) ){
					// They don't have the item, so we need to give them an item
					if( ItemUtils.handItemToPlayer( inv,  new ItemStack(item_id,1,item_subid) ) ){
						wand.setItemWasGiven(true);
					} else {
						call.getPlayer().sendMessage( plugin.messenger.playerError("Can't fit the wand item into your inventory.") );
					}
				}
				call.getPlayer().updateInventory();
			}
			// Store
			plugin.playersWithActiveTools.put(call.getPlayer().getName(), wand);
		} else {
			if( oldwand.itemWasGiven() ){
				int itemSlot = -1;
				// Likely is what they're holding
				if( inv.getItemInHand().getTypeId() == item_id && inv.getItemInHand().getDurability() == item_subid ){
					itemSlot = inv.getHeldItemSlot();
				} else {
					itemSlot = ItemUtils.playerInventoryHasItem(inv, item_id, item_subid);
				}
				if( itemSlot > -1 ){
					ItemUtils.subtractAmountFromPlayerInvSlot( inv, itemSlot, 1 );
					call.getPlayer().updateInventory();
				}
			}
		}
	}
}