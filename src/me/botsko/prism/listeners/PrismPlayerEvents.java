package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.CommandAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.wands.Wand;

import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.player.PlayerCommandPreprocessEvent;
import org.bukkit.event.player.PlayerDropItemEvent;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.event.player.PlayerPickupItemEvent;

public class PrismPlayerEvents implements Listener {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public PrismPlayerEvents( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	 /**
     * Log command use
     * @param event
     */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onCommandPreprocess(PlayerCommandPreprocessEvent event) {
		Player player = event.getPlayer();
        plugin.actionsRecorder.addToQueue( new CommandAction(ActionType.PLAYER_COMMAND, event.getMessage(), player.getLocation(), player.getName()) );
    }
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerDropItem(PlayerDropItemEvent event) {
		plugin.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_DROP, event.getItemDrop().getItemStack(), event.getPlayer().getLocation(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerPickupItem(PlayerPickupItemEvent event) {
		plugin.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_PICKUP, event.getItem().getItemStack(), event.getPlayer().getLocation(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.HIGHEST)
	public void onPlayerInteract(PlayerInteractEvent event) {
		
		Player player = event.getPlayer();
		Block block = event.getClickedBlock();

		// Are they using a wand?
		if(plugin.playersWithActiveTools.containsKey(player.getName())){

			// Pull the wand in use
			Wand wand = plugin.playersWithActiveTools.get(player.getName());
			if(wand != null){

				// Left click is for current block
				if (event.getAction() == Action.LEFT_CLICK_BLOCK) {
					wand.playerLeftClick( player, block );
				}
				// Right click is for relative block on blockface
				if (event.getAction() == Action.RIGHT_CLICK_BLOCK) {
					block = block.getRelative(event.getBlockFace());
					wand.playerRightClick( player, block );
				}
			}
			
			// Always cancel
			event.setCancelled(true);
			
		} else {
			
			// Doors, buttons, containers, etc may only be opened with a right-click as of 1.4
			if (block != null && event.getAction() == Action.RIGHT_CLICK_BLOCK){

				switch (block.getType()){
					case FURNACE:
					case DISPENSER:
					case CHEST:
					case ENDER_CHEST:
					case ANVIL:
						plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.CONTAINER_ACCESS, block, player.getName()) );
						break;
					case WOODEN_DOOR:
					case TRAP_DOOR:
					case FENCE_GATE:
					case LEVER:
					case STONE_BUTTON:
					case WOOD_BUTTON:
						plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.BLOCK_USE, block, player.getName()) );
						break;
					default:
						break;
				}
			} 
		} if (block != null && event.getAction() == Action.PHYSICAL){
			if(block.getType() == Material.SOIL){ // They are stepping on soil
				plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.CROP_TRAMPLE, block.getRelative(BlockFace.UP), player.getName()) );
			}
		}
	}
}
