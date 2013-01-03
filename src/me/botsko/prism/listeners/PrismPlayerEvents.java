package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.BlockAction;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.player.PlayerInteractEvent;

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
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.HIGHEST)
	public void onPlayerInteract(PlayerInteractEvent event) {
		
		Player player = event.getPlayer();
		Block block = event.getClickedBlock();
		
		// Are they inspecting?
		if(plugin.playersWithActiveTools.contains(player.getName())){
		
			// Player left click on block, run a history search
			if (event.getAction() == Action.LEFT_CLICK_BLOCK) {
				// Leave as-is
			}
			// Player right click on block, get last action
			if (event.getAction() == Action.RIGHT_CLICK_BLOCK) {
				block = block.getRelative(event.getBlockFace());
			}
			if(block != null){
				showBlockHistory(player, block, block.getLocation());
				event.setCancelled(true);
			}
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
		}
	}
	
	
	/**
	 * 
	 * @param player
	 * @param block
	 * @param loc
	 */
	protected void showBlockHistory( Player player, Block block, Location loc ){
		
		plugin.debug("Running history search for " + loc.getBlockX() + " " + loc.getBlockY() + " " + loc.getBlockZ());

		// Build params
		QueryParameters params = new QueryParameters();
		params.setWorld( player.getWorld().getName() );
		params.setLoc(loc);
		
		// Query
		ActionsQuery aq = new ActionsQuery(plugin);
		QueryResult results = aq.lookup( player, params );
		if(!results.getActionResults().isEmpty()){
			for(me.botsko.prism.actions.Action a : results.getActionResults()){
				ActionMessage am = new ActionMessage(a);
				player.sendMessage( plugin.playerHeaderMsg( am.getMessage() ) );
			}
		} else {
			String space_name = (block.getType().equals(Material.AIR) ? "space" : block.getType().toString().toLowerCase() + " block");
			player.sendMessage( plugin.playerError( "No history for this " + space_name + " found." ) );
		}
	}
}
