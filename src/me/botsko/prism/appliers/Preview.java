package me.botsko.prism.appliers;

import java.util.ArrayList;
import java.util.List;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.BlockAction;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.entity.Player;

public class Preview extends Applier {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	private Player player;
	
	/**
	 * 
	 */
	private List<Action> results;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Preview( Prism plugin, Player player, List<Action> results ) {
		this.plugin = plugin;
		this.player = player;
		this.results = results;
	}
	
	
	/**
	 * 
	 */
	public void preview( QueryParameters parameters ){
		
		if(!results.isEmpty()){
			
			ArrayList<Undo> undo = new ArrayList<Undo>();
			
			int rolled_back_count = 0, skipped_block_count = 0;
			
			for(Action a : results){
				
				World world = plugin.getServer().getWorld(a.getWorld_name());
				
				//Get some data from the entry
				Location loc = new Location(world, a.getX(), a.getY(), a.getZ());
				
				/**
				 * Rollback block changes
				 */
				if( a instanceof BlockAction ){

					BlockAction b = (BlockAction) a;
					Block block = world.getBlockAt(loc);
					
					// Record the change temporarily so we can cancel
					// and update the client
					Undo u = new Undo( block );
					undo.add(u);
					
					
					// If the block was placed, we need to remove it
					// @todo it may not always be air that was replaced. we should log that
					if(a.getType().doesCreateBlock()){
						// @todo ensure we're not removing a new block that's been placed by someone else
						if(!block.getType().equals(Material.AIR)){
							player.sendBlockChange(block.getLocation(), Material.AIR, (byte)0);
							rolled_back_count++;
						}
					} else {
						
						/**
						 * Restore the block that was removed, unless something
						 * other than air occupies the spot.
						 */
						if(block.getType().equals(Material.AIR)){
							
							if(!mayEverPlace(Material.getMaterial(b.getBlock_id()))){
								skipped_block_count++;
								continue;
							}
							
							player.sendBlockChange(block.getLocation(), b.getBlock_id(), b.getBlock_subid());
							rolled_back_count++;
						}
					}
				}
				
				// Only store the preview when we have results to apply
				if(rolled_back_count > 0){
					// Append the preview and blocks temporarily
					PreviewSession ps = new PreviewSession( player, undo, parameters );
					plugin.playerActivePreviews.put(player.getName(), ps);
				}
			}
			
			// Build the results message
			String msg = rolled_back_count + " planned reversals.";
			if(skipped_block_count > 0){
				msg += " " + skipped_block_count + " skipped.";
			}
			if(rolled_back_count > 0){
				msg += ChatColor.GRAY + " Use /prism preview apply to confirm this rollback.";
			}
			player.sendMessage( plugin.playerHeaderMsg( msg ) );
			
			// Let me know there's no need to cancel/apply
			if(rolled_back_count == 0){
				player.sendMessage( plugin.playerHeaderMsg( ChatColor.GRAY + "Nothing to rollback, preview canceled for you." ) );
			}
			
		} else {
			player.sendMessage( plugin.playerError( "Nothing found to preview. Try using /prism l (args) first." ) );
		}
	}
	
	
	/**
	 * 
	 */
	public void cancel_preview(){
		if(plugin.playerActivePreviews.containsKey(player.getName())){
			
			PreviewSession previewSession = plugin.playerActivePreviews.get( player.getName() );
			
			if(!previewSession.getUndo_queue().isEmpty()){
				for(Undo u : previewSession.getUndo_queue()){
					player.sendBlockChange(u.getOriginalBlock().getLocation(), u.getOriginalBlock().getTypeId(), u.getOriginalBlock().getData());
				}
			}
			
			player.sendMessage( plugin.playerHeaderMsg( "Preview canceled." + ChatColor.GRAY + " Please come again!" ) );
			
			plugin.playerActivePreviews.remove( player.getName() );
			
		}
	}
	
	
	/**
	 * 
	 */
	public void apply_preview(){
		if(plugin.playerActivePreviews.containsKey(player.getName())){
			
			// Get preview session
			PreviewSession ps = plugin.playerActivePreviews.get(player.getName());
			
			// Forward as a rollback
			ActionsQuery aq = new ActionsQuery(plugin);
			QueryResult results = aq.lookup( player, ps.getArgs() );
			if(!results.getActionResults().isEmpty()){
				
				player.sendMessage( plugin.playerHeaderMsg("Applying rollback from preview...") );
				Rollback rb = new Rollback( plugin, player, results.getActionResults(), ps.getArgs() );
				rb.rollback();
				
			} else {
				// @todo no results
			}
			
			plugin.playerActivePreviews.remove( player.getName() );
			
		}
	}
}
