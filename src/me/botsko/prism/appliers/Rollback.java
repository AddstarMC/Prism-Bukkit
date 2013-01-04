package me.botsko.prism.appliers;

import java.util.HashMap;
import java.util.List;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.Chest;
import org.bukkit.block.Sign;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.EntityAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.actions.SignAction;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.EntityUtils;

public class Rollback extends Applier {

	
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
	 */
	private QueryParameters parameters;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Rollback( Prism plugin, Player player, List<Action> results, QueryParameters parameters ) {
		this.plugin = plugin;
		this.player = player;
		this.results = results;
		this.parameters = parameters;
	}
	
	
	/**
	 * 
	 */
	public void rollback(){
		
		// Remove any fire at this location
		if(plugin.getConfig().getBoolean("prism.appliers.remove-fire-on-rollback") && parameters.getAction_type() != null && parameters.getAction_type().contains("block-burn")){
			int fires_ext = BlockUtils.extinguish(player.getLocation(),parameters.getRadius());
			if(fires_ext > 0){
				player.sendMessage( plugin.playerHeaderMsg("Extinguishing fire!" + ChatColor.GRAY + " Like a boss.") );
			}
		}
		
		// Remove item drops in this radius
		if(plugin.getConfig().getBoolean("prism.appliers.remove-drops-on-rollback") && parameters.getAction_type() != null && parameters.getAction_type().contains("explode")){
			int removed = EntityUtils.removeNearbyItemDrops(player, parameters.getRadius());
			if(removed > 0){
				player.sendMessage( plugin.playerHeaderMsg("Removed " + removed + " drops in affected area." + ChatColor.GRAY + " Like a boss.") );
			}
		}
		
		// Rollback blocks
		if(!results.isEmpty()){
			
			// Inform nearby players
			plugin.notifyNearby(player, parameters.getRadius(), player.getDisplayName() + " is performing a rollback nearby. Just so you know.");
			
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
					
					// No sense in trying to rollback
					// when the type doesn't support it.
					if(!a.getType().isCanRollback()){
						continue;
					}
					
					// If the block was placed, we need to remove it
					if(a.getType().doesCreateBlock()){
						// @todo ensure we're not removing a new block that's been placed by someone else
						if(!block.getType().equals(Material.AIR)){
							block.setType(Material.AIR);
							rolled_back_count++;
						}
					} else {
						
						/**
						 * Restore the block that was removed, unless something
						 * other than air occupies the spot.
						 */
						if(block.getType().equals(Material.AIR)){
							
							Material m = Material.getMaterial(b.getBlock_id());
							
							if(!mayEverPlace(m)){
								skipped_block_count++;
								continue;
							}
							
							block.setTypeId( b.getBlock_id() );
							block.setData( b.getBlock_subid() );
							rolled_back_count++;
							
						}
					}
				}
				
				
				/**
				 * Rollback entity kills
				 */
				if( a instanceof EntityAction ){
					
					EntityAction b = (EntityAction) a;
					world.spawnEntity(loc, b.getEntityTypeFromData());
					
					plugin.debug("Rolling back entity " + b.getEntityTypeFromData().getName());
					
					rolled_back_count++;
					
				}
				
				
				/**
				 * Rollback sign actions
				 */
				if( a instanceof SignAction ){
					
					SignAction b = (SignAction) a;
					Block block = world.getBlockAt(loc);
					
					// Ensure a sign exists there (and no other block)
					if(block.getType().equals(Material.AIR)){
						if( !block.getType().equals(Material.SIGN) && !block.getType().equals(Material.WALL_SIGN) ){
							block.setType(Material.WALL_SIGN);
						}
						
						// Set the contents
						Sign s = (Sign)block.getState();
						String[] lines = b.getLines();
						int i = 0;
						if(lines.length > 0){
							for(String line : lines){
								s.setLine(i, line);
								i++;
							}
						}
						s.update();
						rolled_back_count++;
					}
				}
				
				
				/**
				 * Rollback itemstack actions
				 */
				if( a instanceof ItemStackAction ){
					
					ItemStackAction b = (ItemStackAction) a;
					
					Block block = world.getBlockAt(loc);
					if(block.getType().equals(Material.CHEST)){
						Chest chest = (Chest) block.getState();
						
						// If item was removed, put it back.
						if(a.getType().equals(ActionType.ITEM_REMOVE) && plugin.getConfig().getBoolean("prism.appliers.allow_rollback_items_removed_from_container")){
							HashMap<Integer,ItemStack> leftovers = chest.getInventory().addItem( b.getItem() );
							rolled_back_count++;
							if(leftovers.size() > 0){
								plugin.debug("Couldn't rollback items to container, it's full.");
							}
						}
					}
				}
			}
			
			// Build the results message
			String msg = rolled_back_count + " reversals.";
			if(skipped_block_count > 0){
				msg += " " + skipped_block_count + " skipped.";
			}
			if(rolled_back_count > 0){
				msg += ChatColor.GRAY + " It's like it never happened.";
			}
			player.sendMessage( plugin.playerHeaderMsg( msg ) );
			
		} else {
			player.sendMessage( plugin.playerError( "Nothing found to rollback. Try using /prism l (args) first." ) );
		}
	}
}