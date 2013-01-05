package me.botsko.prism.appliers;

import java.util.ArrayList;
import java.util.List;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.Sign;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.SignAction;
import me.botsko.prism.utils.BlockUtils;

public class Restore extends Preview {
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Restore( Prism plugin, Player player, List<Action> results, QueryParameters parameters ) {
		this.plugin = plugin;
		this.player = player;
		this.results = results;
		this.parameters = parameters;
	}
	
	
	/**
	 * Set preview move and then do a rollback
	 * @return
	 */
	public ApplierResult preview(){
		is_preview = true;
		return apply();
	}
	
	
	/**
	 * 
	 */
	public ApplierResult apply(){
		
		ArrayList<String> responses = new ArrayList<String>();
		int restored_count = 0, skipped_block_count = 0;
		ArrayList<Undo> undo = new ArrayList<Undo>();
		
		if(!results.isEmpty()){
			
			for(Action a : results){
				
				// No sense in trying to restore
				// when the type doesn't support it.
				if(!a.getType().isCanRestore()){
					continue;
				}
				
				World world = plugin.getServer().getWorld(a.getWorld_name());
				
				//Get some data from the entry
				Location loc = new Location(world, a.getX(), a.getY(), a.getZ());
				
				/**
				 * Restore block changes
				 */
				if( a instanceof BlockAction ){
					
					BlockAction b = (BlockAction) a;
					Block block = world.getBlockAt(loc);
					
					if(is_preview){
						// Record the change temporarily so we can cancel
						// and update the client
						Undo u = new Undo( block );
						undo.add(u);
					}
					
					// If the block was placed, we must replace it
					if(a.getType().doesCreateBlock()){
						if( BlockUtils.isAcceptableForBlockPlace(block) ){
							
							if(!BlockUtils.mayEverPlace(Material.getMaterial(b.getBlock_id()))){
								skipped_block_count++;
								continue;
							}
							
							if(!is_preview){
								block.setTypeId( b.getBlock_id() );
								block.setData( b.getBlock_subid() );
							} else {
								player.sendBlockChange(block.getLocation(), b.getBlock_id(), b.getBlock_subid());
							}
							
							restored_count++;
						}
					} else {
						
						/**
						 * Again remove the block that was removed
						 */
						if(!block.getType().equals(Material.AIR)){
							if(!is_preview){
								block.setType(Material.AIR);
							} else {
								player.sendBlockChange(block.getLocation(), Material.AIR, (byte)0);
							}
							restored_count++;
						}
					}
				}
				
				
				/**
				 * Rollback sign actions
				 */
				if( a instanceof SignAction ){
					
					SignAction b = (SignAction) a;
					Block block = world.getBlockAt(loc);
					
					// Ensure a sign exists there (and no other block)
					if( block.getType().equals(Material.AIR) || block.getType().equals(Material.SIGN) || block.getType().equals(Material.WALL_SIGN) ){
						
						if( block.getType().equals(Material.AIR) ){
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
						restored_count++;
					}
				}
			}
			
			// Build the results message
			if(!is_preview){
				
				// Build the results message
				String msg = restored_count + " events restored.";
				if(skipped_block_count > 0){
					msg += " " + skipped_block_count + " skipped.";
				}
				if(restored_count > 0){
					msg += ChatColor.GRAY + " It's like it was always there.";
				}
				responses.add( plugin.playerHeaderMsg( msg ) );
				
			} else {
			
				// Build the results message
				String msg = restored_count + " planned restorations.";
				if(skipped_block_count > 0){
					msg += " " + skipped_block_count + " skipped.";
				}
				if(restored_count > 0){
					msg += ChatColor.GRAY + " Use /prism preview apply to confirm this restore.";
				}
				player.sendMessage( plugin.playerHeaderMsg( msg ) );
				
				// Let me know there's no need to cancel/apply
				if(restored_count == 0){
					player.sendMessage( plugin.playerHeaderMsg( ChatColor.GRAY + "Nothing to restore, preview canceled for you." ) );
				}
			}
		} else {
			responses.add(plugin.playerError( "Nothing found to restore. Try using /prism l (args) first." ));
		}
		return new ApplierResult( is_preview, restored_count, skipped_block_count, undo, responses );
	}
}