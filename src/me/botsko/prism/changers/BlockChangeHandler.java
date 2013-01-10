package me.botsko.prism.changers;

import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.events.PrismProcessType;
import me.botsko.prism.utils.BlockUtils;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.entity.Player;

public class BlockChangeHandler {
	
	/**
	 * 
	 */
	protected BlockAction b;
	
	/**
	 * Player (for send block previews to)
	 */
	protected Player player;
	
	/**
	 * Whether or not we're previewing or applying
	 */
	protected boolean is_preview;
	
	/**
	 * Block we'll be affected
	 */
	protected Block block;
	
	/**
	 * Are we rolling back or restoring
	 */
	protected PrismProcessType processType;
	
	
	/**
	 * 
	 * @param a
	 */
	public BlockChangeHandler( PrismProcessType processType, Location loc, BlockAction b, Player player, boolean is_preview ){
		
		// Store the block change information
		this.processType = processType;
		this.b = b;
		this.player = player;
		this.is_preview = is_preview;
		block = loc.getWorld().getBlockAt(loc);

	}
	
	
	/**
	 * 
	 */
	public BlockChangeResult applyChange(){
		
		// Rollbacks remove blocks players created, and restore blocks player's removed
		if(processType.equals(PrismProcessType.ROLLBACK)){
			if(b.getType().doesCreateBlock()){
				return remove();
			} else {
				return place();
			}
		}
		
		// Restores break blocks players placed again, and re-place blocks player's have placed before
		if(processType.equals(PrismProcessType.RESTORE)){
			if(b.getType().doesCreateBlock()){
				return place();
			} else {
				return remove();
			}
		}
		return null;
	}
	
	
	/**
	 * Place a block unless something other than air occupies the spot, or if we detect 
	 * a falling block now sits there. This resolves
	 * the issue of falling blocks taking up the space, preventing this rollback.
	 * However, it also means that a rollback *could* interfere with a player-placed
	 * block.
	 */
	public BlockChangeResult place(){
		
		Material m = Material.getMaterial(b.getBlock_id());
		BlockStateChange blockStateChanges = null;
		
		// We're doing a rollback, we need to ensure the location we're replacing doesn't
		// have a new block already.
		if( processType.equals(PrismProcessType.ROLLBACK) && !BlockUtils.isAcceptableForBlockPlace(block) ){
			return new BlockChangeResult( ChangeResultType.SKIPPED, null );
		}
		
		// On the blacklist?
		if( !BlockUtils.mayEverPlace(m) ){
			return new BlockChangeResult( ChangeResultType.SKIPPED, null );
		}
			
		// If it's attachable to the sides or top, we need to delay
		if( (BlockUtils.isSideFaceDetachableMaterial(m) || BlockUtils.isTopFaceDetachableMaterial(m)) && !BlockUtils.isDoor(m)){
			return new BlockChangeResult( ChangeResultType.DEFERRED, null );
		}

		// If we're not in a preview, actually apply this block
		if(!is_preview){
			
			// Capture the block before we change it
			BlockState originalBlock = block.getState();
			
			// Set the material
			block.setTypeId( b.getBlock_id() );
			block.setData( b.getBlock_subid() );
			
			// Capture the new state
			BlockState newBlock = block.getState();
			
			// Store the state change
			blockStateChanges = new BlockStateChange(originalBlock,newBlock);
			
			// If we're rolling back a door, we need to set it properly
			if( m.equals(Material.WOODEN_DOOR) || m.equals(Material.IRON_DOOR_BLOCK) ){
				BlockUtils.properlySetDoor( block, b.getBlock_id(), b.getBlock_subid());
			}
			// Or a bed
			if( m.equals(Material.BED_BLOCK) ){
				BlockUtils.properlySetBed( block, b.getBlock_id(), b.getBlock_subid());
			}
		} else {
			
			// Otherwise, preview it.
			player.sendBlockChange(block.getLocation(), b.getBlock_id(), b.getBlock_subid());
		}
		
		return new BlockChangeResult( ChangeResultType.APPLIED, blockStateChanges );

	}


	/***
	 * 
	 */
	public BlockChangeResult remove(){
		
		BlockStateChange blockStateChanges = null;
		
		// @todo ensure we're not removing a new block that's been placed by someone else
		if(!block.getType().equals(Material.AIR)){
			if(!is_preview){
				
				// Capture the block before we change it
				BlockState originalBlock = block.getState();
				
				// Set
				block.setType(Material.AIR);
				
				// Capture the new state
				BlockState newBlock = block.getState();
				
				// Store the state change
				blockStateChanges = new BlockStateChange(originalBlock,newBlock);
				
			} else {
				player.sendBlockChange(block.getLocation(), Material.AIR, (byte)0);
			}
			return new BlockChangeResult( ChangeResultType.APPLIED, blockStateChanges );
		}
		return new BlockChangeResult( ChangeResultType.SKIPPED, null );
	}
}
