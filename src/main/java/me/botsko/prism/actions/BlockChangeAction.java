package me.botsko.prism.actions;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.utils.BlockUtils;

import org.bukkit.block.Block;
import org.bukkit.entity.Player;

public class BlockChangeAction extends BlockAction {

	
	/**
	 * 
	 * @return
	 */
	@Override
	public String getNiceName(){
		String name = "";
		if(this.getType().getName().equals("block-fade")){
			name += materialAliases.getItemStackAliasById(this.old_block_id, this.old_block_subid);
		} else {
			name += materialAliases.getItemStackAliasById(this.block_id, this.block_subid);
		}
		return name;
	}
	
	
	/**
	 * 
	 */
	@Override
	public ChangeResult applyRollback( Player player, QueryParameters parameters, boolean is_preview ){
		Block block = getWorld().getBlockAt( getLoc() );
		return placeBlock( player, parameters, is_preview, getType().getName(),getOldBlockId(),getOldBlockSubId(),getBlockId(),getBlockSubId(),block,false );
	}
	
	
	/**
	 * 
	 */
	@Override
	public ChangeResult applyRestore( Player player, QueryParameters parameters, boolean is_preview ){
		Block block = getWorld().getBlockAt( getLoc() );
		return placeBlock( player, parameters, is_preview, getType().getName(),getOldBlockId(),getOldBlockSubId(),getBlockId(),getBlockSubId(),block,false );
	}
	
	
	/**
	 * 
	 */
	@Override
	public ChangeResult applyUndo( Player player, QueryParameters parameters, boolean is_preview ){
		Block block = getWorld().getBlockAt( getLoc() );
		return placeBlock( player, parameters, is_preview, getType().getName(),getOldBlockId(),getOldBlockSubId(),getBlockId(),getBlockSubId(),block,false );
	}
	
	
	/**
	 * 
	 * @param type
	 * @param old_id
	 * @param old_subid
	 * @param new_id
	 * @param new_subid
	 * @param block
	 * @param is_deferred
	 * @return
	 */
	protected ChangeResult placeBlock( Player player, QueryParameters parameters, boolean is_preview, String type, int old_id, byte old_subid, int new_id, byte new_subid, Block block, boolean is_deferred ){
		
		BlockAction b = new BlockAction();
		b.setActionType(type);
		b.setPlugin( plugin );
		b.setWorldName(getWorldName());
		b.setX( getX() );
		b.setY( getY() );
		b.setZ( getZ() );
		if(parameters.getProcessType().equals(PrismProcessType.ROLLBACK)){
			// Run verification for no-overwrite. Only reverse a change
			// if the opposite state is what's present now.
			// We skip this check because if we're in preview mode the block may not
			// have been properly changed yet. https://snowy-evening.com/botsko/prism/302/
			// and https://snowy-evening.com/botsko/prism/258/
			if( BlockUtils.areBlockIdsSameCoreItem(block.getTypeId(), new_id) || is_preview || parameters.hasFlag(Flag.OVERWRITE) ){
				System.out.println("OLD ID: " + old_id);
				b.setBlockId( old_id );
				b.setBlockSubId( old_subid );
				return b.placeBlock( player, parameters, is_preview, block, false );
			} else {
//				plugin.debug("Block change skipped because new id doesn't match what's there now. There now: " + block.getTypeId() + " vs " + new_id);
				return new ChangeResult( ChangeResultType.SKIPPED, null );
			}
		}
		if(parameters.getProcessType().equals(PrismProcessType.RESTORE)){
			// Run verification for no-overwrite. Only reapply a change
			// if the opposite state is what's present now.
			// We skip this check because if we're in preview mode the block may not
			// have been properly changed yet. https://snowy-evening.com/botsko/prism/302/
			// and https://snowy-evening.com/botsko/prism/258/
			if( BlockUtils.areBlockIdsSameCoreItem(block.getTypeId(), old_id) || is_preview || parameters.hasFlag(Flag.OVERWRITE) ){
				b.setBlockId( new_id );
				b.setBlockSubId( new_subid );
				return b.placeBlock( player, parameters, is_preview, block, false );
			} else {
//				plugin.debug("Block change skipped because old id doesn't match what's there now. There now: " + block.getTypeId() + " vs " + old_id);
				return new ChangeResult( ChangeResultType.SKIPPED, null );
			}
		}
		return new ChangeResult( ChangeResultType.SKIPPED, null );
	}
}