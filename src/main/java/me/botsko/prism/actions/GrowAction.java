package me.botsko.prism.actions;

import org.bukkit.block.BlockState;

public class GrowAction extends BlockAction {
	
	
	/**
	 * 
	 * @param action_type
	 * @param block_filters
	 * @param player
	 */
	public GrowAction( ActionType action_type, BlockState blockstate, String player ){
		
		super(action_type, null, player);
		
		// Build an object for the specific details of this action
		actionData = new BlockActionData();

		if(blockstate != null){
			actionData.block_id = blockstate.getTypeId();
			actionData.block_subid = blockstate.getData().getData();
			this.world_name = blockstate.getWorld().getName();
			this.x = blockstate.getLocation().getBlockX();
			this.y = blockstate.getLocation().getBlockY();
			this.z = blockstate.getLocation().getBlockZ();
		}
		
		// Set data from current block
		setDataFromObject();
		setObjectFromData();
		
	}
}