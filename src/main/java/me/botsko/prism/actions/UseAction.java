package me.botsko.prism.actions;

import org.bukkit.block.Block;

public class UseAction extends GenericAction {
	
	
	/**
	 * 
	 * @param action_type
	 * @param block_filters
	 * @param player
	 */
	public UseAction( ActionType action_type, String item_used, Block block, String player ){
		
		super(action_type, player);
		
		if(block != null){
			this.data = item_used;
			this.world_name = block.getWorld().getName();
			this.x = block.getLocation().getBlockX();
			this.y = block.getLocation().getBlockY();
			this.z = block.getLocation().getBlockZ();
		}
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
	}

	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		return data;
	}
}