package me.botsko.prism.actions;

import org.bukkit.Location;
import org.bukkit.block.Block;

public class BlockShiftAction extends GenericAction {
	
	public class BlockShiftActionData {
		public int x;
		public int y;
		public int z;
	}
	
	/**
	 * 
	 */
	protected Block block;
	
	/**
	 * 
	 */
	protected BlockShiftActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public BlockShiftAction( String action_type, Block from, Location to, String player ){
		
		super(action_type, player);
		
		// Build an object for the specific details of this action
		actionData = new BlockShiftActionData();
		
		// Store information for the action
		if(from != null){
			this.block = from;
			this.block_id = block.getTypeId();
			this.block_subid = block.getData();
			actionData.x = from.getX();
			actionData.y = from.getY();
			actionData.z = from.getZ();
			this.world_name = block.getWorld().getName();
			this.x = to.getX();
			this.y = to.getY();
			this.z = to.getZ();
		}
		
		// Set data from current block
		setDataFromObject();
		setObjectFromData();
		
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		setObjectFromData();
	}
	
	
	/**
	 * 
	 */
	protected void setDataFromObject(){
		data = gson.toJson(actionData);
	}
	
	
	/**
	 * 
	 */
	protected void setObjectFromData(){
		if(data != null){
			actionData = gson.fromJson(data, BlockShiftActionData.class);
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		return this.materialAliases.getItemStackAliasById(this.block_id, this.block_subid) + " from " + actionData.x + " " + actionData.z;
	}
}