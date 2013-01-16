package me.botsko.prism.actions;

import org.bukkit.Location;
import org.bukkit.block.Block;

public class BlockShiftAction extends GenericAction {
	
	public class BlockShiftActionData {
		public int block_id;
		public byte block_subid;
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
	public BlockShiftAction( ActionType action_type, Block from, Location to, String player ){
		
		super(action_type, player);
		
		// Build an object for the specific details of this action
		actionData = new BlockShiftActionData();
		
		// Store information for the action
		if(from != null){
			this.block = from;
			actionData.block_id = block.getTypeId();
			actionData.block_subid = block.getData();
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
	 */
	public int getBlock_id(){
		return actionData.block_id;
	}
	
	
	/**
	 * 
	 */
	public byte getBlock_subid(){
		return actionData.block_subid;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		return this.materialAliases.getItemStackAliasById(actionData.block_id, actionData.block_subid) + " from " + actionData.x + " " + actionData.z;
	}
}