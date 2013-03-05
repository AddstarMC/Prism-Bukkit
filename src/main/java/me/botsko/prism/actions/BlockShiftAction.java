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
	protected BlockShiftActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public void setBlock( Block from ){
		
		// Build an object for the specific details of this action
		actionData = new BlockShiftActionData();
		
		// Store information for the action
		if(from != null){
			this.block_id = from.getTypeId();
			this.block_subid = from.getData();
			actionData.x = from.getX();
			actionData.y = from.getY();
			actionData.z = from.getZ();
			this.world_name = from.getWorld().getName();
			
		}
	}
	
	
	/**
	 * 
	 * @param to
	 */
	public void setToLocation( Location to ){
		if(to != null){
			this.x = to.getBlockX();
			this.y = to.getBlockY();
			this.z = to.getBlockZ();
		}
	}
	
	
	/**
	 * 
	 */
	public void save(){
		data = gson.toJson(actionData);
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
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