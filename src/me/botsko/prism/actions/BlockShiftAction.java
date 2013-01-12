package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import org.bukkit.Location;
import org.bukkit.block.Block;

public class BlockShiftAction extends GenericAction {
	
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
		
		// Build an object for the specific details of this action
		actionData = new BlockShiftActionData();
		
		// Store information for the action
		if(action_type != null){
			this.type = action_type;
		}
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
		if(player != null){
			this.player_name = player;
		}
		if(action_time == null){
			java.util.Date date= new java.util.Date();
			action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
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