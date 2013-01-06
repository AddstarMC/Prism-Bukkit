package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import org.bukkit.block.BlockState;
import org.bukkit.inventory.ItemStack;

public class GrowAction extends GenericAction {
	
	/**
	 * 
	 */
	protected BlockState blockstate;
	
	/**
	 * 
	 */
	protected BlockActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public GrowAction( ActionType action_type, BlockState blockstate, String player ){
		
		// Build an object for the specific details of this action
		actionData = new BlockActionData();
		
		// Store information for the action
		if(action_type != null){
			this.type = action_type;
		}
		if(blockstate != null){
			this.blockstate = blockstate;
			actionData.block_id = blockstate.getTypeId();
			actionData.block_subid = blockstate.getData().getData();
			this.world_name = blockstate.getWorld().getName();
			this.x = blockstate.getLocation().getX();
			this.y = blockstate.getLocation().getY();
			this.z = blockstate.getLocation().getZ();
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
			actionData = gson.fromJson(data, BlockActionData.class);
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
		String name = "block";
		ItemStack i = new ItemStack( getBlock_id(),getBlock_subid());
		name = i.getType().name().toLowerCase().replace("_", " ");
		return name;
	}
}