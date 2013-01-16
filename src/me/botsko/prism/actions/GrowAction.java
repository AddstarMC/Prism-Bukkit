package me.botsko.prism.actions;

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
	 * @param block_filters
	 * @param player
	 */
	public GrowAction( ActionType action_type, BlockState blockstate, String player ){
		
		super(action_type, player);
		
		// Build an object for the specific details of this action
		actionData = new BlockActionData();

		if(blockstate != null){
			this.blockstate = blockstate;
			actionData.block_id = blockstate.getTypeId();
			actionData.block_subid = blockstate.getData().getData();
			this.world_name = blockstate.getWorld().getName();
			this.x = blockstate.getLocation().getX();
			this.y = blockstate.getLocation().getY();
			this.z = blockstate.getLocation().getZ();
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