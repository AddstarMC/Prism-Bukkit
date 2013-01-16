package me.botsko.prism.actions;

import org.bukkit.block.BlockFace;
import org.bukkit.entity.Hanging;

public class HangingItemAction extends GenericAction {
	
	public class HangingItemActionData {
		public String type;
		public String direction;
	}
	
	/**
	 * 
	 */
	protected HangingItemActionData actionData;
	

	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public HangingItemAction( ActionType action_type, Hanging hanging, String player ){
		
		super(action_type, player);
		
		actionData = new HangingItemActionData();
		
		if(hanging != null){
			this.actionData.type = hanging.getType().name().toLowerCase();
			this.actionData.direction = hanging.getAttachedFace().name().toLowerCase();
			this.world_name = hanging.getWorld().getName();
			this.x = hanging.getLocation().getBlockX();
			this.y = hanging.getLocation().getBlockY();
			this.z = hanging.getLocation().getBlockZ();
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
			actionData = gson.fromJson(data, HangingItemActionData.class);
		}
	}
	
	/**
	 * 
	 * @return
	 */
	public String getHangingType(){
		return this.actionData.type;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public BlockFace getDirection(){
		if(actionData.direction != null){
			return BlockFace.valueOf(actionData.direction.toUpperCase());
		}
		return null;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "hangingitem";
		name = data.toLowerCase();
		if(this.actionData.type != null){
			name = this.actionData.type;
		}
		return name;
	}
}