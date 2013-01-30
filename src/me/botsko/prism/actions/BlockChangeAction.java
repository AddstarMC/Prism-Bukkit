package me.botsko.prism.actions;

import org.bukkit.Location;

public class BlockChangeAction extends GenericAction {
	
	/**
	 * 
	 */
	protected BlockChangeActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public BlockChangeAction( ActionType action_type, Location loc, int oldId, byte oldSubid, int newId, byte newSubid, String player ){
		
		super(action_type, player);
		
		// Build an object for the specific details of this action
		actionData = new BlockChangeActionData();
		
		actionData.new_id = newId;
		actionData.new_subid = newSubid;
		actionData.old_id = oldId;
		actionData.old_subid = oldSubid;
		
		if(loc != null){
			this.world_name = loc.getWorld().getName();
			this.x = loc.getX();
			this.y = loc.getY();
			this.z = loc.getZ();
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
	 * @return
	 */
	public BlockChangeActionData getActionData(){
		return actionData;
	}
	
	
	/**
	 * 
	 */
	protected void setObjectFromData(){
		if(data != null){
			actionData = gson.fromJson(data, BlockChangeActionData.class);
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "";
		name += materialAliases.getItemStackAliasById(actionData.new_id, actionData.new_subid);
		return name;
	}
	
	
	/**
	 * 
	 * @author botskonet
	 */
	public class BlockChangeActionData {
		public int old_id;
		public byte old_subid;
		public int new_id;
		public byte new_subid;
	}
}