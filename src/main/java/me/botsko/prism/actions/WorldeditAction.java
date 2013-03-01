package me.botsko.prism.actions;

import org.bukkit.Location;


public class WorldeditAction extends GenericAction {
	
	public class WorldeditActionData {
		public String onBehalfOf;
		public int originalBlock_id;
		public int originalBlock_subid;
		public int newBlock_id;
		public int newBlock_subid;
	}
	
	/**
	 * 
	 */
	protected WorldeditActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public WorldeditAction( String action_type, Location loc, int originalBlock_id, int originalBlock_subid, int newBlock_id, int newBlock_subid, String playername ){
		
		super(action_type, playername);
		
		actionData = new WorldeditActionData();
		
		if(playername != null){
			actionData.originalBlock_id = originalBlock_id;
			actionData.originalBlock_subid = originalBlock_subid;
			actionData.newBlock_id = newBlock_id;
			actionData.newBlock_subid = newBlock_subid;
		}
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
	 */
	protected void setObjectFromData(){
		if(data != null){
			actionData = gson.fromJson(data, WorldeditActionData.class);
		}
	}

	
	/**
	 * @return the onBehalfOf
	 */
	public String getOnBehalfOf() {
		return actionData.onBehalfOf;
	}

	
	/**
	 * @return the originalBlock_id
	 */
	public int getOriginalBlockId() {
		return actionData.originalBlock_id;
	}

	
	/**
	 * @return the originalBlock_subid
	 */
	public int getOriginalBlockSubId() {
		return actionData.originalBlock_subid;
	}

	
	/**
	 * @return the newBlock_id
	 */
	public int getNewBlockId() {
		return actionData.newBlock_id;
	}

	
	/**
	 * @return the newBlock_subid
	 */
	public int getNewBlockSubId() {
		return actionData.newBlock_subid;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "";
		name += materialAliases.getItemStackAliasById(actionData.originalBlock_id, (byte)actionData.originalBlock_id);
		name += " to ";
		name += materialAliases.getItemStackAliasById(actionData.newBlock_id, (byte)actionData.newBlock_subid);
		return name;
	}
}