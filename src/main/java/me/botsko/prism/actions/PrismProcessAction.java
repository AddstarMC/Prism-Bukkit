package me.botsko.prism.actions;

import me.botsko.prism.appliers.PrismProcessType;

import org.bukkit.entity.Player;


public class PrismProcessAction extends GenericAction {
	
	public class PrismProcessActionData {
		public String params = "";
		public String processType;
	}
	
	/**
	 * 
	 */
	private PrismProcessActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public PrismProcessAction( ActionType action_type, PrismProcessType processType, Player player, String parameters ){
		
		super(action_type, null);
		
		actionData = new PrismProcessActionData();
		
		if(processType != null){
			actionData.params = parameters;
			actionData.processType = processType.name().toLowerCase();
		}
		if(player != null){
			this.player_name = player.getName();
			this.world_name = player.getWorld().getName();
			this.x = player.getLocation().getX();
			this.y = player.getLocation().getY();
			this.z = player.getLocation().getZ();
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
			actionData = gson.fromJson(data, PrismProcessActionData.class);
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public ActionType getProcessChildActionType(){
		return ActionType.getByActionType( "prism-"+actionData.processType );
	}
	
	
	/**
	 * 
	 */
	public String getNiceName(){
		return actionData.processType + " ("+actionData.params+")";
	}
}