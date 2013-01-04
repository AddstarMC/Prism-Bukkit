package me.botsko.prism.actions;

import java.text.SimpleDateFormat;
import org.bukkit.entity.Hanging;

public class HangingItemAction extends GenericAction {
	
	/**
	 * 
	 */
	protected Hanging hanging;
	

	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public HangingItemAction( ActionType action_type, Hanging hanging, String player ){
		if(action_type != null){
			this.type = action_type;
		}
		if(hanging != null){
			
			this.hanging = hanging;
			this.world_name = hanging.getWorld().getName();
			this.x = hanging.getLocation().getX();
			this.y = hanging.getLocation().getY();
			this.z = hanging.getLocation().getZ();
		}
		if(player != null){
			this.player_name = player;
		}
		if(action_time == null){
			java.util.Date date= new java.util.Date();
			action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		}
		// Set data from current block
		setDataFromHanging();
		setHangingFromData();
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		setDataFromHanging();
	}
	
	
	/**
	 * 
	 */
	protected void setDataFromHanging(){
		if(data == null && hanging != null){
			data = hanging.getType().getName();
		}
	}
	
	
	/**
	 * 
	 */
	protected void setHangingFromData(){
		if(hanging == null && data != null){
//			String[] blockArr = data.split(":");
//			if (!TypeUtils.isNumeric(blockArr[0])) return;
//			
//			block_id = Integer.parseInt(blockArr[0]);
//			if (blockArr.length > 1){
//				block_subid = (byte) Integer.parseInt(blockArr[1]);
//			}
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "hangingitem";
//		String[] blockdata = getData().split(":");
//		if(blockdata.length == 2){
//			ItemStack i = new ItemStack(Integer.parseInt(blockdata[0]),(byte)Integer.parseInt(blockdata[1]));
//			name = i.getType().name().toLowerCase().replace("_", " ");
//		}
		return name;
	}
}