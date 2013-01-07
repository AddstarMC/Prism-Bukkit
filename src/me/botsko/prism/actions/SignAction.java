package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.material.Sign;

public class SignAction extends GenericAction {
	
	/**
	 * 
	 */
	protected Block block;
	
	/**
	 * 
	 */
	protected SignActionData actionData;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public SignAction( ActionType action_type, Block block, String[] lines, String player ){
		
		// Build an object for the specific details of this action
		actionData = new SignActionData();
				
		if(action_type != null){
			this.type = action_type;
		}
		if(block != null){
			actionData.sign_type = block.getType().name();
			Sign sign = (Sign) block.getState().getData();
			actionData.facing = sign.getFacing();
			this.block = block;
			this.world_name = block.getWorld().getName();
			this.x = block.getX();
			this.y = block.getY();
			this.z = block.getZ();
		}
		if(lines != null){
			actionData.lines = lines;
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
			actionData = gson.fromJson(data, SignActionData.class);
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String[] getLines(){
		return actionData.lines;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Material getSignType(){
		if(actionData.sign_type != null){
			Material m = Material.valueOf(actionData.sign_type);
			if(m != null){
				return m;
			}
		}
		return Material.SIGN;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public BlockFace getFacing(){
		return actionData.facing;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "sign (";
		if(actionData.lines != null && actionData.lines.length > 0){
			int c = 1;
			for(String line : actionData.lines){
				if(!line.isEmpty()){
					name += line + (c < actionData.lines.length ? ", " : "");
				}
				c++;
			}
		} else {
			name += "no text";
		}
		name += ")";
		return name;
	}
}