package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import org.bukkit.block.Block;

public class SignAction extends GenericAction {
	
	/**
	 * 
	 */
	protected Block block;

	/**
	 * 
	 */
	protected String[] lines;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public SignAction( ActionType action_type, Block block, String[] lines, String player ){
		if(action_type != null){
			this.type = action_type;
		}
		if(block != null){
			this.block = block;
			this.lines = lines;
			this.world_name = block.getWorld().getName();
			this.x = block.getX();
			this.y = block.getY();
			this.z = block.getZ();
		}
		if(player != null){
			this.player_name = player;
		}
		if(action_time == null){
			java.util.Date date= new java.util.Date();
			action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		}
		// Set data from current block
		setDataFromSignContent();
		setSignContentFromData();
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		setSignContentFromData();
	}
	
	
	/**
	 * There's likely a better way but I'm tired. The only
	 * safe separator character is 17 characters, 1 too many
	 * for any single line.
	 */
	protected void setDataFromSignContent(){
		if(data == null && lines != null){
			data = "";
			for(String line : lines){
				data += line+"-----------------";
			}
		}
	}
	
	
	/**
	 * 
	 */
	protected void setSignContentFromData(){
		if(lines == null && data != null){
			lines = data.split("-----------------");
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "sign (";
		if(lines.length > 0){
			int c = 1;
			for(String line : lines){
				name += line + (c < lines.length ? ", " : "");
				c++;
			}
		} else {
			name += "no text";
		}
		name += ")";
		return name;
	}
}