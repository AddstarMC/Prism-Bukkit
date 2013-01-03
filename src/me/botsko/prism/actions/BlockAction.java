package me.botsko.prism.actions;

import java.text.SimpleDateFormat;

import me.botsko.prism.actiontypes.ActionType;
import me.botsko.prism.utils.TypeUtils;

import org.bukkit.block.Block;
import org.bukkit.inventory.ItemStack;

public class BlockAction extends GenericAction {
	
	/**
	 * 
	 */
	protected Block block;
	
	/**
	 * 
	 */
	protected int block_id;
	
	/**
	 * 
	 */
	protected byte block_subid;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public BlockAction( ActionType action_type, Block block, String player ){
		if(action_type != null){
			this.type = action_type;
		}
		if(block != null){
			this.block = block;
			this.world_name = block.getWorld().getName();
			this.x = block.getLocation().getX();
			this.y = block.getLocation().getY();
			this.z = block.getLocation().getZ();
		}
		if(player != null){
			this.player_name = player;
		}
		if(action_time == null){
			java.util.Date date= new java.util.Date();
			action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		}
		// Set data from current block
		setDataFromBlock();
		setBlockIdsFromData();
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		setBlockIdsFromData();
	}
	
	
	/**
	 * 
	 */
	protected void setDataFromBlock(){
		if(data == null && block != null){
			data = block.getTypeId() + ":" + block.getData();
		}
	}
	
	
	/**
	 * 
	 */
	protected void setBlockIdsFromData(){
		if(block == null && data != null){
			String[] blockArr = data.split(":");
			if (!TypeUtils.isNumeric(blockArr[0])) return;
			
			block_id = Integer.parseInt(blockArr[0]);
			if (blockArr.length > 1){
				block_subid = (byte) Integer.parseInt(blockArr[1]);
			}
		}
	}
	
	
	/**
	 * 
	 */
	public int getBlock_id(){
		return block_id;
	}
	
	
	/**
	 * 
	 */
	public byte getBlock_subid(){
		return block_subid;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "block";
		String[] blockdata = getData().split(":");
		if(blockdata.length == 2){
			ItemStack i = new ItemStack(Integer.parseInt(blockdata[0]),(byte)Integer.parseInt(blockdata[1]));
			name = i.getType().name().toLowerCase().replace("_", " ");
		}
		return name;
	}
}