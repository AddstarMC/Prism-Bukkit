package me.botsko.prism.actions;

import java.text.SimpleDateFormat;
import me.botsko.prism.utils.TypeUtils;

import org.bukkit.Location;
import org.bukkit.entity.Player;
import org.bukkit.inventory.ItemStack;

public class ItemStackAction extends GenericAction {
	
	/**
	 * 
	 */
	protected ItemStack item;

	/**
	 * 
	 */
	protected int quantity;
	
	
	/**
	 * 
	 * @param action_type
	 * @param block
	 * @param player
	 */
	public ItemStackAction( ActionType action_type, ItemStack item, Location loc, Player player ){
		if(action_type != null){
			this.type = action_type;
		}
		if(item != null){
			this.item = item;
			this.quantity = item.getAmount();
			this.world_name = player.getWorld().getName();
			this.x = loc.getX();
			this.y = loc.getY();
			this.z = loc.getZ();
		}
		if(player != null){
			this.player_name = player.getName();
		}
		if(action_time == null){
			java.util.Date date= new java.util.Date();
			action_time = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss").format(date.getTime());
		}
		// Set data from current block
		setDataFromItem();
		setItemStackFromData();
	}
	
	
	/**
	 * 
	 */
	public void setData( String data ){
		this.data = data;
		setItemStackFromData();
	}
	
	
	/**
	 * 
	 */
	protected void setDataFromItem(){
		if(data == null && item != null){
			data = item.getTypeId() + ":" + item.getDurability() + ":" + quantity;
		}
	}
	
	
	/**
	 * 
	 */
	protected void setItemStackFromData(){
		if(item == null && data != null){
			String[] blockArr = data.split(":");
			if (!TypeUtils.isNumeric(blockArr[0])) return;
			int block_id = Integer.parseInt(blockArr[0]);
			if (blockArr.length == 3){
				int block_subid = (byte) Integer.parseInt(blockArr[1]);
				quantity = Integer.parseInt(blockArr[2]);
				item = new ItemStack(block_id,quantity,(short)block_subid);
			}
		}
	}
	
	
	/**
	 * 
	 * @return
	 */
	public ItemStack getItem(){
		return item;
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		String name = "";
		if(item != null){
			name = quantity + " " + item.getType().name().toLowerCase().replace("_", " ");
		}
		return name;
	}
}