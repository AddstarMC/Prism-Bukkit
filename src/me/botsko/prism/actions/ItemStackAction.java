package me.botsko.prism.actions;

import java.text.SimpleDateFormat;
import java.util.Map.Entry;

import me.botsko.prism.utils.TypeUtils;

import org.bukkit.Location;
import org.bukkit.enchantments.Enchantment;
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
	public ItemStackAction( ActionType action_type, ItemStack item, int quantity, Location loc, String player_name ){
		if(action_type != null){
			this.type = action_type;
		}
		if(item != null){
			this.item = item;
			this.quantity = quantity;
			this.world_name = loc.getWorld().getName();
			this.x = loc.getX();
			this.y = loc.getY();
			this.z = loc.getZ();
		}
		if(player_name != null){
			this.player_name = player_name;
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
			if(!item.getEnchantments().isEmpty()){
				for(Entry<Enchantment, Integer> ench : item.getEnchantments().entrySet()){
					data += ":" + ench.getKey().getId() + "," + ench.getValue();
				}
			}
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
			if (blockArr.length >= 3){
				int block_subid = (byte) Integer.parseInt(blockArr[1]);
				quantity = Integer.parseInt(blockArr[2]);
				item = new ItemStack(block_id,quantity,(short)block_subid);
				if(blockArr.length > 3){
					for(int i = 3; i < blockArr.length; i++){
						item.addEnchantment(Enchantment.getById(Integer.parseInt(blockArr[i].split(",")[0])), Integer.parseInt(blockArr[i].split(",")[1]));
					}
				}
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
			name = quantity + " " + this.materialAliases.getItemStackAliasByItemStack(item);
		}
		return name;
	}
}