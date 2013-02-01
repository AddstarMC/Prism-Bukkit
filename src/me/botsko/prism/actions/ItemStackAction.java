package me.botsko.prism.actions;

import java.util.Map.Entry;
import me.botsko.prism.utils.TypeUtils;

import org.bukkit.Color;
import org.bukkit.Location;
import org.bukkit.enchantments.Enchantment;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.meta.LeatherArmorMeta;

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
		
		super(action_type, player_name);
		
		if(item != null){
			this.item = item;
			this.quantity = quantity;
			this.world_name = loc.getWorld().getName();
			this.x = loc.getX();
			this.y = loc.getY();
			this.z = loc.getZ();
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
			data += addLeatherArmorColors();
			if(!item.getEnchantments().isEmpty()){
				for(Entry<Enchantment, Integer> ench : item.getEnchantments().entrySet()){
					data += ":" + ench.getKey().getId() + "," + ench.getValue();
				}
			}
		}
	}
	
	
	/**
	 * We need to add the colors for Leather Armor to the data.
	 * @return
	 */
	private String addLeatherArmorColors() {
		if(isLeather()){
			LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
			if(lam.getColor() != null){
				return ":;" + lam.getColor().asRGB() + ";";
			}
		}
		return "";
	}
	
	
	/**
	 * Get if the item is leather armor
	 * @return
	 */
	private boolean isLeather(){
		switch(item.getType()){
			case LEATHER_HELMET:
			case LEATHER_CHESTPLATE:
			case LEATHER_LEGGINGS:
			case LEATHER_BOOTS:
				return true;
			default:
				return false;
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
				int block_subid = Integer.parseInt(blockArr[1]);
				quantity = Integer.parseInt(blockArr[2]);
				item = new ItemStack(block_id,quantity,(short)block_subid);
				if(blockArr.length > 3){
					if(isLeather() && blockArr[3].contains(";")){
						String rgb = blockArr[3].replaceAll(";", "");
						if (!TypeUtils.isNumeric(rgb)) return;
						int color = Integer.parseInt(rgb);
						LeatherArmorMeta lam = (LeatherArmorMeta) item.getItemMeta();
						lam.setColor(Color.fromRGB(color));
						item.setItemMeta(lam);
					}
					for(int i = 3; i < blockArr.length; i++){
						if(!blockArr[i].contains(";"))
							item.addUnsafeEnchantment(Enchantment.getById(Integer.parseInt(blockArr[i].split(",")[0])), Integer.parseInt(blockArr[i].split(",")[1]));
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