package me.botsko.prism.actions;

import org.bukkit.ChatColor;
import org.bukkit.inventory.ItemStack;

public class ActionMessage {
	
	/**
	 * 
	 */
	protected Action a;
	
	/**
	 * 
	 * @param a
	 */
	public ActionMessage( Action a ){
		this.a = a;
	}
	
	
	/**
	 * 
	 */
	public String getMessage(){
		
		String msg = getPosNegPrefix();
		msg += ChatColor.LIGHT_PURPLE + a.getPlayer_name();
		msg += " " + ChatColor.WHITE + getNiceActionType();
		msg += " " + ChatColor.LIGHT_PURPLE + getNiceData();
		msg += ChatColor.WHITE + " on " + ChatColor.GRAY + a.getDisplay_date();
		
		return msg;
		
	}
	
	
	/**
	 * 
	 * @param type
	 * @return
	 */
	protected String getPosNegPrefix(){
		
		if(a.getAction_type().equals("block-break")){
			return ChatColor.RED + "- " + ChatColor.WHITE;
		}
		else if(a.getAction_type().equals("block-place")){
			return ChatColor.GREEN + "+ " + ChatColor.WHITE;
		}
		return "";
	}
	
	
	/**
	 * 
	 * @param type
	 * @return
	 */
	protected String getNiceActionType(){
		
		if(a.getAction_type().equals("block-break")){
			return "broke";
		}
		if(a.getAction_type().equals("block-place")){
			return "placed";
		}
		return "unknown";
	}
	
	
	/**
	 * 
	 * @param data
	 * @return
	 */
	protected String getNiceData(){
		
		String name = "";
		
		if(a.getAction_type().equals("block-break") || a.getAction_type().equals("block-place")){
			String[] blockdata = a.getData().split(":");
			if(blockdata.length == 2){
				ItemStack i = new ItemStack(Integer.parseInt(blockdata[0]),(byte)Integer.parseInt(blockdata[1]));
				name = i.getType().name().toLowerCase();
			}
		}
		
		return name;
		
	}
}