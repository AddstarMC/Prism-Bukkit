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
		
		ChatColor highlight = ChatColor.DARK_AQUA;
		
		String msg = "";
		msg += ChatColor.GRAY + "" + a.getId() + " ";
		msg += getPosNegPrefix();
		msg += highlight + a.getPlayer_name();
		msg += " " + ChatColor.WHITE + getNiceActionType();
		msg += " " + highlight + getNiceData();
		msg += ChatColor.WHITE + " on " + ChatColor.GRAY + a.getDisplay_date();
		msg += ChatColor.WHITE + " at " + ChatColor.GRAY + a.getDisplay_time();
		
		return msg;
		
	}
	
	
	/**
	 * 
	 * @param type
	 * @return
	 */
	protected String getPosNegPrefix(){
		
		if(a.getAction_type().equals("block-break") || a.getAction_type().equals("entity-kill")){
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
		if(a.getAction_type().equals("entity-kill")){
			return "killed a";
		}
		return "did mystery stuff to";
	}
	
	
	/**
	 * 
	 * @param data
	 * @return
	 */
	protected String getNiceData(){
		
		String name = "something";
		
		if(a.getAction_type().equals("block-break") || a.getAction_type().equals("block-place")){
			String[] blockdata = a.getData().split(":");
			if(blockdata.length == 2){
				ItemStack i = new ItemStack(Integer.parseInt(blockdata[0]),(byte)Integer.parseInt(blockdata[1]));
				name = i.getType().name().toLowerCase();
			}
		}
		if(a.getAction_type().equals("entity-kill")){
			name = a.getData().toLowerCase();
		}
		
		return name;
		
	}
}