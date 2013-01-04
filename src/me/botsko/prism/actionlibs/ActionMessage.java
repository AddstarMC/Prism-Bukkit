package me.botsko.prism.actionlibs;

import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.ActionType;

import org.bukkit.ChatColor;

public class ActionMessage {
	
	/**
	 * 
	 */
	protected Action a;
	
	/**
	 * 
	 */
	private boolean hide_id = true;
	
	
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
		if(!hide_id){
			msg += ChatColor.GRAY + "" + a.getId() + " ";
		}
		msg += getPosNegPrefix();
		msg += highlight + a.getPlayer_name();
		msg += " " + ChatColor.WHITE + a.getType().getNiceDescription();
		msg += " " + highlight + a.getNiceName();
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
		
		if( a.getType().doesCreateBlock() || a.getType().equals(ActionType.ITEM_INSERT) || a.getType().equals(ActionType.SIGN_CHANGE) ){
			return ChatColor.GREEN + "+ " + ChatColor.WHITE;
		}
		else {
			return ChatColor.RED + "- " + ChatColor.WHITE;
		}
	}


	/**
	 * @param hide_id the hide_id to set
	 */
	public void hideId(boolean hide_id) {
		this.hide_id = hide_id;
	}
}