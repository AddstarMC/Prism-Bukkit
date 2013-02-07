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
			msg += ChatColor.GRAY + "[" + a.getId() + "] ";
		}
		
		// Date & Time
		msg += ChatColor.GRAY + a.getDisplay_date();
		msg += " " + ChatColor.GRAY + a.getDisplay_time().toLowerCase();
		
		// +/-
		msg += getPosNegPrefix();
		
		// Who
		msg += highlight + a.getPlayer_name();
		
		// Description of event
		msg += " " + ChatColor.WHITE + a.getType().getNiceDescription();
		if(a.getType().getHandler() != null){
			msg += " " + highlight + a.getNiceName();
		} else {
			// We should really improve this, but this saves me from having to make
			// a custom handler.
			if(a.getType().equals(ActionType.LAVA_BUCKET)){
				msg += " " + highlight + "lava";
			} 
			else if (a.getType().equals(ActionType.WATER_BUCKET)){
				msg += " " + highlight + "water";
			}
		}
		
		// Aggregate count
		if( a.getAggregateCount() > 1 ){
			msg += ChatColor.GREEN + " x"+a.getAggregateCount();
		}
		
		// Time since
		if(!a.getTimeSince().isEmpty()){
			msg += ChatColor.WHITE + " " + a.getTimeSince();
		}
		
		// Action type reminder
		msg += " " + ChatColor.GRAY + "(a:" + a.getType().getActionShortType() + ")";
		
		return msg;
		
	}
	
	
	/**
	 * 
	 * @param type
	 * @return
	 */
	protected String getPosNegPrefix(){
		
		if( a.getType().doesCreateBlock() || a.getType().equals(ActionType.ITEM_INSERT) || a.getType().equals(ActionType.SIGN_CHANGE) ){
			return ChatColor.GREEN + " + " + ChatColor.WHITE;
		}
		else {
			return ChatColor.RED + " - " + ChatColor.WHITE;
		}
	}


	/**
	 * @param hide_id the hide_id to set
	 */
	public void hideId(boolean hide_id) {
		this.hide_id = hide_id;
	}
}