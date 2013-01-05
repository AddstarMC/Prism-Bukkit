package me.botsko.prism.appliers;

import java.util.List;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Action;

import org.bukkit.ChatColor;
import org.bukkit.entity.Player;

public class Preview implements Previewable {
	
	/**
	 * 
	 */
	protected Prism plugin;
	
	/**
	 * 
	 */
	protected Player player;
	
	/**
	 * 
	 */
	protected List<Action> results;
	
	/**
	 * 
	 */
	protected QueryParameters parameters;
	
	/**
	 * 
	 */
	protected boolean is_preview = false;
	
	
	/**
	 * 
	 */
	public void cancel_preview(){
		if(plugin.playerActivePreviews.containsKey(player.getName())){
			
			PreviewSession previewSession = plugin.playerActivePreviews.get( player.getName() );
			
			if(!previewSession.getResults().getUndoQueue().isEmpty()){
				for(Undo u : previewSession.getResults().getUndoQueue()){
					player.sendBlockChange(u.getOriginalBlock().getLocation(), u.getOriginalBlock().getTypeId(), u.getOriginalBlock().getData());
				}
			}
			
			player.sendMessage( plugin.playerHeaderMsg( "Preview canceled." + ChatColor.GRAY + " Please come again!" ) );
			
			plugin.playerActivePreviews.remove( player.getName() );
			
		}
	}
	
	
	/**
	 * 
	 */
	public void apply_preview(){
		if(plugin.playerActivePreviews.containsKey(player.getName())){
			
			// Get preview session
			PreviewSession ps = plugin.playerActivePreviews.get(player.getName());
			
//			player.sendMessage( plugin.playerHeaderMsg("Applying rollback from preview...") );
			ps.getPreviewer().apply();
			
			plugin.playerActivePreviews.remove( player.getName() );
			
		}
	}
	

	/**
	 * 
	 */
	public ApplierResult preview() {
		return null;
	}


	/**
	 * 
	 */
	public ApplierResult apply() {
		return null;
	}
}
