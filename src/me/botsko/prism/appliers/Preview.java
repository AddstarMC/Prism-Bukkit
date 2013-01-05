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
	 * @param is_preview
	 */
	public void setIsPreview(boolean is_preview){
		this.is_preview = is_preview;
	}
	
	
	/**
	 * 
	 */
	public void cancel_preview(){
		if(plugin.playerActivePreviews.containsKey(player.getName())){
			
			PreviewSession previewSession = plugin.playerActivePreviews.get( player.getName() );
			plugin.debug("Undo queue empty: " + previewSession.getResults().getUndoQueue().isEmpty());
			if(!previewSession.getResults().getUndoQueue().isEmpty()){
				
				for(Undo u : previewSession.getResults().getUndoQueue()){
					plugin.debug("Preview block sent to player: " + u.getOriginalBlock().getType().name());
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
			
			player.sendMessage( plugin.playerHeaderMsg("Applying rollback from preview...") );
			ps.getPreviewer().setIsPreview(false);
			ApplierResult result = ps.getPreviewer().apply();
			if(!result.getMessages().isEmpty()){
				for(String resp : result.getMessages()){
					player.getPlayer().sendMessage(resp);
				}
			}
			
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
