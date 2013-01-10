package me.botsko.prism.appliers;

import java.util.List;

import org.bukkit.ChatColor;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Action;

public class Restore extends Preview {
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Restore( Prism plugin, Player player, List<Action> results, QueryParameters parameters ) {
		this.processType = PrismProcessType.RESTORE;
		this.plugin = plugin;
		this.player = player;
		this.results = results;
		this.parameters = parameters;
	}
	
	
	/**
	 * Set preview move and then do a rollback
	 * @return
	 */
	public ApplierResult preview(){
		is_preview = true;
		return apply();
	}
	
	
	/**
	 * 
	 */
	public ApplierResult apply(){
		
		// Give the results to the changequeue
		ApplierResult changesApplied = super.apply();
		
		// No changes!
		if(changesApplied == null){
			player.sendMessage( plugin.playerHeaderMsg( ChatColor.GRAY + "No actions found that match the criteria." ) );
		}
			
		// Build the results message
		if(!is_preview){
			
			// Build the results message
			String msg = changesApplied.getChanges_applied() + " events restored.";
			if(changesApplied.getChanges_skipped() > 0){
				msg += " " + changesApplied.getChanges_skipped() + " skipped.";
			}
			if(changesApplied.getChanges_applied() > 0){
				msg += ChatColor.GRAY + " It's like it was always there.";
			}
			player.sendMessage( plugin.playerHeaderMsg( msg ) );
			
		} else {
		
			// Build the results message
			String msg = changesApplied.getChanges_applied() + " planned restorations.";
			if(changesApplied.getChanges_skipped() > 0){
				msg += " " + changesApplied.getChanges_skipped() + " skipped.";
			}
			if(changesApplied.getChanges_applied() > 0){
				msg += ChatColor.GRAY + " Use /prism preview apply to confirm this restore.";
			}
			player.sendMessage( plugin.playerHeaderMsg( msg ) );
			
			// Let me know there's no need to cancel/apply
			if(changesApplied.getChanges_applied() == 0){
				player.sendMessage( plugin.playerHeaderMsg( ChatColor.GRAY + "Nothing to restore, preview canceled for you." ) );
			}
		}
		return changesApplied;
	}
}