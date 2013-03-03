package me.botsko.prism.appliers;

import me.botsko.prism.Prism;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

public class PrismApplierCallback implements ApplierCallback {
	
	
	/**
	 * 
	 */
	public void handle( CommandSender sender, ApplierResult result ){
		// Send player success messages
		if(result.getProcessType().equals(PrismProcessType.ROLLBACK)){
		
			// Build the results message
			if(!result.isPreview()){
				
				String msg = result.getChangesApplied() + " reversals.";
				if(result.getChangesSkipped() > 0){
					msg += " " + result.getChangesSkipped() + " skipped.";
				}
				if(result.getChangesApplied() > 0){
					msg += ChatColor.GRAY + " It's like it never happened.";
				}
				sender.sendMessage( Prism.messenger.playerHeaderMsg( msg ) );
				
			} else {
			
				// Build the results message
				String msg = "At least " + result.getChangesApplied() + " planned reversals.";
				if(result.getChangesSkipped() > 0){
					msg += " " + result.getChangesSkipped() + " skipped.";
				}
				if(result.getChangesApplied() > 0){
					msg += ChatColor.GRAY + " Use /prism preview apply to confirm.";
				}
				sender.sendMessage( Prism.messenger.playerHeaderMsg( msg ) );
				
				// Let me know there's no need to cancel/apply
				if(result.getChangesApplied() == 0){
					sender.sendMessage( Prism.messenger.playerHeaderMsg( ChatColor.GRAY + "Nothing to rollback, preview canceled for you." ) );
				}
			}
		}
		
		
		// Build the results message
		if(result.getProcessType().equals(PrismProcessType.RESTORE)){
			if(!result.isPreview()){
				
				// Build the results message
				String msg = result.getChangesApplied() + " events restored.";
				if(result.getChangesSkipped() > 0){
					msg += " " + result.getChangesSkipped() + " skipped.";
				}
				if(result.getChangesApplied() > 0){
					msg += ChatColor.GRAY + " It's like it was always there.";
				}
				sender.sendMessage( Prism.messenger.playerHeaderMsg( msg ) );
				
			} else {
			
				// Build the results message
				String msg = result.getChangesApplied() + " planned restorations.";
				if(result.getChangesSkipped() > 0){
					msg += " " + result.getChangesSkipped() + " skipped.";
				}
				if(result.getChangesApplied() > 0){
					msg += ChatColor.GRAY + " Use /prism preview apply to confirm.";
				}
				sender.sendMessage( Prism.messenger.playerHeaderMsg( msg ) );
				
				// Let me know there's no need to cancel/apply
				if(result.getChangesApplied() == 0){
					sender.sendMessage( Prism.messenger.playerHeaderMsg( ChatColor.GRAY + "Nothing to restore, preview canceled for you." ) );
				}
			}
		}
		
		
		// Build the results message
		if(result.getProcessType().equals(PrismProcessType.UNDO)){
				
			// Build the results message
			String msg = result.getChangesApplied() + " things neverminded.";
			if(result.getChangesSkipped() > 0){
				msg += " " + result.getChangesSkipped() + " skipped.";
			}
			if(result.getChangesApplied() > 0){
				msg += ChatColor.GRAY + " If anyone asks, you never did that.";
			}
			sender.sendMessage( Prism.messenger.playerHeaderMsg( msg ) );
			
		}
	}
}