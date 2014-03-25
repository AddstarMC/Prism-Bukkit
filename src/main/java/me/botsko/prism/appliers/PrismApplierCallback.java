package me.botsko.prism.appliers;

import java.util.HashMap;
import java.util.Map.Entry;

import me.botsko.prism.Prism;

import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;

public class PrismApplierCallback implements ApplierCallback {

    /**
	 * 
	 */
    @Override
    public void handle(CommandSender sender, ApplierResult result) {

        // Did we move anyone?
        final HashMap<Entity, Integer> entitiesMoved = result.getEntitiesMoved();
        if( !entitiesMoved.isEmpty() ) {
            for ( final Entry<Entity, Integer> entry : entitiesMoved.entrySet() ) {
                if( entry.getKey() instanceof Player ) {
                    ( (Player) entry.getKey() ).sendMessage( Prism.messenger.playerSubduedHeaderMsg( "Moved you "
                            + entry.getValue() + " blocks to safety due to a rollback." ) );
                }
            }
        }

        // Send player success messages
        if( result.getProcessType().equals( PrismProcessType.ROLLBACK ) ) {

            // Build the results message
            if( !result.isPreview() ) {

                String msg = result.getChangesApplied() + " reversals.";
                if( result.getChangesSkipped() > 0 ) {
                    msg += " " + result.getChangesSkipped() + " skipped.";
                }
                if( result.getChangesApplied() > 0 ) {
                    msg += ChatColor.GRAY + " It's like it never happened.";
                }
                sender.sendMessage( Prism.messenger.playerHeaderMsg( msg ) );

            } else {

                // Build the results message
                String msg = "At least " + result.getChangesPlanned() + " planned reversals.";
                if( result.getChangesSkipped() > 0 ) {
                    msg += " " + result.getChangesSkipped() + " skipped.";
                }
                if( result.getChangesPlanned() > 0 ) {
                    msg += ChatColor.GRAY + " Use /prism preview apply to confirm.";
                }
                sender.sendMessage( Prism.messenger.playerHeaderMsg( msg ) );

                // Let me know there's no need to cancel/apply
                if( result.getChangesPlanned() == 0 ) {
                    sender.sendMessage( Prism.messenger.playerHeaderMsg( ChatColor.GRAY
                            + "Nothing to rollback, preview canceled for you." ) );
                }
            }
        }

        // Build the results message
        if( result.getProcessType().equals( PrismProcessType.RESTORE ) ) {
            if( !result.isPreview() ) {

                // Build the results message
                String msg = result.getChangesApplied() + " events restored.";
                if( result.getChangesSkipped() > 0 ) {
                    msg += " " + result.getChangesSkipped() + " skipped.";
                }
                if( result.getChangesApplied() > 0 ) {
                    msg += ChatColor.GRAY + " It's like it was always there.";
                }
                sender.sendMessage( Prism.messenger.playerHeaderMsg( msg ) );

            } else {

                // Build the results message
                String msg = result.getChangesPlanned() + " planned restorations.";
                if( result.getChangesSkipped() > 0 ) {
                    msg += " " + result.getChangesSkipped() + " skipped.";
                }
                if( result.getChangesPlanned() > 0 ) {
                    msg += ChatColor.GRAY + " Use /prism preview apply to confirm.";
                }
                sender.sendMessage( Prism.messenger.playerHeaderMsg( msg ) );

                // Let me know there's no need to cancel/apply
                if( result.getChangesPlanned() == 0 ) {
                    sender.sendMessage( Prism.messenger.playerHeaderMsg( ChatColor.GRAY
                            + "Nothing to restore, preview canceled for you." ) );
                }
            }
        }

        // Build the results message
        if( result.getProcessType().equals( PrismProcessType.UNDO ) ) {

            // Build the results message
            String msg = result.getChangesApplied() + " changes undone.";
            if( result.getChangesSkipped() > 0 ) {
                msg += " " + result.getChangesSkipped() + " skipped.";
            }
            if( result.getChangesApplied() > 0 ) {
                msg += ChatColor.GRAY + " If anyone asks, you never did that.";
            }
            sender.sendMessage( Prism.messenger.playerHeaderMsg( msg ) );

        }

        // Notify shared players of previews
        for ( final CommandSender sharedPlayer : result.getParameters().getSharedPlayers() ) {
            sharedPlayer.sendMessage( Prism.messenger.playerHeaderMsg( "A preview is being shared with you: "
                    + result.getParameters().getOriginalCommand() ) );
        }
    }
}