package me.botsko.prism.purge;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import org.bukkit.command.CommandSender;

public class SenderPurgeCallback implements PurgeCallback {

    private CommandSender sender;

    @Override
    public void cycle(QueryParameters param, int cycleRowsAffected, int totalRecordsAffected,
                      boolean cycleComplete, long maxCycleTime) {
        if (sender == null) {
            return;
        }
        Prism.messenger.sendMessage(sender,
                Prism.messenger.playerSubduedHeaderMsg("Purge cycle cleared " + cycleRowsAffected + " records."));
        if (cycleComplete) {
            Prism.messenger.sendMessage(sender,
                    Prism.messenger.playerHeaderMsg(totalRecordsAffected + " records purged. Max cycle time "
                            + maxCycleTime + " msec."));
        }
    }

    public void setSender(CommandSender sender) {
        this.sender = sender;
    }
}