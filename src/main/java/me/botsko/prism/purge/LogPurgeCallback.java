package me.botsko.prism.purge;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;

public class LogPurgeCallback implements PurgeCallback {

    /**
     * Simply log the purges, being done automatically.
     */
    @Override
    public void cycle(QueryParameters param, int cycleRowsAffected, int totalRecordsAffected,
                      boolean cycleComplete, long maxCycleTime) {
        Prism.debug("Purge cycle cleared " + cycleRowsAffected + " rows.");
        if (cycleComplete) {
            Prism.log("Cleared " + totalRecordsAffected + " rows. Max cycle time " + maxCycleTime + " msec. Using:"
                    + param.getOriginalCommand());
        }
    }
}