package me.botsko.prism.purge;

import me.botsko.prism.actionlibs.QueryParameters;

public interface PurgeCallback {
    void cycle(QueryParameters param, int cycleRowsAffected, int totalRecordsAffected,
               boolean cycleComplete, long maxCycleTime);
}