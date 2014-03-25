package me.botsko.prism.purge;

import me.botsko.prism.actionlibs.QueryParameters;

public interface PurgeCallback {
    public void cycle(QueryParameters param, int cycle_rows_affected, int total_records_affected, boolean cycle_complete);
}