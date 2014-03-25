package me.botsko.prism.purge;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;

public class LogPurgeCallback implements PurgeCallback {

    /**
     * Simply log the purges, being done automatically
     */
    @Override
    public void cycle(QueryParameters param, int cycle_rows_affected, int total_records_affected, boolean cycle_complete) {
        Prism.debug( "Purge cycle cleared " + cycle_rows_affected + " rows." );
        if( cycle_complete ) {
            Prism.log( "Cleared " + total_records_affected + " rows from the database. Using:"
                    + param.getOriginalCommand() );
        }
    }
}