package me.botsko.prism.purge;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;

public class LogPurgeCallback implements PurgeCallback {

    /**
     * Simply log the purges, being done automatically
     */
    @Override
    public void cycle(QueryParameters param, int cycle_rows_affected, int total_records_affected, boolean cycle_complete, long max_cycle_time) {
        Prism.debug( "Purge cycle cleared " + cycle_rows_affected + " rows." );
        //Prism.log( "Purge cycle cleared " + cycle_rows_affected + " rows.  " + cycle_time + " msec." );
        if( cycle_complete ) {
            Prism.log( "Cleared " + total_records_affected + " rows. Max cycle time " + max_cycle_time + " msec. Using:"
                    + param.getOriginalCommand() );
        }
    }
}