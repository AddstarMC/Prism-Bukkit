package me.botsko.prism.measurement;

import java.util.Date;
import java.util.Map.Entry;
import java.util.concurrent.ConcurrentSkipListMap;

public class QueueStats {

    /**
	 * 
	 */
    protected final ConcurrentSkipListMap<Long, Integer> perRunRecordingCounts = new ConcurrentSkipListMap<Long, Integer>();

    /**
     * 
     * @param count
     */
    public void addRunCount(int count) {

        final Date date = new Date();
        final long currentTime = date.getTime();

        // Delete any that are older than a few minutes, so we don't spam the
        // screen
        // unless we only have a few already
        if( perRunRecordingCounts.size() > 5 ) {
            int i = 0;
            for ( final Entry<Long, Integer> entry : perRunRecordingCounts.descendingMap().entrySet() ) {
                if( i++ > 5 ) {
                    perRunRecordingCounts.remove( entry.getKey() );
                }
            }
        }
        perRunRecordingCounts.put( currentTime, count );

    }

    /**
     * 
     * @return
     */
    public ConcurrentSkipListMap<Long, Integer> getRecentRunCounts() {
        return perRunRecordingCounts;
    }
}