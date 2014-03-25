package me.botsko.prism.actionlibs;

import me.botsko.prism.Prism;

public class QueueDrain {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
     * 
     * @param plugin
     */
    public QueueDrain(Prism plugin) {
        this.plugin = plugin;
    }

    /**
	 * 
	 */
    public void forceDrainQueue() {

        Prism.log( "Forcing recorder queue to run a new batch before shutdown..." );

        final RecordingTask recorderTask = new RecordingTask( plugin );

        // Force queue to empty
        while ( !RecordingQueue.getQueue().isEmpty() ) {

            Prism.log( "Starting drain batch..." );
            Prism.log( "Current queue size: " + RecordingQueue.getQueue().size() );

            // run insert
            try {
                recorderTask.insertActionsIntoDatabase();
            } catch ( final Exception e ) {
                e.printStackTrace();
                Prism.log( "Stopping queue drain due to caught exception. Queue items lost: "
                        + RecordingQueue.getQueue().size() );
                break;
            }

            if( RecordingManager.failedDbConnectionCount > 0 ) {
                Prism.log( "Stopping queue drain due to detected database error. Queue items lost: "
                        + RecordingQueue.getQueue().size() );
            }
        }
    }
}