package me.botsko.prism.actionlibs;

import me.botsko.prism.Prism;

public class QueueDrain {

    private final Prism plugin;

    /**
     * Creat a drain.
     *
     * @param plugin Prism.
     */
    public QueueDrain(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Drain the queue.
     */
    public void forceDrainQueue() {

        me.botsko.prism.PrismLogHandler.log("Forcing recorder queue to run a new batch before shutdown...");

        final RecordingTask recorderTask = new RecordingTask(plugin);

        // Force queue to empty
        while (!RecordingQueue.getQueue().isEmpty()) {

            me.botsko.prism.PrismLogHandler.log("Starting drain batch...");
            me.botsko.prism.PrismLogHandler.log("Current queue size: " + RecordingQueue.getQueue().size());

            // run insert
            try {
                recorderTask.insertActionsIntoDatabase();
            } catch (final Exception e) {
                e.printStackTrace();
                me.botsko.prism.PrismLogHandler.log("Stopping queue drain due to caught exception. Queue items lost: "
                        + RecordingQueue.getQueue().size());
                break;
            }

            if (RecordingManager.failedDbConnectionCount > 0) {
                me.botsko.prism.PrismLogHandler.log("Stopping queue drain due to detected database error. Queue items lost: "
                        + RecordingQueue.getQueue().size());
            }
        }
    }
}