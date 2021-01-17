package me.botsko.prism.actionlibs;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.database.InsertQuery;
import me.botsko.prism.measurement.QueueStats;

import java.sql.Connection;
import java.sql.SQLException;

public class RecordingTask implements Runnable {

    private final Prism plugin;
    private static int actionsPerInsert;

    public static void setActionsPerInsert(int adjust) {
        actionsPerInsert = adjust;
    }


    /**
     * Create the task.
     *
     * @param plugin Plugin
     */
    public RecordingTask(Prism plugin) {
        this.plugin = plugin;
        actionsPerInsert = plugin.getConfig().getInt("prism.database.actions-per-insert-batch");
    }

    /**
     * Insert Action.
     *
     * @param a Handler
     * @return rows affected.
     */
    public static long insertActionIntoDatabase(Handler a) {
        return Prism.getPrismDataSource().getDataInsertionQuery().insertActionIntoDatabase(a);
    }

    /**
     * If the queue isn't empty run an insert.
     */
    public void save() {
        if (!RecordingQueue.getQueue().isEmpty()) {
            insertActionsIntoDatabase();
        }
    }

    /**
     * Create a Insertion.
     */
    void insertActionsIntoDatabase() {
        int actionsRecorded = 0;
        int perBatch = actionsPerInsert;
        if (perBatch < 1) {
            perBatch = 1000;
        }
        if (!RecordingQueue.getQueue().isEmpty()) {
            if (Prism.getPrismDataSource().isPaused()) {
                Prism.log(
                        "Prism database paused. An external actor has paused database processing..."
                                + "scheduling next recording");
                scheduleNextRecording();
                return;
            }
            long start = System.currentTimeMillis();
            Prism.debug("Beginning batch insert from queue. " + start);
            try (
                    Connection conn = Prism.getPrismDataSource().getConnection()
            ) {
                if ((conn == null) || (conn.isClosed())) {
                    if (RecordingManager.failedDbConnectionCount == 0) {
                        Prism.log(
                                "Prism database error. Connection should be there but it's not. "
                                        + "Leaving actions to log in queue.");
                    }
                    RecordingManager.failedDbConnectionCount++;
                    if (RecordingManager.failedDbConnectionCount > plugin.getConfig()
                            .getInt("prism.database.max-failures-before-wait")) {
                        Prism.log("Too many problems connecting. Giving up for a bit.");
                        scheduleNextRecording();
                    }
                    Prism.debug("Database connection still missing, incrementing count.");
                    return;
                } else {
                    RecordingManager.failedDbConnectionCount = 0;
                }
            } catch (SQLException e) {
                e.printStackTrace();
                Prism.getPrismDataSource().handleDataSourceException(e);
                return;
            }
            InsertQuery batchedQuery;
            try {
                batchedQuery = Prism.getPrismDataSource().getDataInsertionQuery();
                batchedQuery.createBatch();
            } catch (Exception e) {
                e.printStackTrace();
                if (e instanceof SQLException) {
                    Prism.getPrismDataSource().handleDataSourceException((SQLException) e);
                }
                Prism.debug("Database connection issue;");
                RecordingManager.failedDbConnectionCount++;
                return;
            }
            int i = 0;
            while (!RecordingQueue.getQueue().isEmpty()) {
                final Handler a = RecordingQueue.getQueue().poll();

                // poll() returns null if queue is empty
                if (a == null) {
                    break;
                }
                if (a.isCanceled()) {
                    continue;
                }
                batchedQuery.insertActionIntoDatabase(a);

                actionsRecorded++;

                // Break out of the loop and just commit what we have
                if (i >= perBatch) {
                    Prism.debug("Recorder: Batch max exceeded, running insert. Queue remaining: "
                            + RecordingQueue.getQueue().size());
                    break;
                }
                i++;
            }
            long batchDoneTime = System.currentTimeMillis();
            long batchingTime = batchDoneTime - start;
            // The main delay is here
            try {
                batchedQuery.processBatch();
            } catch (Exception e) {
                e.printStackTrace();
            }
            // Save the current count to the queue for short historical data
            long batchProcessedEnd = System.currentTimeMillis();
            long batchRunTime = batchProcessedEnd - batchDoneTime;
            plugin.queueStats.addRunInfo(new QueueStats.TaskRunInfo(actionsRecorded,batchingTime,batchRunTime));
        }
    }

    /**
     * Rebuild the datasource and schedule the next run.
     */
    @Override
    public void run() {
        if (RecordingManager.failedDbConnectionCount > 5) {
            Prism.getPrismDataSource().rebuildDataSource(); // force rebuild pool after several failures
        }
        save();
        scheduleNextRecording();
    }

    /**
     * Get the delay based on connection failure.
     *
     * @return delay
     */
    private int getTickDelayForNextBatch() {

        // If we have too many rejected connections, increase the schedule
        if (RecordingManager.failedDbConnectionCount > plugin.getConfig()
                .getInt("prism.database.max-failures-before-wait")) {
            return RecordingManager.failedDbConnectionCount * 20;
        }

        int recorderTickDelay = plugin.getConfig().getInt("prism.queue-empty-tick-delay");
        if (recorderTickDelay < 1) {
            recorderTickDelay = 3;
        }
        return recorderTickDelay;
    }

    /**
     * Schedule a async recording with delay.
     */
    private void scheduleNextRecording() {
        if (!plugin.isEnabled()) {
            Prism.log(
                    "Can't schedule new recording tasks as plugin is now disabled. If you're shutting"
                            + " down the server, ignore me.");
            return;
        }
        plugin.recordingTask = plugin.getServer().getScheduler().runTaskLaterAsynchronously(plugin,
                this, getTickDelayForNextBatch());
    }
}
