package me.botsko.prism.actionlibs;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;

import me.botsko.prism.database.InsertQuery;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Handler;

public class RecordingTask implements Runnable {

	/**
	 *
	 */
	private final Prism plugin;

	/**
	 *
	 * @param plugin
	 */
	public RecordingTask(Prism plugin) {
		this.plugin = plugin;
	}

	/**
	 *
	 */
	public void save() {
		if (!RecordingQueue.getQueue().isEmpty()) {
			insertActionsIntoDatabase();
		}
	}

	/**
	 *
	 * @param a
	 */
	public static long insertActionIntoDatabase(Handler a) {
        return Prism.getPrismDataSource().getDataInsertionQuery().insertActionIntoDatabase(a);
	}

	/**
	 *
	 * @throws SQLException
	 */
	public void insertActionsIntoDatabase() {
		int actionsRecorded = 0;
			int perBatch = plugin.getConfig().getInt("prism.database.actions-per-insert-batch");
			if (perBatch < 1)
				perBatch = 1000;
			if (!RecordingQueue.getQueue().isEmpty()) {
                if (Prism.getPrismDataSource().isPaused()) {
                    Prism.log(
                            "Prism database paused. An external actor has paused database processing...scheduling next recording");
                    scheduleNextRecording();
                    return;
                }
				Prism.debug("Beginning batch insert from queue. " + System.currentTimeMillis());
				// Handle dead connections
				Connection conn = Prism.getPrismDataSource().getConnection();
                try {
                    if ((conn == null) || (conn.isClosed())) {
                        if (RecordingManager.failedDbConnectionCount == 0) {
                            Prism.log(
                                    "Prism database error. Connection should be there but it's not. Leaving actions to log in queue.");
                        }
                        RecordingManager.failedDbConnectionCount++;
                        if (RecordingManager.failedDbConnectionCount > plugin.getConfig()
                                .getInt("prism.database.max-failures-before-wait")) {
                            Prism.log("Too many problems connecting. Giving up for a bit.");
                            scheduleNextRecording();
                        }
                        Prism.debug("Database connection still missing, incrementing count.");
                        conn.close();
                        return;
                    } else {
                        conn.close();
                        RecordingManager.failedDbConnectionCount = 0;
                    }
                } catch (SQLException e) {
                    e.printStackTrace();
                    Prism.getPrismDataSource().handleDataSourceException(e);
                    return;
                }
                InsertQuery batchedQuery = null;
                try {
                    batchedQuery = Prism.getPrismDataSource().getDataInsertionQuery();
                    batchedQuery.createBatch();
                } catch (Exception e) {
                    e.printStackTrace();
                    if (e instanceof SQLException)
                        Prism.getPrismDataSource().handleDataSourceException((SQLException) e);
                    Prism.debug("Database connection issue;");
                    RecordingManager.failedDbConnectionCount++;
                    return;
                }
				int i = 0;
				while (!RecordingQueue.getQueue().isEmpty()) {
					final Handler a = RecordingQueue.getQueue().poll();

					// poll() returns null if queue is empty
					if (a == null)
						break;

					if (a.isCanceled())
						continue;
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
				// The main delay is here
				try {
                    batchedQuery.processBatch();
                } catch (Exception e) {
                    e.printStackTrace();
                }
                // Save the current count to the queue for short historical data
                plugin.queueStats.addRunCount(actionsRecorded);
            }
	}

	/**
	 *
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
	 *
	 * @return
	 */
	protected int getTickDelayForNextBatch() {

		// If we have too many rejected connections, increase the schedule
		if (RecordingManager.failedDbConnectionCount > plugin.getConfig()
				.getInt("prism.database.max-failures-before-wait")) {
			return RecordingManager.failedDbConnectionCount * 20;
		}

		int recorder_tick_delay = plugin.getConfig().getInt("prism.queue-empty-tick-delay");
		if (recorder_tick_delay < 1) {
			recorder_tick_delay = 3;
		}
		return recorder_tick_delay;
	}

	/**
	 *
	 */
	protected void scheduleNextRecording() {
		if (!plugin.isEnabled()) {
			Prism.log(
					"Can't schedule new recording tasks as plugin is now disabled. If you're shutting down the server, ignore me.");
			return;
		}
		plugin.recordingTask = plugin.getServer().getScheduler().runTaskLaterAsynchronously(plugin,
				this, getTickDelayForNextBatch());
	}

	public static void updateRollbackDatabase(ArrayList<Long> dataIds, boolean isRestore) {
		if (dataIds.isEmpty()) {
			return;
		}

		String prefix = Prism.config.getString("prism.mysql.prefix");
		final Connection conn = Prism.getPrismDataSource().getConnection();
		try {
			if (conn == null || conn.isClosed()) {
				Prism.log("Prism database error. Connection should be there but it's not. This action wasn't logged.");
				return;
			}

			conn.setAutoCommit(false);
			final PreparedStatement s = conn.prepareStatement("INSERT INTO "
					+ prefix + "data_rollback (data_id,rollback) VALUES (?,?) ON DUPLICATE KEY UPDATE rollback = ?");
			for (long id : dataIds) {
				s.setLong(1, id);
				if (isRestore) {
					s.setNull(2, java.sql.Types.BIGINT);
					s.setNull(3, java.sql.Types.BIGINT);
				} else {
					s.setInt(2, 1);
					s.setInt(3, 1);
				}
				s.addBatch();
			}

			s.executeBatch();
			conn.commit();

		} catch (SQLException e) {
			e.printStackTrace();
		} finally {
			if (conn != null)
				try {
					conn.close();
				} catch (final SQLException ignored) {}
		}
	}
}
