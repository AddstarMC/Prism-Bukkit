package me.botsko.prism.purge;

import java.util.concurrent.CopyOnWriteArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;

public class PurgeTask implements Runnable {

	/**
	 * 
	 */
	private final Prism plugin;

	/**
	 * 
	 */
	private final CopyOnWriteArrayList<QueryParameters> paramList;

	/**
	 * 
	 */
	private int cycleRowsAffected = 0;

	/**
	 * 
	 */
	private final int purgeTickDelay;

	/**
	 * 
	 */
	private long minId = 0;

	/**
	 * 
	 */
	private long maxId = 0;

	/**
	 * 
	 */
	private final PurgeCallback callback;

	/**
	 * Used when we dont know the min max
	 *
	 * @param plugin
	 * @param paramList
	 * @param purgeTickDelay
	 * @param callback
	 */
	public PurgeTask(Prism plugin, CopyOnWriteArrayList<QueryParameters> paramList, int purgeTickDelay,
					 PurgeCallback callback) {
		this.plugin = plugin;
		this.paramList = paramList;
		this.purgeTickDelay = purgeTickDelay;
		this.callback = callback;
	}
	/**
	 * 
	 * @param plugin
	 */
	public PurgeTask(Prism plugin, CopyOnWriteArrayList<QueryParameters> paramList, int purgeTickDelay, long minId,
			long maxId, PurgeCallback callback) {
		this.plugin = plugin;
		this.paramList = paramList;
		this.purgeTickDelay = purgeTickDelay;
		this.minId = minId;
		this.maxId = maxId;
		this.callback = callback;
	}

	/**
	 * 
	 */
	@Override
	public void run() {

		if (paramList.isEmpty()) {
			return;
		}

		final ActionsQuery aq = new ActionsQuery(plugin);

		// Pull the next-in-line purge param
		final QueryParameters param = paramList.get(0);
		if (minId == 0 && maxId == 0) {
			// First run - Set the min and max IDs
			minId = aq.getMinIDForQuery(param);
			if (minId > 0) {
				maxId = aq.getMaxIDForQuery(param);
			}
		}
		boolean cycleComplete = false;
		cycleRowsAffected = 0;

		long startTime = System.nanoTime();

		// We're chunking by IDs instead of using LIMIT because
		// that should be a lot better as far as required record lock counts
		// http://mysql.rjweb.org/doc.php/deletebig
		int spread = plugin.getConfig().getInt("prism.purge.records-per-batch");
		if (spread <= 1) {
			spread = 10000;
		}
		// Delete includes id < newMinId. This ensures the maxId isn't exceeded on the final chunk
		// and also handles the case where minId == maxId (they need to be different by at least 1).
		long newMinId = Math.min(minId + spread, maxId + 1);

		// Make sure there are rows to potentially delete
		if (maxId > 0) {
			param.setMinPrimaryKey(minId);
			param.setMaxPrimaryKey(newMinId);
			cycleRowsAffected = aq.delete(param);
			plugin.total_records_affected += cycleRowsAffected;
		}
		// If done, remove rule and mark complete
		if (newMinId >= maxId) {
			paramList.remove(param);
			cycleComplete = true;
		}

        long cycleTime = (System.nanoTime() - startTime) / 1000000L; // msec
        plugin.max_cycle_time = Math.max(plugin.max_cycle_time, cycleTime);

		Prism.debug("------------------- " + param.getOriginalCommand());
		Prism.debug("minId: " + minId);
		Prism.debug("maxId: " + maxId);
		Prism.debug("newMinId: " + newMinId);
		Prism.debug("cycleRowsAffected: " + cycleRowsAffected);
		Prism.debug("cycleComplete: " + cycleComplete);
		Prism.debug("plugin.total_records_affected: " + plugin.total_records_affected);
		Prism.debug("-------------------");

		// Send cycle to callback
		callback.cycle(param, cycleRowsAffected, plugin.total_records_affected, cycleComplete, plugin.max_cycle_time);

		if (!plugin.isEnabled()) {
			Prism.log(
					"Can't schedule new purge tasks as plugin is now disabled. If you're shutting down the server, ignore me.");
			return;
		}

		// If cycle is incomplete, reschedule it
		if (!cycleComplete) {
			plugin.getPurgeManager().deleteTask = plugin.getServer().getScheduler().runTaskLaterAsynchronously(plugin,
					new PurgeTask(plugin, paramList, purgeTickDelay, newMinId, maxId, callback), purgeTickDelay);
		} else {
			// reset counts
			plugin.total_records_affected = 0;
			plugin.max_cycle_time = 0;

			if (paramList.isEmpty()) {
				return;
			}

			Prism.log("Moving on to next purge rule...");

			// schedule a new task with next param
			plugin.getPurgeManager().deleteTask = plugin.getServer().getScheduler().runTaskLaterAsynchronously(plugin,
					new PurgeTask(plugin, paramList, purgeTickDelay, callback), purgeTickDelay);
		}
	}
}