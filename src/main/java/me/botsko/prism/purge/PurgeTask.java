package me.botsko.prism.purge;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;

import java.util.concurrent.CopyOnWriteArrayList;

public class PurgeTask implements Runnable {


    private final Prism plugin;
    private final CopyOnWriteArrayList<QueryParameters> paramList;
    private final int purgeTickDelay;
    private final PurgeCallback callback;
    @SuppressWarnings("FieldCanBeLocal")
    private int cycleRowsAffected = 0;
    private long minId = 0;
    private long maxId = 0;

    /**
     * Used when we dont know the min max.
     *
     * @param plugin Prism
     * @param paramList List
     * @param purgeTickDelay int
     * @param callback Callback
     */
    @SuppressWarnings("WeakerAccess")
    public PurgeTask(Prism plugin, CopyOnWriteArrayList<QueryParameters> paramList, int purgeTickDelay,
                     PurgeCallback callback) {
        this.plugin = plugin;
        this.paramList = paramList;
        this.purgeTickDelay = purgeTickDelay;
        this.callback = callback;
    }

    /**
     * Constructor.
     *
     * @param plugin Prism
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
            long[] result = aq.getQueryExtents(param);
            minId = result[0];
            if (minId > 0) {
                maxId = result[1];
            }
        }
        boolean cycleComplete = false;
        cycleRowsAffected = 0;
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
        long startTime = System.nanoTime();
        // Make sure there are rows to potentially delete
        if (maxId > 0) {
            param.setMinPrimaryKey(minId);
            param.setMaxPrimaryKey(newMinId);
            cycleRowsAffected = aq.delete(param);
            plugin.totalRecordsAffected += cycleRowsAffected;
        }
        // If done, remove rule and mark complete
        if (newMinId >= maxId) {
            paramList.remove(param);
            cycleComplete = true;
        }

        long cycleTime = (System.nanoTime() - startTime) / 1000000L; // msec
        plugin.maxCycleTime = Math.max(plugin.maxCycleTime, cycleTime);

        Prism.debug("------------------- ");
        Prism.debug("params: " + param.getOriginalCommand());
        Prism.debug("minId: " + minId);
        Prism.debug("maxId: " + maxId);
        Prism.debug("newMinId: " + newMinId);
        Prism.debug("cycleRowsAffected: " + cycleRowsAffected);
        Prism.debug("cycleComplete: " + cycleComplete);
        Prism.debug("plugin.total_records_affected: " + plugin.totalRecordsAffected);
        Prism.debug("-------------------");

        // Send cycle to callback
        callback.cycle(param, cycleRowsAffected, plugin.totalRecordsAffected, cycleComplete, plugin.maxCycleTime);

        if (!plugin.isEnabled()) {
            Prism.log("Can't schedule new purge tasks as plugin is now disabled. "
                            + "If you're shutting down the server, ignore me.");
            return;
        }

        // If cycle is incomplete, reschedule it
        if (!cycleComplete) {
            plugin.getPurgeManager().deleteTask = plugin.getServer().getScheduler().runTaskLaterAsynchronously(plugin,
                    new PurgeTask(plugin, paramList, purgeTickDelay, newMinId, maxId, callback), purgeTickDelay);
        } else {
            // reset counts
            plugin.totalRecordsAffected = 0;
            plugin.maxCycleTime = 0;

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