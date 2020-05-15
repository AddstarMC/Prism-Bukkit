package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.purge.PurgeTask;
import me.botsko.prism.purge.SenderPurgeCallback;
import org.bukkit.ChatColor;
import org.bukkit.scheduler.BukkitTask;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class DeleteCommand extends AbstractCommand {

    private final Prism plugin;
    private BukkitTask deleteTask;

    /**
     * Constructor.
     *
     * @param plugin Prism
     */
    public DeleteCommand(Prism plugin) {
        this.plugin = plugin;
    }

    @Override
    public void handle(final CallInfo call) {

        // Allow for canceling tasks
        if (call.getArgs().length > 1 && call.getArg(1).equals("cancel")) {
            if (plugin.getPurgeManager().deleteTask != null) {
                plugin.getPurgeManager().deleteTask.cancel();
                call.getSender().sendMessage(Prism.messenger.playerMsg("Current purge tasks have been canceled."));
            } else {
                call.getSender().sendMessage(Prism.messenger.playerError("No purge task is currently running."));
            }
            return;
        }

        // Allow for wiping live queue
        if (call.getArgs().length > 1 && call.getArg(1).equals("queue")) {
            if (RecordingQueue.getQueue().size() > 0) {
                Prism.log("User " + call.getSender().getName()
                        + " wiped the live queue before it could be written to the database. "
                        + RecordingQueue.getQueue().size() + " events lost.");
                RecordingQueue.getQueue().clear();
                call.getSender().sendMessage(Prism.messenger.playerSuccess("Unwritten data in queue cleared."));
            } else {
                call.getSender().sendMessage(Prism.messenger.playerError("Event queue is empty, nothing to wipe."));
            }
            return;
        }

        // Process and validate all of the arguments
        final QueryParameters parameters = PreprocessArgs.process(plugin, call.getSender(), call.getArgs(),
                PrismProcessType.DELETE, 1, !plugin.getConfig().getBoolean("prism.queries.never-use-defaults"));
        if (parameters == null) {
            return;
        }
        parameters.setStringFromRawArgs(call.getArgs(), 1);

        StringBuilder defaultsReminder = checkIfDefaultUsed(parameters);
        if (parameters.getFoundArgs().size() > 0) {

            call.getSender().sendMessage(Prism.messenger.playerSubduedHeaderMsg("Purging data..." + defaultsReminder));
            call.getSender().sendMessage(Prism.messenger
                    .playerHeaderMsg("Starting purge cycle." + ChatColor.GRAY + " No one will ever know..."));

            plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> {
                int purgeTickDelay = plugin.getConfig().getInt("prism.purge.batch-tick-delay");
                if (purgeTickDelay < 1) {
                    purgeTickDelay = 20;
                }

                // build callback
                final SenderPurgeCallback callback = new SenderPurgeCallback();
                callback.setSender(call.getSender());

                // add to an arraylist so we're consistent
                final CopyOnWriteArrayList<QueryParameters> paramList = new CopyOnWriteArrayList<>();
                paramList.add(parameters);

                final ActionsQuery aq = new ActionsQuery(plugin);
                final long[] extents = aq.getQueryExtents(parameters);
                final long minId = extents[0];
                final long maxId = extents[1];
                Prism.log(
                        "Beginning prism database purge cycle. Will be performed in batches so "
                                + "we don't tie up the db...");
                deleteTask = plugin.getServer().getScheduler().runTaskAsynchronously(plugin,
                        new PurgeTask(plugin, paramList, purgeTickDelay, minId, maxId, callback));
            });
        } else {
            call.getSender().sendMessage(Prism.messenger.playerError("You must supply at least one parameter."));
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return PreprocessArgs.complete(call.getSender(), call.getArgs());
    }
}