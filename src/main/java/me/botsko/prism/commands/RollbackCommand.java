package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.PrismApplierCallback;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.appliers.Rollback;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;

import java.util.List;

public class RollbackCommand extends AbstractCommand {

    private final Prism plugin;

    public RollbackCommand(Prism plugin) {
        this.plugin = plugin;
    }

    @Override
    public void handle(final CallInfo call) {

        final QueryParameters parameters = PreprocessArgs.process(plugin, call.getSender(), call.getArgs(),
                PrismProcessType.ROLLBACK, 1, !plugin.getConfig().getBoolean("prism.queries.never-use-defaults"));
        if (parameters == null) {
            return;
        }
        parameters.setProcessType(PrismProcessType.ROLLBACK);
        parameters.setStringFromRawArgs(call.getArgs(), 1);
        StringBuilder defaultsReminder = checkIfDefaultUsed(parameters);
        call.getSender().sendMessage(Prism.messenger.playerSubduedHeaderMsg("Preparing results..." + defaultsReminder));
        plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> {

            final ActionsQuery aq = new ActionsQuery(plugin);
            final QueryResult results = aq.lookup(parameters, call.getSender());
            if (!results.getActionResults().isEmpty()) {

                call.getSender().sendMessage(Prism.messenger.playerHeaderMsg("Beginning rollback..."));

                // Perform rollback on the main thread
                plugin.getServer().getScheduler().runTask(plugin, () -> {
                    final Rollback rb = new Rollback(plugin, call.getSender(), results.getActionResults(),
                            parameters, new PrismApplierCallback());
                    rb.apply();
                });

            } else {
                call.getSender().sendMessage(
                        Prism.messenger.playerError("Nothing found to rollback. Try using /prism l (args) first."));
            }
        });
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return PreprocessArgs.complete(call.getSender(), call.getArgs());
    }
}