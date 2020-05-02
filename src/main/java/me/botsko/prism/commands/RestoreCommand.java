package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.Previewable;
import me.botsko.prism.appliers.PrismApplierCallback;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.appliers.Restore;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import org.bukkit.entity.Player;

import java.util.List;

public class RestoreCommand extends AbstractCommand {

    private final Prism plugin;

    public RestoreCommand(Prism plugin) {
        this.plugin = plugin;
    }

    @Override
    public void handle(final CallInfo call) {

        final QueryParameters parameters = PreprocessArgs.process(plugin, call.getSender(), call.getArgs(),
                PrismProcessType.RESTORE, 1, !plugin.getConfig().getBoolean("prism.queries.never-use-defaults"));
        if (parameters == null) {
            return;
        }
        parameters.setProcessType(PrismProcessType.RESTORE);
        parameters.setStringFromRawArgs(call.getArgs(), 1);
        StringBuilder defaultsReminder = checkIfDefaultUsed(parameters);
        call.getSender().sendMessage(Prism.messenger.playerSubduedHeaderMsg("Preparing results..." + defaultsReminder));

        plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> {

            final ActionsQuery aq = new ActionsQuery(plugin);
            final QueryResult results = aq.lookup(parameters, call.getSender());
            if (!results.getActionResults().isEmpty()) {

                call.getSender().sendMessage(Prism.messenger.playerHeaderMsg("Restoring changes..."));

                // Inform nearby players
                if (call.getSender() instanceof Player) {
                    final Player player = (Player) call.getSender();
                    plugin.notifyNearby(player, parameters.getRadius(),
                            player.getDisplayName() + " is re-applying block changes nearby. Just so you know.");
                }

                // Perform restore on the main thread
                plugin.getServer().getScheduler().runTask(plugin, () -> {
                    final Previewable rs = new Restore(plugin, call.getSender(), results.getActionResults(),
                            parameters, new PrismApplierCallback());
                    rs.apply();
                });

            } else {
                call.getSender().sendMessage(
                        Prism.messenger.playerError("Nothing found to restore. Try using /prism l (args) first."));
            }
        });
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return PreprocessArgs.complete(call.getSender(), call.getArgs());
    }
}