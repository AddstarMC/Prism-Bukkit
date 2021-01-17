package me.botsko.prism.commands;

import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.appliers.PreviewSession;
import me.botsko.prism.appliers.Previewable;
import me.botsko.prism.appliers.PrismApplierCallback;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.appliers.Restore;
import me.botsko.prism.appliers.Rollback;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.utils.MiscUtils;
import net.kyori.adventure.audience.Audience;

import java.util.ArrayList;
import java.util.List;

public class PreviewCommand extends AbstractCommand {

    private final Prism plugin;

    private final List<String> secondaries;

    /**
     * Contructor.
     *
     * @param plugin Prism
     */
    public PreviewCommand(Prism plugin) {
        this.plugin = plugin;
        secondaries = new ArrayList<>();
        secondaries.add("apply");
        secondaries.add("cancel");
        secondaries.add("rollback");
        secondaries.add("restore");
        secondaries.add("rb");
        secondaries.add("rs");
    }

    @Override
    public void handle(final CallInfo call) {
        final Audience audience = Prism.getAudiences().sender(call.getPlayer());
        if (call.getArgs().length >= 2) {

            if (call.getArg(1).equalsIgnoreCase("apply")) {
                if (plugin.playerActivePreviews.containsKey(call.getPlayer().getName())) {
                    final PreviewSession previewSession = plugin.playerActivePreviews.get(call.getPlayer().getName());
                    previewSession.getPreviewer().apply_preview();
                    plugin.playerActivePreviews.remove(call.getPlayer().getName());
                } else {
                    Prism.messenger.sendMessage(call.getPlayer(),
                          Prism.messenger.playerError("You have no preview pending."));
                }
                return;
            }

            if (call.getArg(1).equalsIgnoreCase("cancel")) {
                if (plugin.playerActivePreviews.containsKey(call.getPlayer().getName())) {
                    final PreviewSession previewSession = plugin.playerActivePreviews.get(call.getPlayer().getName());
                    previewSession.getPreviewer().cancel_preview();
                    plugin.playerActivePreviews.remove(call.getPlayer().getName());
                } else {
                    Prism.messenger.sendMessage(call.getPlayer(),Prism.messenger.playerError(
                          Il8nHelper.getMessage("preview-none-pending")));
                }
                return;
            }

            // Ensure no current preview is waiting
            if (plugin.playerActivePreviews.containsKey(call.getPlayer().getName())) {
                Prism.messenger.sendMessage(call.getPlayer(),Prism.messenger
                        .playerError(Il8nHelper.getMessage("preview-pending")));
                return;
            }

            if (call.getArg(1).equalsIgnoreCase("rollback") || call.getArg(1).equalsIgnoreCase("restore")
                    || call.getArg(1).equalsIgnoreCase("rb") || call.getArg(1).equalsIgnoreCase("rs")) {

                final QueryParameters parameters = PreprocessArgs.process(plugin, call.getPlayer(), call.getArgs(),
                        PrismProcessType.ROLLBACK, 2,
                        !plugin.getConfig().getBoolean("prism.queries.never-use-defaults"));
                if (parameters == null) {
                    return;
                }
                parameters.setStringFromRawArgs(call.getArgs(), 1);

                if (parameters.getActionTypes().containsKey("world-edit")) {
                    Prism.messenger.sendMessage(call.getPlayer(),Prism.messenger
                            .playerError(Il8nHelper.getMessage("preview-worldedit-unsupported")));
                    return;
                }
                StringBuilder defaultsReminder = checkIfDefaultUsed(parameters);
                Prism.messenger.sendMessage(call.getPlayer(),Prism.messenger
                        .playerSubduedHeaderMsg(
                                Il8nHelper.getMessage("queryparameter.defaults.prefix",
                                        defaultsReminder.toString())));
                plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> {

                    // Perform preview
                    final ActionsQuery aq = new ActionsQuery(plugin);
                    final QueryResult results = aq.lookup(parameters, call.getPlayer());

                    // Rollback
                    if (call.getArg(1).equalsIgnoreCase("rollback")
                            || call.getArg(1).equalsIgnoreCase("rb")) {
                        handleRollBack(call, parameters, results, audience);
                        assert (parameters.getProcessType() == PrismProcessType.ROLLBACK); //todo remove debug
                    }
                    // Restore
                    if (call.getArg(1).equalsIgnoreCase("restore")
                            || call.getArg(1).equalsIgnoreCase("rs")) {
                        handleRestore(call, parameters, results, audience);
                        assert (parameters.getProcessType() == PrismProcessType.RESTORE);//todo remove debug
                    }
                });
                return;
            }
            Prism.messenger.sendMessage(call.getPlayer(),
                  Prism.messenger.playerError(Il8nHelper.getMessage("invalid-command")));
        }
    }

    private void handleRestore(CallInfo call, QueryParameters parameters, QueryResult results, Audience audience) {
        parameters.setProcessType(PrismProcessType.RESTORE);
        if (!results.getActionResults().isEmpty()) {

            Prism.messenger.sendMessage(call.getPlayer(),
                  Prism.messenger.playerHeaderMsg(
                    Il8nHelper.getMessage("preview-apply-start")));

            // Perform preview on the main thread
            plugin.getServer().getScheduler().runTask(plugin, () -> {
                final Previewable rs = new Restore(plugin, call.getPlayer(),
                        results.getActionResults(), parameters, new PrismApplierCallback());
                rs.preview();
            });
        } else {
            Prism.messenger.sendMessage(call.getPlayer(),
                  Prism.messenger.playerError(Il8nHelper.getMessage("preview-no-actions")));
        }
    }


    private void handleRollBack(final CallInfo call, final QueryParameters parameters,
                                final QueryResult results, final Audience audience) {
        parameters.setProcessType(PrismProcessType.ROLLBACK);
        if (!results.getActionResults().isEmpty()) {

            audience.sendMessage(Prism.messenger.playerHeaderMsg(
                    Il8nHelper.getMessage("preview-apply-start")));

            // Perform preview on the main thread
            plugin.getServer().getScheduler().runTask(plugin, () -> {
                final Previewable rs = new Rollback(plugin, call.getPlayer(),
                        results.getActionResults(), parameters, new PrismApplierCallback());
                rs.preview();
            });
        } else {
            Prism.messenger.sendMessage(call.getPlayer(),
                  Prism.messenger.playerError("Nothing found to preview."));
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        if (call.getArgs().length == 2) {
            return MiscUtils.getStartingWith(call.getArg(1), secondaries);
        }
        return PreprocessArgs.complete(call.getSender(), call.getArgs());
    }
}