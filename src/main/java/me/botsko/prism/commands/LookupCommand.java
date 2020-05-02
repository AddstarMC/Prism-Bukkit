package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.utils.MiscUtils;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

import java.util.List;

public class LookupCommand implements SubHandler {


    private final Prism plugin;

    /**
     * Perform a lookup.
     *
     * @param plugin Prism
     */
    public LookupCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command.
     */
    @Override
    public void handle(final CallInfo call) {

        // Process and validate all of the arguments
        final QueryParameters parameters = PreprocessArgs.process(plugin, call.getSender(),
                call.getArgs(),
                PrismProcessType.LOOKUP, 1,
                !plugin.getConfig().getBoolean("prism.queries.never-use-defaults"));
        if (parameters == null) {
            return;
        }

        // Run the lookup itself in an async task so the lookup query isn't done on the main thread
        plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> {

            // determine if defaults were used
            final List<String> defaultsUsed = parameters.getDefaultsUsed();
            StringBuilder defaultsReminder = new StringBuilder();
            if (!defaultsUsed.isEmpty()) {
                defaultsReminder.append("Using defaults:");
                for (final String d : defaultsUsed) {
                    defaultsReminder.append(" ").append(d);
                }
            }

            final ActionsQuery aq = new ActionsQuery(plugin);
            final QueryResult results = aq.lookup(parameters, call.getSender());
            StringBuilder sharingWithPlayers = new StringBuilder();
            for (final CommandSender shareWith : parameters.getSharedPlayers()) {
                sharingWithPlayers.append(shareWith.getName()).append(", ");
            }
            sharingWithPlayers = new StringBuilder(sharingWithPlayers.substring(0,
                    (sharingWithPlayers.length() == 0) ? 0 : sharingWithPlayers.length() - 2));

            // Add current sender
            parameters.addSharedPlayer(call.getSender());

            for (final CommandSender player : parameters.getSharedPlayers()) {

                final boolean isSender = player.getName().equals(call.getSender().getName());

                if (!isSender) {
                    player.sendMessage(Prism.messenger
                            .playerHeaderMsg(ChatColor.YELLOW + ""
                                    + ChatColor.ITALIC
                                    + call.getSender().getName()
                                    + ChatColor.GOLD
                                    + " shared these Prism lookup logs with you:"));
                } else if (sharingWithPlayers.length() > 0) {
                    player.sendMessage(
                            Prism.messenger.playerHeaderMsg(ChatColor.GOLD
                                    + "Sharing results with players: "
                                    + ChatColor.YELLOW + ""
                                    + ChatColor.ITALIC + sharingWithPlayers));
                }

                if (!results.getActionResults().isEmpty()) {
                    player.sendMessage(Prism.messenger.playerHeaderMsg("Showing "
                            + results.getTotalResults()
                            + " results. Page 1 of "
                            + results.getTotalPages()));
                    if ((defaultsReminder.length() > 0) && isSender) {
                        player.sendMessage(Prism.messenger.playerSubduedHeaderMsg(
                                defaultsReminder.toString()));
                    }
                    final List<Handler> paginated = results.getPaginatedActionResults();
                    if (paginated != null) {
                        int resultCount = results.getIndexOfFirstResult();
                        for (final Handler a : paginated) {
                            final ActionMessage am = new ActionMessage(a);
                            if (parameters.hasFlag(Flag.EXTENDED)
                                    || plugin.getConfig()
                                    .getBoolean("prism.messenger.always-show-extended")) {
                                am.showExtended();
                            }
                            am.setResultIndex(resultCount);
                            MiscUtils.sendClickableTpRecord(am, player);
                            resultCount++;
                        }
                        MiscUtils.sendPageButtons(results, player);
                    } else {
                        player.sendMessage(Prism.messenger
                                .playerError("Pagination can't find anything. "
                                        + "Do you have the right page number?"));
                    }
                    if (parameters.hasFlag(Flag.PASTE)) {
                        StringBuilder paste = new StringBuilder();
                        for (final Handler a : results.getActionResults()) {
                            paste.append(new ActionMessage(a).getRawMessage()).append("\r\n");
                        }
                        MiscUtils.paste_results(player, paste.toString());
                    }
                } else {
                    if (defaultsReminder.length() > 0) {
                        if (isSender) {
                            player.sendMessage(Prism.messenger.playerSubduedHeaderMsg(
                                    defaultsReminder.toString()));
                        }
                    }
                    if (isSender) {
                        player.sendMessage(Prism.messenger.playerError("Nothing found."
                                + ChatColor.GRAY
                                + " Either you're missing something, or we are."));
                    }
                }
            }

            // Flush timed data
            plugin.eventTimer.printTimeRecord();

        });
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return PreprocessArgs.complete(call.getSender(), call.getArgs());
    }
}