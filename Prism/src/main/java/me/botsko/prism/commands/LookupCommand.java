package me.botsko.prism.commands;

import me.botsko.prism.Il8nHelper;
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
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import net.kyori.adventure.text.format.TextDecoration;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

import java.util.List;
import java.util.regex.Pattern;

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
                defaultsReminder.append(Il8nHelper.getRawMessage("queryparameter.defaults.prefix"));
                for (final String d : defaultsUsed) {
                    defaultsReminder.append(" ").append(d);
                }
            }

            final ActionsQuery aq = new ActionsQuery(plugin);
            final QueryResult results = aq.lookup(parameters, call.getSender());
            final StringBuilder sharingWithPlayers = new StringBuilder();
            for (final CommandSender shareWith : parameters.getSharedPlayers()) {
                sharingWithPlayers.append(shareWith.getName()).append(", ");
            }
            if (sharingWithPlayers.length() > 0) {
                sharingWithPlayers.delete(sharingWithPlayers.lastIndexOf(","), sharingWithPlayers.length());
            }
            final String playersList = sharingWithPlayers.toString();
            // Add current sender
            parameters.addSharedPlayer(call.getSender());

            for (final CommandSender player : parameters.getSharedPlayers()) {

                final boolean isSender = player.getName().equals(call.getSender().getName());

                if (!isSender) {
                    Prism.messenger.sendMessage(player, Prism.messenger
                            .playerHeaderMsg(
                                    Il8nHelper.getMessage("lookup-share-message")
                                            .color(NamedTextColor.GOLD)
                                            .replaceText(Pattern.compile("<sender>"), builder ->
                                                    Component.text().content(call.getSender().getName())
                                                            .color(NamedTextColor.YELLOW)
                                                            .decoration(TextDecoration.ITALIC,
                                                                    TextDecoration.State.TRUE), 1)));
                } else if (sharingWithPlayers.length() > 0) {
                    Component component = Il8nHelper.getMessage("lookup-share-to-message")
                            .color(NamedTextColor.GOLD)
                            .replaceText(Pattern.compile("<players>"), builder ->
                                    Component.text().content(playersList)
                                            .color(NamedTextColor.YELLOW)
                                            .decoration(TextDecoration.ITALIC,
                                                    TextDecoration.State.TRUE), 1);
                    Prism.messenger.sendMessage(call.getSender(), Prism.messenger.playerHeaderMsg(component));
                }
                if (!results.getActionResults().isEmpty()) {
                    Prism.messenger.sendMessage(player,
                            Prism.messenger.playerHeaderMsg(
                                    Il8nHelper.formatMessage("lookup-header-message",
                                            results.getTotalResults(), 1, results.getTotalPages())));
                    if ((defaultsReminder.length() > 0) && isSender) {
                        Prism.messenger.sendMessage(player, Prism.messenger.playerSubduedHeaderMsg(
                                Component.text(defaultsReminder.toString())));
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
                        Prism.messenger.sendMessage(player, Prism.messenger
                                .playerError(Il8nHelper.getMessage("no-pagination-found")));
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
                            Prism.messenger.sendMessage(player, Prism.messenger.playerSubduedHeaderMsg(
                                    Component.text(defaultsReminder.toString())));
                        }
                    }
                    if (isSender) {
                        Prism.messenger.sendMessage(player, Prism.messenger.playerError("Nothing found."
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