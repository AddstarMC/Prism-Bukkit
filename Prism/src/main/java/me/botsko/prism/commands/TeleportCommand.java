package me.botsko.prism.commands;

import io.papermc.lib.PaperLib;
import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.api.actions.Handler;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.utils.TypeUtils;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.World;
import org.bukkit.entity.Player;

import java.util.List;

public class TeleportCommand implements SubHandler {

    private final Prism plugin;

    /**
     * TeleportCommand.
     *
     * @param plugin Prism.
     */
    TeleportCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command.
     */
    @Override
    public void handle(CallInfo call) {

        // Is there anything even stored to paginate?
        String keyName = "console";
        if (call.getSender() instanceof Player) {
            keyName = call.getSender().getName();
        }
        if (!plugin.cachedQueries.containsKey(keyName) && !call.getArg(1).contains("id:")) {
            Prism.messenger.sendMessage(call.getSender(), Prism.messenger.playerError(
                    "There's no saved query to use results from. Maybe they expired? Try your lookup again."));
            return;
        }

        // Parse the incoming ident
        String ident = call.getArg(1);
        if (ident.contains("id:")) {
            ident = ident.replace("id:", "");
        }

        // Determine result index to tp to - either an id, or the next/previous
        // id
        long recordId;
        if (ident.equals("next") || ident.equals("prev")) {
            // Get stored results
            final QueryResult results = plugin.cachedQueries.get(keyName);
            recordId = results.getLastTeleportIndex();
            recordId = (recordId == 0 ? 1 : recordId);
            if (recordId > 0) {
                if (ident.equals("next")) {
                    recordId++;
                } else {
                    if (recordId > 1) {
                        recordId--;
                    }
                }
            }
        } else {
            if (!TypeUtils.isNumeric(ident)) {
                Prism.messenger.sendMessage(call.getPlayer(), Prism.messenger
                        .playerError("You must provide a numeric result number or record ID to teleport to."));
                return;
            }
            recordId = Integer.parseInt(ident);
            if (recordId <= 0) {
                Prism.messenger.sendMessage(call.getPlayer(),
                        Prism.messenger.playerError("Result number or record ID must be greater than zero."));
                return;
            }
        }

        // If a record id provided, re-query the database
        final Handler destinationAction;
        if (call.getArg(1).contains("id:")) {

            // Build params
            final QueryParameters params = new QueryParameters();
            params.setWorld(call.getPlayer().getWorld().getName());
            params.setId(recordId);

            // Query
            final ActionsQuery aq = new ActionsQuery(plugin);
            final QueryResult results = aq.lookup(params, call.getPlayer());
            if (results.getActionResults().isEmpty()) {
                Prism.messenger.sendMessage(call.getPlayer(),
                        Prism.messenger.playerError("No records exists with this ID."));
                return;
            }

            // Get the first result
            destinationAction = results.getActionResults().get(0);

        } else {

            // Get stored results
            final QueryResult results = plugin.cachedQueries.get(keyName);

            if (recordId > results.getActionResults().size()) {
                Prism.messenger.sendMessage(call.getPlayer(), Prism.messenger.playerError(
                        "No records exists at this index. Did you mean /pr tp id:" + recordId + " instead?"));
                return;
            }

            final int key = (int) (recordId - 1);

            // Get the result index specified
            destinationAction = results.getActionResults().get(key);

            // Refresh the query time and replace
            results.setQueryTime();
            results.setLastTeleportIndex(recordId);
            plugin.cachedQueries.replace(keyName, results);

        }

        if (destinationAction != null) {
            final World world = destinationAction.getLoc().getWorld();
            if (world == null) {
                Prism.messenger.sendMessage(call.getPlayer(),
                        Prism.messenger.playerError("Action record occurred in world we can't find anymore."));
                return;
            }
            if (PaperLib.isPaper()) {
                PaperLib.teleportAsync(call.getPlayer(), destinationAction.getLoc())
                        .thenAccept(
                              success -> sendTeleportCompleteMessage(success, call.getPlayer(), destinationAction));
            } else {
                sendTeleportCompleteMessage(call.getPlayer().teleport(destinationAction.getLoc()), call.getPlayer(),
                        destinationAction);
            }
        }
    }

    private void sendTeleportCompleteMessage(boolean success, Player player, Handler destinationAction) {
        if (success) {
            Prism.messenger.sendMessage(player, Prism.messenger.playerSubduedHeaderMsg(
                    Il8nHelper.getMessage("teleport.complete")
                            .replaceText("<actionType>",
                                    Component.text(
                                            destinationAction.getActionType().getName()).color(NamedTextColor.WHITE))
                            .replaceText("<source>",
                                    Component.text(destinationAction.getSourceName()).color(NamedTextColor.WHITE))
                            .replaceText("<timeSince>",
                                    Component.text(destinationAction.getTimeSince()).color(NamedTextColor.WHITE))));
        } else {
            Prism.messenger.sendMessage(player,
                    Prism.messenger.playerError("Prism teleportation failed"));
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}