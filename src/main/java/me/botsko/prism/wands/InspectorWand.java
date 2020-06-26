package me.botsko.prism.wands;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.utils.block.Utilities;
import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;

import java.util.ArrayList;
import java.util.Collection;

public class InspectorWand extends QueryWandBase {

    /**
     * Constructor.
     * @param plugin Prism
     */
    public InspectorWand(Prism plugin) {
        super(plugin);
    }


    @Override
    public void playerRightClick(Player player, Entity entity) {
    }

    @Override
    public void playerLeftClick(Player player, Location loc) {
        showLocationHistory(player, loc);
    }

    @Override
    public void playerRightClick(Player player, Location loc) {
        showLocationHistory(player, loc);
    }

    /**
     * Show the Location History for a player.
     *
     * @param player Player
     * @param loc    Location
     */
    private void showLocationHistory(final Player player, final Location loc) {

        final Block block = loc.getBlock();
        final Block sibling = Utilities.getSiblingForDoubleLengthBlock(block);
        plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> {

            // Build params
            QueryParameters params;

            try {
                params = parameters.clone();
            } catch (final CloneNotSupportedException ex) {
                params = new QueryParameters();
                player.sendMessage(Prism.messenger
                        .playerError("Error retrieving parameters. Checking with default parameters."));
            }
            params.setWorld(player.getWorld().getName());
            params.setSpecificBlockLocation(loc);

            // Do we need a second location? (For beds, doors, etc)
            if (sibling != null) {
                params.addSpecificBlockLocation(sibling.getLocation());
            }

            // Ignoring any actions via config?
            if (params.getActionTypes().size() == 0) {
                @SuppressWarnings("unchecked") final Collection<String> ignoreActions =
                        (ArrayList<String>) plugin.getConfig().getList("prism.wands.inspect.ignore-actions");
                if (ignoreActions != null && !ignoreActions.isEmpty()) {
                    for (final String ignore : ignoreActions) {
                        params.addActionType(ignore, MatchRule.EXCLUDE);
                    }
                }
            }
            final QueryResult results = getResult(params, player);
            if (!results.getActionResults().isEmpty()) {

                final String blockname = Prism.getItems().getAlias(block.getType(), block.getBlockData());
                player.sendMessage(Prism.messenger.playerHeaderMsg(ChatColor.GOLD + "--- Inspecting " + blockname
                        + " at " + loc.getBlockX() + " " + loc.getBlockY() + " " + loc.getBlockZ() + " ---"));
                if (results.getActionResults().size() > 5) {
                    player.sendMessage(Prism.messenger.playerHeaderMsg("Showing " + results.getTotalResults()
                            + " results. Page 1 of " + results.getTotalPages()));
                }
                for (final me.botsko.prism.actions.Handler a : results.getPaginatedActionResults()) {
                    final ActionMessage am = new ActionMessage(a);
                    if (parameters.hasFlag(Flag.EXTENDED)
                            || plugin.getConfig().getBoolean("prism.messenger.always-show-extended")) {
                        am.showExtended();
                    }
                    player.sendMessage(Prism.messenger.playerMsg(am.getMessage()));
                }
            } else {
                final String space_name = (block.getType().equals(Material.AIR) ? "space"
                        : block.getType().toString().replaceAll("_", " ").toLowerCase()
                        + (block.getType().toString().endsWith("BLOCK") ? "" : " block"));
                player.sendMessage(Prism.messenger.playerError("No history for this " + space_name + " found."));
            }
        });
    }

}
