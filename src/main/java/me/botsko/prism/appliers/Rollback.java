package me.botsko.prism.appliers;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.utils.EntityUtils;
import me.botsko.prism.utils.block.Utilities;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

import java.util.ArrayList;
import java.util.Collection;

public class Rollback extends Preview {

    /**
     * @param plugin
     * @return
     */
    public Rollback(Prism plugin, CommandSender sender, Collection<Handler> results, QueryParameters parameters,
                    ApplierCallback callback) {
        super(plugin, sender, results, parameters, callback);
    }

    /**
     * Set preview move and then do a rollback
     *
     * @return
     */
    @Override
    public void preview() {
        setIsPreview(true);
        apply();
    }

    /**
     *
     */
    @Override
    public void apply() {

        if (player != null) {

            // Remove any fire at this location
            if (plugin.getConfig().getBoolean("prism.appliers.remove-fire-on-burn-rollback")
                    && parameters.getActionTypes().containsKey("block-burn")) {
                if (!parameters.hasFlag(Flag.NO_EXT)) {
                    final ArrayList<BlockStateChange> blockStateChanges = Utilities.extinguish(player.getLocation(),
                            parameters.getRadius());
                    if (!blockStateChanges.isEmpty()) {
                        player.sendMessage(Prism.messenger
                                .playerHeaderMsg("Extinguishing fire!" + ChatColor.GRAY + " Like a boss."));
                    }
                }
            }

            // Remove item drops in this radius
            if (plugin.getConfig().getBoolean("prism.appliers.remove-drops-on-explode-rollback")
                    && (parameters.getActionTypes().containsKey("tnt-explode")
                    || parameters.getActionTypes().containsKey("creeper-explode"))) {
                if (!parameters.hasFlag(Flag.NO_ITEMCLEAR)) {
                    final int removed = EntityUtils.removeNearbyItemDrops(player, parameters.getRadius());
                    if (removed > 0) {
                        player.sendMessage(Prism.messenger.playerHeaderMsg(
                                "Removed " + removed + " drops in affected area." + ChatColor.GRAY + " Like a boss."));
                    }
                }
            }

            // Remove any liquid at this location
            ArrayList<BlockStateChange> drained = null;
            if (parameters.hasFlag(Flag.DRAIN)) {
                drained = Utilities.drain(player.getLocation(), parameters.getRadius());
            }
            if (parameters.hasFlag(Flag.DRAIN_LAVA)) {
                drained = Utilities.drainLava(player.getLocation(), parameters.getRadius());
            }
            if (parameters.hasFlag(Flag.DRAIN_WATER)) {
                drained = Utilities.drainWater(player.getLocation(), parameters.getRadius());
            }
            if (drained != null && drained.size() > 0) {
                player.sendMessage(
                        Prism.messenger.playerHeaderMsg("Draining liquid!" + ChatColor.GRAY + " Like a boss."));
            }
        }

        // Give the results to the changequeue
        super.apply();

    }
}