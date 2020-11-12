package me.botsko.prism.appliers;

import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.utils.EntityUtils;
import me.botsko.prism.utils.block.Utilities;
import org.bukkit.command.CommandSender;

import java.util.ArrayList;
import java.util.Collection;

public class Rollback extends Preview {

    /**
     * Constructor.
     *
     * @param plugin Prism.
     */
    public Rollback(Prism plugin, CommandSender sender, Collection<Handler> results, QueryParameters parameters,
                    ApplierCallback callback) {
        super(plugin, sender, results, parameters, callback);
    }

    @Override
    public void preview() {
        setIsPreview(true);
        apply();
    }

    /**
     * {@inheritDoc}
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
                        Prism.messenger.sendMessage(player,Prism.messenger
                                .playerHeaderMsg(Il8nHelper.getMessage("fire-extinguished-sucess")));
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
                        Prism.messenger.sendMessage(player,Prism.messenger.playerHeaderMsg(
                              Il8nHelper.formatMessage("rollback-removedDrops",removed)));
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
                Prism.messenger.sendMessage(player,
                        Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("command-drain-done")));
            }
        }

        // Give the results to the change queue
        super.apply();

    }
}