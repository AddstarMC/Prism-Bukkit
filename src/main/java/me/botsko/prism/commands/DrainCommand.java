package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.events.PrismBlocksDrainEvent;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.ChatColor;

import java.util.ArrayList;
import java.util.List;

public class DrainCommand implements SubHandler {

    private final Prism plugin;

    /**
     * Constructor.
     * @param plugin Prism
     */
    public DrainCommand(Prism plugin) {
        this.plugin = plugin;
    }

    @Override
    public void handle(CallInfo call) {

        String drainType = "";
        int radius = plugin.getConfig().getInt("prism.drain.default-radius");
        if (call.getArgs().length == 3) {
            if (call.getArg(1).equalsIgnoreCase("water") || call.getArg(1).equalsIgnoreCase("lava")) {
                drainType = call.getArg(1);
            } else {
                call.getPlayer().sendMessage(
                        Prism.messenger.playerError("Invalid drain type. Must be lava, water, or left out."));
                return;
            }
            // Validate radius
            radius = validateRadius(call, call.getArg(2));
        } else if (call.getArgs().length == 2) {
            if (TypeUtils.isNumeric(call.getArg(1))) {
                radius = validateRadius(call, call.getArg(1));
            } else {
                if (call.getArg(1).equalsIgnoreCase("water") || call.getArg(1).equalsIgnoreCase("lava")) {
                    drainType = call.getArg(1);
                } else {
                    call.getPlayer().sendMessage(
                            Prism.messenger.playerError("Invalid drain type. Must be lava, water, or left out."));
                    return;
                }
            }
        }

        if (radius == 0) {
            return;
        }

        // Build seeking message
        String msg = "Looking for " + drainType + " within " + radius + " blocks.";
        if (drainType.equals("water")) {
            msg += ChatColor.GRAY + " It's just too wet.";
        } else if (drainType.equals("lava")) {
            msg += ChatColor.GRAY + " It's getting hot in here.";
        }
        call.getPlayer().sendMessage(Prism.messenger.playerHeaderMsg(msg));

        ArrayList<BlockStateChange> blockStateChanges = null;
        if (drainType.isEmpty()) {
            blockStateChanges = BlockUtils.drain(call.getPlayer().getLocation(), radius);
        } else if (drainType.equals("water")) {
            blockStateChanges = BlockUtils.drainWater(call.getPlayer().getLocation(), radius);
        } else if (drainType.equals("lava")) {
            blockStateChanges = BlockUtils.drainLava(call.getPlayer().getLocation(), radius);
        }

        if (blockStateChanges != null && !blockStateChanges.isEmpty()) {

            // @todo remove the extra space in msg
            call.getPlayer().sendMessage(Prism.messenger
                    .playerHeaderMsg("Drained " + blockStateChanges.size() + " " + drainType + " blocks."));
            call.getPlayer().sendMessage(Prism.messenger.playerSubduedHeaderMsg("Use /prism undo last if needed."));

            // Trigger the event
            final PrismBlocksDrainEvent event = new PrismBlocksDrainEvent(blockStateChanges, call.getPlayer(), radius);
            plugin.getServer().getPluginManager().callEvent(event);

        } else {
            call.getPlayer().sendMessage(Prism.messenger.playerError("Nothing found to drain within that radius."));
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }

    protected int validateRadius(CallInfo call, String radiusArg) {
        if (TypeUtils.isNumeric(radiusArg)) {
            final int _tmp_radius = Integer.parseInt(radiusArg);
            if (_tmp_radius > 0) {
                if (_tmp_radius > plugin.getConfig().getInt("prism.drain.max-radius")) {
                    call.getPlayer().sendMessage(Prism.messenger.playerError("Radius exceeds max set in config."));
                    return 0;
                } else {
                    return _tmp_radius;
                }
            } else {
                call.getPlayer().sendMessage(Prism.messenger.playerError(
                        "Radius must be greater than zero. Or leave it off to use the default."
                                + " Use /prism ? for help."));
                return 0;
            }
        } else {
            call.getPlayer().sendMessage(Prism.messenger.playerError(
                    "Radius must be a number. Or leave it off to use the default. Use /prism ? for help."));
            return 0;
        }
    }
}