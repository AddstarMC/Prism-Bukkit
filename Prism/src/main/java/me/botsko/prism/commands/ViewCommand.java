package me.botsko.prism.commands;

import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.utils.ChunkUtils;
import org.bukkit.Material;
import org.bukkit.block.Block;

import java.util.ArrayList;
import java.util.List;

public class ViewCommand implements SubHandler {

    private final Prism plugin;

    /**
     * Constructor.
     *
     * @param plugin Prism
     */
    public ViewCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command.
     */
    @Override
    public void handle(CallInfo call) {

        final String playerName = call.getPlayer().getName();

        if (call.getArg(1).equals("chunk")) {

            // Do they already have a view?
            if (plugin.playerActiveViews.containsKey(playerName)) {

                // Get bounding blocks
                final ArrayList<Block> blocks = plugin.playerActiveViews.get(playerName);

                // Reset to current
                ChunkUtils.resetPreviewBoundaryBlocks(call.getPlayer(), blocks);

                // Close
                Prism.messenger.sendMessage(call.getSender(),
                        Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("command-view-close")));
                plugin.playerActiveViews.remove(playerName);

            } else {

                // Get bounding blocks
                final ArrayList<Block> blocks = ChunkUtils.getBoundingBlocksAtY(
                        call.getPlayer().getLocation().getChunk(), call.getPlayer().getLocation().getBlockY());

                // Set preview blocks
                ChunkUtils.setPreviewBoundaryBlocks(call.getPlayer(), blocks, Material.GLOWSTONE);
                plugin.playerActiveViews.put(playerName, blocks);

                Prism.messenger.sendMessage(call.getSender(),
                        Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("command-view-chunks")));

            }
            return;
        }

        Prism.messenger.sendMessage(call.getSender(),
                Prism.messenger.playerError(Il8nHelper.getMessage("invalid-command")));

    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}