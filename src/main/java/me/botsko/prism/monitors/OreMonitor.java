package me.botsko.prism.monitors;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.utils.MiscUtils;
import me.botsko.prism.utils.TypeUtils;
import net.md_5.bungee.api.ChatColor;
import net.md_5.bungee.api.chat.BaseComponent;
import net.md_5.bungee.api.chat.HoverEvent;
import net.md_5.bungee.api.chat.TextComponent;
import net.md_5.bungee.api.chat.hover.content.Item;
import org.bukkit.GameMode;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.entity.Player;

import java.util.ArrayList;
import java.util.List;

public class OreMonitor {


    private final int thresholdMax = 100;

    private final Prism plugin;
    protected Player player;
    protected Block block;
    private int threshold = 1;

    /**
     * Constructor.
     * @param plugin Prism
     */
    public OreMonitor(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Process alerts.
     * @param player Player
     * @param block Block
     */
    public void processAlertsFromBlock(final Player player, final Block block) {

        if (!plugin.getConfig().getBoolean("prism.alerts.ores.enabled")) {
            return;
        }

        if (player == null || player.getGameMode().equals(GameMode.CREATIVE)) {
            return;
        }

        if (block != null && isWatched(block) && !plugin.alertedBlocks.containsKey(block.getLocation())) {

            threshold = 1;

            // identify all ore blocks on same Y axis in x/z direction
            final ArrayList<Block> matchingBlocks = new ArrayList<>();
            final ArrayList<Block> foundores = findNeighborBlocks(block.getType(), block, matchingBlocks);
            if (!foundores.isEmpty()) {

                // Determine light level
                int light = 0;
                final BlockFace[] blockFaces =
                        new BlockFace[] {BlockFace.NORTH, BlockFace.SOUTH, BlockFace.EAST, BlockFace.WEST,
                              BlockFace.UP, BlockFace.DOWN};
                for (BlockFace blockFace : blockFaces) {
                    light = Math.max(light, block.getRelative(blockFace).getLightLevel());
                    if (light >= 15) {
                        break;
                    }
                }
                light = light * 100 / 15;

                // Create alert message
                final String count = foundores.size() + (foundores.size() >= thresholdMax ? "+" : "");
                final String msg = player.getName() + " found " + count + " "
                        + getOreNiceName(block) + " " + light + "% light";
                final TextComponent component = new TextComponent(msg);
                component.setColor(getOreColor(block));
                HoverEvent hoverBlock = new HoverEvent(HoverEvent.Action.SHOW_ITEM,
                        new Item(block.getBlockData().getMaterial().getKey().toString(),
                                1, null));
                component.setHoverEvent(hoverBlock);
                plugin.getServer().getScheduler().runTaskAsynchronously(plugin, () -> {
                    // check if block placed
                    boolean wasplaced = false;

                    // Build params
                    final QueryParameters params = new QueryParameters();
                    params.setWorld(player.getWorld().getName());
                    params.addSpecificBlockLocation(block.getLocation());
                    params.addActionType("block-place");

                    final ActionsQuery aq = new ActionsQuery(plugin);
                    final QueryResult results = aq.lookup(params, player);
                    if (!results.getActionResults().isEmpty()) {
                        wasplaced = true;
                    }

                    if (!wasplaced) {
                        List<BaseComponent> send = new ArrayList<>();
                        send.add(component);
                        // Alert staff
                        plugin.alertPlayers(null, send);

                        // Log to console
                        if (plugin.getConfig().getBoolean("prism.alerts.ores.log-to-console")) {
                            Prism.log(TypeUtils.colorize(msg));
                        }

                        // Log to commands
                        List<String> commands = plugin.getConfig().getStringList("prism.alerts.ores.log-commands");
                        MiscUtils.dispatchAlert(msg, commands);
                    }
                });
            }
        }
    }

    /**
     * GetOreColor.
     *
     * @param block Block
     * @return String
     */
    private ChatColor getOreColor(Block block) {
        if (isWatched(block)) {
            ChatColor color = Prism.getAlertedOres().get(block.getType());
            if (color != null) {
                return color;
            }
        }
        return ChatColor.WHITE;
    }

    /**
     * Get Nice Name.
     * @param block Block.
     * @return String
     */
    private String getOreNiceName(Block block) {
        return block.getType().toString().replace("_", " ").toLowerCase().replace("glowing", " ");
    }

    /**
     * True if watching.
     * @param block Block
     * @return bool
     */
    private boolean isWatched(Block block) {
        return Prism.getAlertedOres().containsKey(block.getType());
    }

    /**
     * Find nearby of same type.
     * @param type Material
     * @param currBlock Block
     * @param matchingBlocks List to match.
     * @return List that matched.
     */
    private ArrayList<Block> findNeighborBlocks(Material type, Block currBlock, ArrayList<Block> matchingBlocks) {

        if (isWatched(currBlock)) {

            matchingBlocks.add(currBlock);
            final java.util.Date date = new java.util.Date();
            plugin.alertedBlocks.put(currBlock.getLocation(), date.getTime());

            for (int x = -1; x <= 1; x++) {
                for (int z = -1; z <= 1; z++) {
                    for (int y = -1; y <= 1; y++) {
                        final Block newblock = currBlock.getRelative(x, y, z);
                        // ensure it matches the type and wasn't already found
                        if (newblock.getType() == type && !matchingBlocks.contains(newblock)) {
                            threshold++;
                            if (threshold <= thresholdMax) {
                                findNeighborBlocks(type, newblock, matchingBlocks);
                            }
                        }
                    }
                }
            }
        }

        return matchingBlocks;

    }
}
