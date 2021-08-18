package me.botsko.prism.listeners;

import com.google.common.cache.Cache;
import com.google.common.cache.CacheBuilder;
import me.botsko.prism.Prism;
import me.botsko.prism.PrismLogHandler;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.api.actions.ActionType;
import me.botsko.prism.utils.MaterialTag;
import me.botsko.prism.utils.block.Utilities;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.BlockState;
import org.bukkit.block.DoubleChest;
import org.bukkit.block.Jukebox;
import org.bukkit.block.Sign;
import org.bukkit.block.data.type.Chest;
import org.bukkit.block.data.type.Chest.Type;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.block.BlockBreakEvent;
import org.bukkit.event.block.BlockBurnEvent;
import org.bukkit.event.block.BlockDispenseEvent;
import org.bukkit.event.block.BlockExplodeEvent;
import org.bukkit.event.block.BlockFadeEvent;
import org.bukkit.event.block.BlockFormEvent;
import org.bukkit.event.block.BlockFromToEvent;
import org.bukkit.event.block.BlockIgniteEvent;
import org.bukkit.event.block.BlockPistonExtendEvent;
import org.bukkit.event.block.BlockPistonRetractEvent;
import org.bukkit.event.block.BlockPlaceEvent;
import org.bukkit.event.block.BlockSpreadEvent;
import org.bukkit.event.block.LeavesDecayEvent;
import org.bukkit.event.block.SignChangeEvent;
import org.bukkit.event.player.PlayerBedEnterEvent;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.ItemStack;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.function.BiConsumer;
import java.util.function.Consumer;

public class PrismBlockEvents extends BaseListener {

    private final Cache<Location, PlayerBed> weakCache = CacheBuilder
            .newBuilder()
            .expireAfterWrite(30, TimeUnit.SECONDS)
            .build();

    /**
     * Constructor.
     *
     * @param plugin Prism.
     */
    public PrismBlockEvents(Prism plugin) {
        super(plugin);
    }

    /**
     * Process over each item.
     * @param block Block
     * @param callback callback that consumes items/integer pairs.
     */
    void forEachItem(Block block, BiConsumer<ItemStack, Integer> callback) {
        if (block.getType().equals(Material.JUKEBOX)) {
            final Jukebox jukebox = (Jukebox) block.getState();
            final Material playing = jukebox.getPlaying();
            if (playing.equals(Material.AIR)) {
                return;
            }
            final ItemStack i = new ItemStack(jukebox.getPlaying(), 1);
            callback.accept(i, 0);
            return;
        }
        if (block.getState() instanceof InventoryHolder) {
            InventoryHolder container = (InventoryHolder) block.getState();

            if (container instanceof DoubleChest) {
                if (((Chest) block.getBlockData()).getType() == Type.LEFT) {
                    container = ((DoubleChest) container).getLeftSide();
                } else {
                    container = ((DoubleChest) container).getRightSide();
                }
            }

            int slot = 0;
            if (container != null) {
                for (final ItemStack i : container.getInventory().getContents()) {
                    // when double chests are broken, they record *all* contents
                    // even though only half of the chest breaks.
                    if ((block.getType().equals(Material.CHEST) || block.getType().equals(Material.TRAPPED_CHEST))
                            && slot > 26) {
                        break;
                    }
                    // record item
                    if (i != null) {
                        callback.accept(i, slot);
                    }
                    slot++;
                }
            }
        }
    }

    private void logBlockRelationshipsForBlock(Player player, Block block) {
        relatedBlockCallback(block, b -> RecordingQueue.addToQueue(
                ActionFactory.createBlock(ActionType.BLOCK_BREAK, b, player)), s -> plugin.preplannedBlockFalls.put(
                        s, player.getUniqueId().toString()));
    }

    void logBlockRelationshipsForBlock(String nonPlayer, Block block) {
        relatedBlockCallback(block, b -> RecordingQueue.addToQueue(
                ActionFactory.createBlock(ActionType.BLOCK_BREAK, b, nonPlayer)), s -> plugin.preplannedBlockFalls.put(
                        s, nonPlayer));
    }

    private void relatedBlockCallback(Block block, Consumer<Block> breakCallback, Consumer<String> fallCallback) {

        if (MaterialTag.DOORS.isTagged(block.getType())) {
            return;
        }

        // Find a list of all blocks above this block that we know will fall.
        final ArrayList<Block> falling_blocks = Utilities.findFallingBlocksAboveBlock(block);
        if (falling_blocks.size() > 0) {
            for (final Block b : falling_blocks) {
                breakCallback.accept(b);
            }
        }

        // Some blocks will essentially never have attachments - not
        // even worth spending time looking for them.
        // SUGAR CANE is not a solid but does have top face attached
        if (!block.getType().isSolid() && !block.getType().equals(Material.SUGAR_CANE)) {
            return;
        }

        // if it's a piston, the base will break without a physics events
        if (block.getType().equals(Material.PISTON_HEAD) || block.getType().equals(Material.MOVING_PISTON)) {
            final ArrayList<Block> pistonBases = Utilities.findSideFaceAttachedBlocks(block);
            if (pistonBases.size() > 0) {
                for (final Block p : pistonBases) {
                    breakCallback.accept(p);
                }
            }
        }

        // Find a list of side-face attached blocks that will detach
        ArrayList<Block> detachedBlocks = Utilities.findSideFaceAttachedBlocks(block);
        if (detachedBlocks.size() > 0) {
            for (final Block b : detachedBlocks) {
                breakCallback.accept(b);
            }
        }

        // Find a list of top-side attached blocks that will detach
        detachedBlocks = Utilities.findTopFaceAttachedBlocks(block);
        if (detachedBlocks.size() > 0) {
            for (final Block b : detachedBlocks) {
                breakCallback.accept(b);
            }
        }

        // Find a list of all hanging entities on this block
        final ArrayList<Entity> hanging = Utilities.findHangingEntities(block);
        if (hanging.size() > 0) {
            for (final Entity e : hanging) {
                final String coord_key = e.getLocation().getBlockX() + ":" + e.getLocation().getBlockY() + ":"
                        + e.getLocation().getBlockZ();
                fallCallback.accept(coord_key);
            }
        }
    }

    /**
     * Handle a block break event.
     * @param event BlockBreakEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockBreak(final BlockBreakEvent event) {

        final Player player = event.getPlayer();
        final Block block = Utilities.getBaseBlock(event.getBlock());

        if (block.getType().equals(Material.AIR)) {
            return;
        }

        // Run ore find alerts
        if (!player.hasPermission("prism.alerts.ores.ignore") && !player.hasPermission("prism.alerts.ignore")) {
            plugin.oreMonitor.processAlertsFromBlock(player, block, "prism.alerts.ores");
        }

        if (!Prism.getIgnore().event(ActionType.BLOCK_BREAK, player)) {
            return;
        }

        // log items removed from container
        // note: done before the container so a "rewind" for rollback will work
        // properly
        // logItemRemoveFromDestroyedContainer( player, block );
        forEachItem(block, (i, s) -> RecordingQueue.addToQueue(
                ActionFactory.createItemStack(ActionType.ITEM_REMOVE, i, i.getAmount(), 0, null,
                block.getLocation(), player)));

        // Change handling a bit if it's a long block
        /*
         * final Block sibling = BlockUtils.getSiblingForDoubleLengthBlock(block);
         *
         * if (sibling != null && !block.getType().equals(Material.CHEST) &&
         * !block.getType().equals(Material.TRAPPED_CHEST)) {
         *
         * block = sibling; }
         */

        RecordingQueue.addToQueue(ActionFactory.createBlock(ActionType.BLOCK_BREAK, block, player));

        // check for block relationships
        logBlockRelationshipsForBlock(player, block);

        // if obsidian, log portal blocks
        if (block.getType().equals(Material.OBSIDIAN)) {
            final ArrayList<Block> blocks = Utilities.findConnectedBlocksOfType(Material.NETHER_PORTAL, block, null);
            if (!blocks.isEmpty()) {
                // Only log 1 portal break, we don't need all 8
                RecordingQueue.addToQueue(ActionFactory.createBlock(ActionType.BLOCK_BREAK, blocks.get(0), player));
            }
        }

        // Pass to the break alerter
        if (!player.hasPermission("prism.alerts.use.break.ignore")
                && !player.hasPermission("prism.alerts.ignore")) {
            plugin.useMonitor.alertOnBlockBreak(player, event.getBlock(), "prism.alerts.use.break");
        }
    }

    /**
     * Handle BlockPlaceEvent.
     * @param event BlockPlaceEvent.
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockPlace(final BlockPlaceEvent event) {

        final Player player = event.getPlayer();
        final Block block = event.getBlock();

        if (!Prism.getIgnore().event(ActionType.BLOCK_PLACE, player)) {
            return;
        }

        if (block.getType().equals(Material.AIR)) {
            return;
        }

        final BlockState s = event.getBlockReplacedState();

        // TODO: old and new appear flipped compared to other actions... check
        RecordingQueue.addToQueue(ActionFactory.createBlockChange(ActionType.BLOCK_PLACE, block.getLocation(),
                s.getType(), s.getBlockData(), block.getType(), block.getBlockData(), player));

        // Pass to the placement alerter
        if (!player.hasPermission("prism.alerts.use.place.ignore")
                && !player.hasPermission("prism.alerts.ignore")) {
            plugin.useMonitor.alertOnBlockPlacement(player, block, "prism.alerts.use.place");
        }
    }

    /**
     * BlockSpreadEvent.
     * @param event BlockSpreadEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockSpread(final BlockSpreadEvent event) {

        // If fire, do we track fire spread? If not, do we track block-spread
        ActionType type = ActionType.BLOCK_SPREAD;
        if (event.getNewState().getType().equals(Material.FIRE)) {
            if (!Prism.getIgnore().event(ActionType.FIRE_SPREAD)) {
                return;
            }
            type = ActionType.FIRE_SPREAD;
        } else {
            if (!Prism.getIgnore().event(ActionType.BLOCK_SPREAD, event.getBlock())) {
                return;
            }
        }

        final Block b = event.getBlock();
        final BlockState s = event.getNewState();

        RecordingQueue.addToQueue(ActionFactory.createBlockChange(type, b.getLocation(), b.getType(), b.getBlockData(),
                s.getType(), s.getBlockData(), "Environment"));
    }

    /**
     * BlockFormEvent.
     * @param event BlockFormEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockForm(final BlockFormEvent event) {
        if (!Prism.getIgnore().event(ActionType.BLOCK_FORM, event.getBlock())) {
            return;
        }
        final Block b = event.getBlock();
        final BlockState s = event.getNewState();

        RecordingQueue.addToQueue(ActionFactory.createBlockChange(ActionType.BLOCK_FORM, b.getLocation(), b.getType(),
                b.getBlockData(), s.getType(), s.getBlockData(), "Environment"));
    }

    /**
     * BlockFadeEvent.
     * @param event BlockFadeEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockFade(final BlockFadeEvent event) {
        if (!Prism.getIgnore().event(ActionType.BLOCK_FADE, event.getBlock())) {
            return;
        }
        final Block b = event.getBlock();
        if (b.getType().equals(Material.FIRE)) {
            return;
        }
        final BlockState s = event.getNewState();

        RecordingQueue.addToQueue(ActionFactory.createBlockChange(ActionType.BLOCK_FADE, b.getLocation(), b.getType(),
                b.getBlockData(), s.getType(), s.getBlockData(), "Environment"));
    }

    /**
     * LeavesDecayEvent.
     * @param event LeavesDecayEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onLeavesDecay(final LeavesDecayEvent event) {
        if (!Prism.getIgnore().event(ActionType.LEAF_DECAY, event.getBlock())) {
            return;
        }
        RecordingQueue.addToQueue(ActionFactory.createBlock(ActionType.LEAF_DECAY, event.getBlock(),
                "Environment"));
    }

    /**
     * Primarily for tracking bed explosions in the nether and end.
     * @param event BlockExplodeEvent
     */
    @EventHandler(priority = EventPriority.HIGHEST, ignoreCancelled = true)
    public void onBlockExplode(BlockExplodeEvent event) {
        if (!Prism.getIgnore().event(ActionType.BED_EXPLODE, event.getBlock())) {
            return;
        }
        //while it might be nice to check that its a bed - the block is already air
        PlayerBed playerBed = weakCache.getIfPresent(event.getBlock().getLocation());
        if (playerBed == null) {
            return;
        }
        String source;
        source = playerBed.player.getName();
        List<Block> affected = event.blockList();
        RecordingQueue.addToQueue(ActionFactory.createBlock(ActionType.BED_EXPLODE, playerBed.bed, playerBed.player));
        constructBlockEvent(ActionType.BED_EXPLODE, source, affected);
        weakCache.invalidate(event.getBlock().getLocation());
    }

    /**
     * Tracks players entering a bed  and where its not possible cache's it in case of explosion.
     * @param enterEvent PlayerBedEnterEvent
     */
    @EventHandler(priority = EventPriority.MONITOR)
    public void onBedEnter(PlayerBedEnterEvent enterEvent) {
        if (enterEvent.getBedEnterResult() == PlayerBedEnterEvent.BedEnterResult.NOT_POSSIBLE_HERE) {
            weakCache.put(enterEvent.getBed().getLocation(), new PlayerBed(enterEvent.getPlayer(),
                    enterEvent.getBed().getState()));
            PrismLogHandler.log("Player attempted to enter bed: " + enterEvent.getPlayer().getName()
                    + " @ " + enterEvent.getBed().getLocation().toString());
        }
        if (!Prism.getIgnore().event(ActionType.BLOCK_USE, enterEvent.getBed())) {
            return;
        }
        RecordingQueue.addToQueue(ActionFactory.createBlock(ActionType.BLOCK_USE, enterEvent.getBed(),
                enterEvent.getPlayer()));
    }

    /**
     * BlockBurnEvent.
     * @param event BlockBurnEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockBurn(final BlockBurnEvent event) {
        if (!Prism.getIgnore().event(ActionType.BLOCK_BURN, event.getBlock())) {
            return;
        }
        Block block = event.getBlock();
        RecordingQueue.addToQueue(ActionFactory.createBlock(ActionType.BLOCK_BURN, block, "Environment"));

        // Change handling a bit if it's a long block
        final Block sibling = Utilities.getSiblingForDoubleLengthBlock(block);
        if (sibling != null && !block.getType().equals(Material.CHEST)
                && !block.getType().equals(Material.TRAPPED_CHEST)) {
            block = sibling;
        }

        // check for block relationships
        logBlockRelationshipsForBlock("Environment", block);

    }

    /**
     * SignChangeEvent.
     * @param event SignChangeEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onSignChange(final SignChangeEvent event) {
        if (!Prism.getIgnore().event(ActionType.SIGN_CHANGE, event.getPlayer())) {
            return;
        }
        if (event.getBlock().getState() instanceof Sign) {
            RecordingQueue.addToQueue(
                    ActionFactory.createSign(ActionType.SIGN_CHANGE, event.getBlock(), event.getLines(),
                            event.getPlayer()));
        }
    }

    /**
     * BlockIgniteEvent.
     * @param event BlockIgniteEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onSetFire(final BlockIgniteEvent event) {

        ActionType cause = null;
        switch (event.getCause()) {
            case FIREBALL:
                cause = ActionType.FIREBALL;
                break;
            case FLINT_AND_STEEL:
                cause = ActionType.LIGHTER;
                break;
            case LAVA:
                cause = ActionType.LAVA_IGNITE;
                break;
            case LIGHTNING:
                cause = ActionType.LIGHTNING;
                break;
            default:
        }
        if (cause != null) {

            if (!Prism.getIgnore().event(cause, event.getBlock().getWorld())) {
                return;
            }

            final Player player = event.getPlayer();

            if (player != null) {
                if ((cause.equals(ActionType.LIGHTER) || cause.equals(ActionType.FIREBALL))
                        && plugin.config.alertConfig.uses.lighter
                        && !player.hasPermission("prism.alerts.use.lighter.ignore")
                        && !player.hasPermission("prism.alerts.ignore")) {
                    plugin.useMonitor.alertOnItemUse(player, "used a " + cause.name, "prism.alerts.use.lighter");
                }
            }

            if (player != null) {
                RecordingQueue.addToQueue(ActionFactory.createBlock(cause, event.getBlock(), player));
            } else {
                RecordingQueue.addToQueue(ActionFactory.createBlock(cause, event.getBlock(), "Environment"));
            }

        }
    }

    /**
     * BlockDispenseEvent.
     * @param event BlockDispenseEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockDispense(final BlockDispenseEvent event) {
        if (!Prism.getIgnore().event(ActionType.BLOCK_DISPENSE)) {
            return;
        }
        RecordingQueue.addToQueue(ActionFactory.createItemStack(ActionType.BLOCK_DISPENSE, event.getItem(),
                event.getItem().getAmount(), -1, null, event.getBlock().getLocation(), "dispenser"));
    }

    /**
     * BlockPistonExtendEvent.
     * @param event BlockPistonExtendEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPistonExtend(final BlockPistonExtendEvent event) {

        if (plugin.config.alertConfig.vanillaXray) {
            final Block noPlayer = event.getBlock().getRelative(event.getDirection()).getRelative(event.getDirection())
                    .getRelative(BlockFace.DOWN);
            for (final Player pl : plugin.getServer().getOnlinePlayers()) {
                final Location loc = pl.getLocation();
                if (loc.getBlockX() == noPlayer.getX() && loc.getBlockY() == noPlayer.getY()
                        && loc.getBlockZ() == noPlayer.getZ()) {
                    plugin.useMonitor.alertOnVanillaXray(pl, "possibly used a vanilla piston/xray trick", null);
                    break;
                }
            }
        }

        if (!Prism.getIgnore().event(ActionType.BLOCK_SHIFT, event.getBlock())) {
            return;
        }

        final List<Block> blocks = event.getBlocks();
        if (!blocks.isEmpty()) {
            for (final Block block : blocks) {

                if (block.getType().equals(Material.AIR)) {
                    continue;
                }

                // Pistons move blocks to the block next to them. If nothing is
                // there it shows as air.
                // We should record the from coords, to coords, and block
                // replaced, as well as the block moved.
                RecordingQueue.addToQueue(ActionFactory.createBlockShift(ActionType.BLOCK_SHIFT, block,
                        block.getRelative(event.getDirection()).getLocation(), "Piston"));

            }
        }
    }

    /**
     * BlockPistonRetractEvent.
     * @param event BlockPistonRetractEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPistonRetract(final BlockPistonRetractEvent event) {
        if (!Prism.getIgnore().event(ActionType.BLOCK_SHIFT, event.getBlock())) {
            return;
        }
        if (!event.isSticky()) {
            return;
        }

        BlockFace facing = event.getDirection();

        for (Block block : event.getBlocks()) {

            if (block.getType().equals(Material.AIR)) {
                continue;
            }
            RecordingQueue.addToQueue(ActionFactory.createBlockShift(ActionType.BLOCK_SHIFT, block,
                    block.getRelative(facing).getLocation(), "Piston"));
        }
    }

    /**
     * BlockFromToEvent.
     * @param event BlockFromToEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockFromTo(final BlockFromToEvent event) {

        // Ignore blocks that aren't liquid. @todo what else triggers this?
        if (!event.getBlock().isLiquid()) {
            return;
        }

        final BlockState from = event.getBlock().getState();
        final BlockState to = event.getToBlock().getState();

        // Watch for blocks that the liquid can break
        if (Utilities.canFlowBreakMaterial(to.getType())) {
            if (from.getType() == Material.WATER) {
                if (Prism.getIgnore().event(ActionType.WATER_BREAK, event.getBlock())) {
                    RecordingQueue.addToQueue(ActionFactory.createBlock(ActionType.WATER_BREAK, event.getToBlock(),
                            "Water"));
                }
            } else if (from.getType() == Material.LAVA) {
                if (Prism.getIgnore().event(ActionType.LAVA_BREAK, event.getBlock())) {
                    RecordingQueue.addToQueue(ActionFactory.createBlock(ActionType.LAVA_BREAK, event.getToBlock(),
                            "Lava"));
                }
            }
        }

        // Record water flow
        if (from.getType() == Material.WATER) {
            if (Prism.getIgnore().event(ActionType.WATER_FLOW, event.getBlock())) {
                RecordingQueue.addToQueue(ActionFactory.createBlock(ActionType.WATER_FLOW, event.getBlock(), "Water"));
            }
        }

        // Record lava flow
        if (from.getType() == Material.LAVA) {
            if (Prism.getIgnore().event(ActionType.LAVA_FLOW, event.getBlock())) {
                RecordingQueue.addToQueue(ActionFactory.createBlock(ActionType.LAVA_FLOW, event.getBlock(), "Lava"));
            }
        }
    }

    private static class PlayerBed {
        final Player player;
        final BlockState bed;

        public PlayerBed(Player player, BlockState bed) {
            this.player = player;
            this.bed = bed;
        }
    }
}