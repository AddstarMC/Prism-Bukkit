package me.botsko.prism.appliers;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.events.PrismBlocksRollbackEvent;
import me.botsko.prism.utils.EntityUtils;
import me.botsko.prism.wands.RollbackWand;
import me.botsko.prism.wands.Wand;
import net.md_5.bungee.api.chat.BaseComponent;
import net.md_5.bungee.api.chat.TextComponent;
import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.block.data.BlockData;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Entity;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Player;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

public class Preview implements Previewable {

    protected final Prism plugin;

    private final PrismProcessType processType;
    protected final CommandSender sender;
    protected final Player player;
    protected final QueryParameters parameters;
    private final HashMap<Entity, Integer> entitiesMoved = new HashMap<>();
    protected final ArrayList<BlockStateChange> blockStateChanges = new ArrayList<>();
    private final List<Handler> worldChangeQueue = Collections.synchronizedList(new LinkedList<>());
    private boolean isPreview = false;
    private int skippedBlockCount;
    private int changesAppliedCount;
    private int changesPlannedCount;
    private int blockChangesRead = 0;
    private int worldChangeQueueTaskId;
    private ApplierCallback callback;

    /**
     * Constructor.
     * @param plugin Prism
     */
    public Preview(Prism plugin, CommandSender sender, Collection<Handler> results, QueryParameters parameters,
                   ApplierCallback callback) {

        this.processType = parameters.getProcessType();
        this.plugin = plugin;
        this.sender = sender;
        this.parameters = parameters;

        if (sender instanceof Player) {
            this.player = (Player) sender;
        } else {
            this.player = null;
        }

        if (callback != null) {
            this.callback = callback;
        }

        // Append all actions to the queue.
        worldChangeQueue.addAll(results);

    }

    @Override
    public void setIsPreview(boolean isPreview) {
        this.isPreview = isPreview;
    }

    @Override
    public void cancel_preview() {
        if (player == null)
            return;
        if (!blockStateChanges.isEmpty()) {

            // pull all players that are part of this preview
            final List<CommandSender> previewPlayers = parameters.getSharedPlayers();
            previewPlayers.add(player);

            for (final BlockStateChange u : blockStateChanges) {
                Location loc = u.getOriginalBlock().getLocation();
                BlockData data = u.getOriginalBlock().getBlockData();

                for (final CommandSender sharedPlayer : previewPlayers) {
                    if (sharedPlayer instanceof Player)
                        EntityUtils.sendBlockChange((Player) sharedPlayer, loc, data);
                }
            }
        }
        sender.sendMessage(
                Prism.messenger.playerHeaderMsg("Preview canceled." + ChatColor.GRAY + " Please come again!"));
    }

    /**
     *
     */
    @Override
    public void apply_preview() {
        if (player == null)
            return;
        sender.sendMessage(Prism.messenger.playerHeaderMsg("Applying rollback from preview..."));
        setIsPreview(false);
        changesAppliedCount = 0;
        skippedBlockCount = 0;
        changesPlannedCount = 0;
        apply();
    }

    /**
     *
     */
    @Override
    public void preview() {
    }

    @Override
    public void apply() {

        if (!worldChangeQueue.isEmpty()) {

            if (!isPreview && player != null) {

                Wand oldwand = null;
                if (Prism.playersWithActiveTools.containsKey(player.getName())) {
                    // Pull the wand in use
                    oldwand = Prism.playersWithActiveTools.get(player.getName());
                }

                boolean show_nearby = true;
                if (oldwand instanceof RollbackWand) {
                    show_nearby = false;
                }
                if (show_nearby) {
                    // Inform nearby players
                    plugin.notifyNearby(player, parameters.getRadius(), player.getDisplayName() + " is performing a "
                            + processType.name().toLowerCase() + " near you.");
                    // Inform staff
                    if (plugin.getConfig().getBoolean("prism.alerts.alert-staff-to-applied-process")) {
                        final String cmd = parameters.getOriginalCommand();
                        if (cmd != null) {
                            List<BaseComponent> msg = new ArrayList<>();
                            TextComponent s = new TextComponent(processType.name().toLowerCase() + " by "
                                    + player.getDisplayName());
                            s.setColor(net.md_5.bungee.api.ChatColor.WHITE);
                            msg.add(s);
                            TextComponent t = new TextComponent(parameters.getOriginalCommand());
                            t.setColor(net.md_5.bungee.api.ChatColor.GRAY);
                            msg.add(t);
                            plugin.alertPlayers(player, msg);
                        }
                    }
                }
            }

            // Offload the work of world changes
            // to a scheduled sync task
            processWorldChanges();
        }
    }

    private void processWorldChanges() {

        blockChangesRead = 0;

        worldChangeQueueTaskId = plugin.getServer().getScheduler().scheduleSyncRepeatingTask(plugin, () -> {

            if (plugin.getConfig().getBoolean("prism.debug")) {
                Prism.debug("World change queue size: " + worldChangeQueue.size());
            }

            if (worldChangeQueue.isEmpty()) {
                sender.sendMessage(
                        Prism.messenger.playerError(ChatColor.GRAY + "No actions found that match the criteria."));
                return;
            }

            int iterationCount = 0;
            final int currentQueueOffset = blockChangesRead;
            if (currentQueueOffset < worldChangeQueue.size()) {
                for (final Iterator<Handler> iterator = worldChangeQueue.listIterator(currentQueueOffset); iterator
                        .hasNext(); ) {
                    final Handler a = iterator.next();

                    // Only iterate the queue using a diff offset when
                    // previewing, actual rollbacks
                    // will remove from the queue and we'd begin at zero
                    // again
                    if (isPreview)
                        blockChangesRead++;

                    // We only want to process a set number of block changes
                    // per
                    // schedule. Breaking here will leave the rest in the
                    // queue.
                    iterationCount++;
                    if (iterationCount >= 1000) {
                        break;
                    }

                    // No sense in trying to rollback
                    // when the type doesn't support it.
                    if (processType.equals(PrismProcessType.ROLLBACK) && !a.getActionType().canRollback()) {
                        iterator.remove();
                        continue;
                    }

                    // No sense in trying to restore
                    // when the type doesn't support it.
                    if (processType.equals(PrismProcessType.RESTORE) && !a.getActionType().canRestore()) {
                        iterator.remove();
                        continue;
                    }

					/*
					  Reverse or restore block changes by allowing the handler to decide what needs
					  to happen.
					 */
                    ChangeResult result = null;

                    try {
                        if (processType.equals(PrismProcessType.ROLLBACK)) {
                            result = a.applyRollback(player, parameters, isPreview);
                        }
                        if (processType.equals(PrismProcessType.RESTORE)) {
                            result = a.applyRestore(player, parameters, isPreview);
                        }
                        if (processType.equals(PrismProcessType.UNDO)) {
                            result = a.applyUndo(player, parameters, isPreview);
                        }

                        // No action, continue
                        if (result == null) {
                            iterator.remove();
                            continue;
                        }
                        // Skip actions that have not returned any results
                        if (result.getType() == null) {
                            skippedBlockCount++;
                            iterator.remove();
                            continue;
                        }
                        // Skipping
                        else if (result.getType().equals(ChangeResultType.SKIPPED)) {
                            skippedBlockCount++;
                            iterator.remove();
                            continue;
                        }
                        // Skipping, but change planned
                        else if (result.getType().equals(ChangeResultType.PLANNED)) {
                            changesPlannedCount++;
                            continue;
                        }
                        // Change applied
                        else {
                            blockStateChanges.add(result.getBlockStateChange());
                            changesAppliedCount++;
                        }
                        // Unless a preview, remove from queue
                        if (!isPreview) {
                            iterator.remove();
                        }
                    } catch (final Exception e) {

                        // Something caused an exception. We *have* to catch
                        // this
                        // so we can remove the item from the queue,
                        // otherwise
                        // the cycle will just spin in eternity and all
                        // damnation, normally killing their server through
                        // log files in the GB.

                        // Log the error so they have something to report
                        String line = "Applier error:";
                        String message = e.getMessage();

                        if (message != null) {
                            line += (' ' + message);
                        }

                        Prism.log(line);
                        e.printStackTrace();

                        // Count as skipped, remove from queue
                        skippedBlockCount++;
                        iterator.remove();

                    }
                }
            }

            // The task for this action is done being used
            if (worldChangeQueue.isEmpty() || blockChangesRead >= worldChangeQueue.size()) {
                plugin.getServer().getScheduler().cancelTask(worldChangeQueueTaskId);
                if (isPreview) {
                    postProcessPreview();
                } else {
                    postProcess();
                }
            }
        }, 2L, 2L);
    }

    private void postProcessPreview() {
        // If there's planned changes, save the preview
        if (isPreview && (changesAppliedCount > 0 || changesPlannedCount > 0)) {
            // Append the preview and blocks temporarily
            final PreviewSession ps = new PreviewSession(player, this);
            plugin.playerActivePreviews.put(player.getName(), ps);
            moveEntitiesToSafety();
        }
        fireApplierCallback();
    }

    private void postProcess() {

        // POST ROLLBACK TRIGGERS
        if (processType.equals(PrismProcessType.ROLLBACK)) {

            // We're going to modify the action type of the query params
            // and pass it along to a restore.
            // NOTE: These params have been modified from original, so
            // do NOT use the object for original params.

            // /**
            // * If we've rolled back any containers we need to restore
            // item-removes.
            // */
            // if(parameters.shouldTriggerRollbackFor(ActionType.ITEM_REMOVE)){
            //
            // Prism.debug("Action being rolled back triggers a second rollback: Item
            // Remove");
            //
            // QueryParameters triggerParameters;
            // try {
            // triggerParameters = parameters.clone();
            // triggerParameters.resetActionTypes();
            // triggerParameters.addActionType(ActionType.ITEM_REMOVE);
            //
            // ActionsQuery aq = new ActionsQuery(plugin);
            // QueryResult results = aq.lookup( player, triggerParameters );
            // if(!results.getActionResults().isEmpty()){
            // Rollback rb = new Rollback( plugin, player,
            // results.getActionResults(), triggerParameters );
            // rb.apply();
            // }
            // } catch (CloneNotSupportedException e) {
            // e.printStackTrace();
            // }
            // }
        }

        moveEntitiesToSafety();

        fireApplierCallback();

    }

    private void moveEntitiesToSafety() {
        if (parameters.getWorld() != null && player != null) {
            final List<Entity> entities = player.getNearbyEntities(parameters.getRadius(), parameters.getRadius(),
                    parameters.getRadius());
            entities.add(player);
            for (final Entity entity : entities) {
                if (entity instanceof LivingEntity) {
                    int add = 0;
                    if (EntityUtils.inCube(parameters.getPlayerLocation(), parameters.getRadius(),
                            entity.getLocation())) {
                        final Location l = entity.getLocation();
                        while (!EntityUtils.playerMayPassThrough(l.getBlock().getType())) {
                            add++;
                            if (l.getY() >= 256)
                                break;
                            l.setY(l.getY() + 1);
                        }
                        if (add > 0) {
                            entitiesMoved.put(entity, add);
                            entity.teleport(l);
                        }
                    }
                }
            }
        }
    }

    private void fireApplierCallback() {

        // If previewing, the applied count will never apply, we'll
        // assume it's all planned counts
        if (isPreview) {
            changesPlannedCount += changesAppliedCount;
            changesAppliedCount = 0;
        }

        final ApplierResult results = new ApplierResult(isPreview, changesAppliedCount, skippedBlockCount,
                changesPlannedCount, blockStateChanges, parameters, entitiesMoved);

        if (callback != null) {
            callback.handle(sender, results);
        }

        // Trigger the events
        if (processType.equals(PrismProcessType.ROLLBACK)) {
            final PrismBlocksRollbackEvent event = new PrismBlocksRollbackEvent(blockStateChanges, player, parameters,
                    results);
            plugin.getServer().getPluginManager().callEvent(event);
        }

        plugin.eventTimer.recordTimedEvent("applier function complete");

        // record timed events to log
        if (plugin.getConfig().getBoolean("prism.debug")) {
            // Flush timed data
            plugin.eventTimer.printTimeRecord();
            Prism.debug("Changes: " + changesAppliedCount);
            Prism.debug("Planned: " + changesPlannedCount);
            Prism.debug("Skipped: " + skippedBlockCount);
        }
    }
}