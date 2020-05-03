package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryDragEvent;
import org.bukkit.event.inventory.InventoryPickupItemEvent;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.ItemStack;

import java.util.Map;
import java.util.Map.Entry;

public class PrismInventoryEvents implements Listener {

    private final boolean trackingInsert;
    private final boolean trackingRemove;
    private static final String INSERT = "item-insert";
    private static final String REMOVE = "item-remove";
    private final Prism plugin;

    /**
     * Constructor.
     * @param plugin Prism
     */
    public PrismInventoryEvents(Prism plugin) {
        this.plugin = plugin;
        this.trackingInsert = plugin.getConfig().getBoolean("prism.tracking.item-insert");
        this.trackingRemove = plugin.getConfig().getBoolean("prism.tracking.item-remove");
    }

    /**
     * InventoryPickupItemEvent.
     * @param event InventoryPickupItemEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onInventoryPickupItem(final InventoryPickupItemEvent event) {

        if (!plugin.getConfig().getBoolean("prism.track-hopper-item-events")) {
            return;
        }

        if (!Prism.getIgnore().event("item-pickup")) {
            return;
        }

        // If hopper
        if (event.getInventory().getType().equals(InventoryType.HOPPER)) {
            RecordingQueue.addToQueue(ActionFactory.createItemStack("item-pickup", event.getItem().getItemStack(),
                    event.getItem().getItemStack().getAmount(), -1, null, event.getItem().getLocation(), "hopper"));
        }
    }

    /**
     * Handle inventory transfers.
     *
     * @param event InventoryDragEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onInventoryDrag(final InventoryDragEvent event) {
        if (!trackingInsert && !trackingRemove) {
            return;
        }
        // Get container
        final InventoryHolder ih = event.getInventory().getHolder();

        // Store some info
        final Player player = (Player) event.getWhoClicked();

        // Ignore all item move events where players modify their own inventory
        if (ih instanceof Player) {
            Player other = (Player) event.getInventory().getHolder();

            if (event.getWhoClicked().equals(other)) {
                return;
            }
        }

        Location containerLoc = event.getInventory().getLocation();

        if (containerLoc == null) {
            return;
        }

        final Map<Integer, ItemStack> newItems = event.getNewItems();
        for (final Entry<Integer, ItemStack> entry : newItems.entrySet()) {

            int rawSlot = entry.getKey();

            // Top inventory
            if (rawSlot < event.getInventory().getSize()) {
                ItemStack stack = event.getView().getItem(rawSlot);
                int slotViewAmount = (stack == null)
                        ? 0 : stack.getAmount();
                int amount = entry.getValue().getAmount() - slotViewAmount;

                RecordingQueue.addToQueue(ActionFactory.createItemStack("item-insert", entry.getValue(), amount,
                        rawSlot, null, containerLoc, player));
            }
        }
    }

    /**
     * Handle inventory transfers.
     *
     * @param event InventoryClickEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onInventoryClick(final InventoryClickEvent event) {
        int slot = event.getRawSlot();

        // Specifically slot -999, or out of the window
        if (slot < 0) {
            return;
        }

        Location containerLoc = event.getInventory().getLocation();

        // Virtual inventory or something (enderchest?)
        if (containerLoc == null) {
            return;
        }

        if (!trackingInsert && !trackingRemove) {
            return;
        }
        // Store some info
        final Player player = (Player) event.getWhoClicked();

        // Ignore all item move events where players modify their own inventory
        if (event.getInventory().getHolder() instanceof Player) {
            Player other = (Player) event.getInventory().getHolder();

            if (other.equals(player)) {
                return;
            }
        }
        boolean isTopInv = slot < event.getInventory().getSize();

        ItemStack heldItem = event.getCursor();
        ItemStack slotItem = event.getCurrentItem();

        // This happens when opening someone else's inventory, so don't bother tracking it
        if (slotItem == null) {
            return;
        }

        switch (event.getClick()) {
            // IGNORE BOTTOM
            case LEFT:
                if (isTopInv) {
                    if (heldItem.getType() == Material.AIR) { //todo if the item under the cursor is null what to do?
                        if (slotItem.getType() != Material.AIR) {
                            RecordingQueue.addToQueue(ActionFactory.createItemStack(REMOVE, slotItem,
                                    slotItem.getAmount(), slot, null, containerLoc, player));
                        }
                    } else {
                        if (slotItem.getType() == Material.AIR || slotItem.equals(heldItem)) {
                            int amount = Math.min(slotItem.getType().getMaxStackSize(),
                                    slotItem.getAmount() + heldItem.getAmount()) - slotItem.getAmount();

                            if (amount > 0) {
                                RecordingQueue.addToQueue(ActionFactory.createItemStack(INSERT, slotItem, amount, slot,
                                        null, containerLoc, player));
                            }
                        }
                    }
                }
                break;

            case RIGHT:
                if (isTopInv) {
                    if (heldItem.getType() == Material.AIR) {
                        if (slotItem.getType() != Material.AIR) {
                            RecordingQueue.addToQueue(ActionFactory.createItemStack(REMOVE, slotItem,
                                    (slotItem.getAmount() + 1) / 2, slot, null, containerLoc, player));
                        }
                    } else {
                        if ((slotItem.getType() == Material.AIR || slotItem.equals(heldItem))
                                && slotItem.getAmount() < slotItem.getType().getMaxStackSize()) {
                            RecordingQueue.addToQueue(ActionFactory.createItemStack(INSERT, slotItem, 1, slot, null,
                                    containerLoc, player));
                        }
                    }
                }
                break;

            case NUMBER_KEY:
                if (isTopInv) {
                    ItemStack swapItem = player.getInventory().getItem(event.getHotbarButton());

                    if (slotItem.getType() != Material.AIR) {
                        RecordingQueue.addToQueue(ActionFactory.createItemStack(REMOVE, slotItem, slotItem.getAmount(),
                                slot, null, containerLoc, player));
                    }

                    if (swapItem != null && swapItem.getType() != Material.AIR) {
                        RecordingQueue.addToQueue(ActionFactory.createItemStack(INSERT, swapItem, swapItem.getAmount(),
                                slot, null, containerLoc, player));
                    }
                }
                break;

            // HALF 'N HALF
            case DOUBLE_CLICK: {
                int amount = heldItem.getType().getMaxStackSize() - heldItem.getAmount();

                ItemStack[] contents = event.getInventory().getStorageContents();
                int length = contents.length;

                for (int i = 0; i < length; ++i) {
                    ItemStack is = contents[i];

                    int size = 0;
                    if (is != null && (is.getType() != Material.AIR || is.equals(heldItem))) {
                        size += is.getAmount();
                    }

                    int transferred = Math.min(size, amount);
                    amount -= transferred;

                    if (transferred > 0) {
                        RecordingQueue.addToQueue(ActionFactory.createItemStack(REMOVE, heldItem, transferred, i, null,
                                containerLoc, player));
                    }

                    if (amount <= 0) {
                        break;
                    }
                }
                break;
            }

            // CROSS INVENTORY EVENTS
            case SHIFT_LEFT:
            case SHIFT_RIGHT:
                if (isTopInv) {
                    if (slotItem.getType() != Material.AIR) {
                        int stackSize = slotItem.getType().getMaxStackSize();
                        int remaining = slotItem.getAmount();

                        for (ItemStack is : event.getView().getBottomInventory().getStorageContents()) {
                            if (is.getType() == Material.AIR) {
                                remaining -= stackSize;
                            } else if (is.isSimilar(slotItem)) {
                                remaining -= (stackSize - Math.min(is.getAmount(), stackSize));
                            }

                            if (remaining <= 0) {
                                remaining = 0;
                                break;
                            }
                        }

                        RecordingQueue.addToQueue(ActionFactory.createItemStack(REMOVE, slotItem,
                                slotItem.getAmount() - remaining, slot, null, containerLoc, player));
                    }
                } else {
                    int stackSize = slotItem.getType().getMaxStackSize();
                    int amount = slotItem.getAmount();

                    ItemStack[] contents = event.getInventory().getStorageContents();
                    int length = contents.length;

                    // Fill item stacks first
                    for (int i = 0; i < length; ++i) {
                        ItemStack is = contents[i];

                        if (slotItem.isSimilar(is)) {
                            int transferred = Math.min(stackSize - is.getAmount(), amount);
                            amount -= transferred;

                            if (transferred > 0) {
                                RecordingQueue.addToQueue(ActionFactory.createItemStack(INSERT, slotItem, transferred,
                                        i, null, containerLoc, player));
                            }

                            if (amount <= 0) {
                                break;
                            }
                        }
                    }

                    // Fill empty slots
                    if (amount > 0) {
                        for (int i = 0; i < length; ++i) {
                            ItemStack is = contents[i];

                            if (is == null || is.getType() == Material.AIR) {
                                int transferred = Math.min(stackSize, amount);
                                amount -= transferred;

                                if (transferred > 0) {
                                    RecordingQueue.addToQueue(ActionFactory.createItemStack(INSERT, slotItem,
                                            transferred, i, null, containerLoc, player));
                                }

                                if (amount <= 0) {
                                    break;
                                }
                            }
                        }
                    }
                }
                break;

            // DROPS
            case DROP:
                if (slotItem.getType() != Material.AIR && slotItem.getAmount() > 0) {
                    RecordingQueue.addToQueue(
                            ActionFactory.createItemStack(REMOVE, slotItem, 1, slot, null, containerLoc, player));
                }
                break;

            case CONTROL_DROP:
                if (slotItem.getType() != Material.AIR && slotItem.getAmount() > 0) {
                    RecordingQueue.addToQueue(ActionFactory.createItemStack(REMOVE, slotItem, slotItem.getAmount(),
                            slot, null, containerLoc, player));
                }
                break;

            case WINDOW_BORDER_LEFT:
                // Drop stack on cursor
            case WINDOW_BORDER_RIGHT:
                // Drop 1 on cursor

            default:
                // What the hell did you do
        }
    }
}
