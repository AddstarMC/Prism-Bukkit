package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.api.actions.Handler;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.entity.HumanEntity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.enchantment.EnchantItemEvent;
import org.bukkit.event.inventory.CraftItemEvent;
import org.bukkit.event.inventory.InventoryClickEvent;
import org.bukkit.event.inventory.InventoryDragEvent;
import org.bukkit.event.inventory.InventoryPickupItemEvent;
import org.bukkit.event.inventory.InventoryType;
import org.bukkit.event.inventory.PrepareItemCraftEvent;
import org.bukkit.event.player.PlayerItemBreakEvent;
import org.bukkit.inventory.InventoryHolder;
import org.bukkit.inventory.ItemStack;

import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.stream.Collectors;

public class PrismInventoryEvents implements Listener {

    private boolean trackingInsert;
    private boolean trackingRemove;
    private boolean trackingBreaks;
    private static final String INSERT = "item-insert";
    private static final String REMOVE = "item-remove";
    private static final String BREAK = "item-break";
    private final Prism plugin;

    /**
     * Constructor.
     * @param plugin Prism
     */
    public PrismInventoryEvents(Prism plugin) {
        this.plugin = plugin;
        this.trackingInsert = !Prism.getIgnore().event(INSERT);
        this.trackingRemove = !Prism.getIgnore().event(REMOVE);
        this.trackingBreaks = Prism.getIgnore().event(BREAK);

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
        if (notTrackingInsertAndRemove()) {
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

                RecordingQueue.addToQueue(ActionFactory.createItemStack(INSERT, entry.getValue(), amount,
                        rawSlot, null, containerLoc, player));
            }
        }
    }

    /**
     * EnchantItemEvent.
     *
     * @param event EnchantItemEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEnchantItem(final EnchantItemEvent event) {
        if (!Prism.getIgnore().event("enchant-item", event.getEnchanter())) {
            return;
        }
        final Player player = event.getEnchanter();
        RecordingQueue.addToQueue(ActionFactory.createItemStack("enchant-item", event.getItem(),
                event.getEnchantsToAdd(), event.getEnchantBlock().getLocation(), player));
    }

    /**
     * Handle Crafting.
     * @param prepareItemCraftEvent event.
     */
    @EventHandler(priority = EventPriority.MONITOR,ignoreCancelled = true)
    public void onPrepareCraftItem(PrepareItemCraftEvent prepareItemCraftEvent) {
        if (Prism.getIgnore().event("craft-item")) {
            return;
        }
        List<HumanEntity> recordable = prepareItemCraftEvent.getViewers().stream().filter(humanEntity -> {
            if (humanEntity instanceof Player) {
                return Prism.getIgnore().event("craft-item", (Player) humanEntity);
            }
            return false;
        }).collect(Collectors.toList());
        if (recordable.size() > 0) {
            //todo
            Prism.debug("PrepareCraftEvent: " + prepareItemCraftEvent.toString());
        }

    }

    /**
     * CraftItemEvent.
     *
     * @param event CraftItemEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onCraftItem(final CraftItemEvent event) {
        final Player player = (Player) event.getWhoClicked();
        if (!Prism.getIgnore().event("craft-item", player)) {
            return;
        }
        final ItemStack item = event.getRecipe().getResult();
        RecordingQueue.addToQueue(
                ActionFactory.createItemStack("craft-item", item, 1, -1, null, player.getLocation(), player));
    }

    /**
     * Handle inventory transfers.
     *
     * @param event InventoryClickEvent
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onInventoryClick(final InventoryClickEvent event) {
        int slot = event.getRawSlot(); //this is the unique slot number for the view.
        // Specifically slot -999, or out of the window
        if (slot < 0) {
            return;
        }
        Location containerLoc = event.getInventory().getLocation(); //this is the top Inventory
        // Virtual inventory or something (enderchest?)
        if (containerLoc == null) {
            return;
        }

        if (notTrackingInsertAndRemove()) {
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
        Prism.debug("HELD:" + ((heldItem != null) ? heldItem.toString() : "NULL"));
        Prism.debug("SLOT:" +  slotItem.toString());

        switch (event.getClick()) {
            // IGNORE BOTTOM
            case LEFT:
                if (isTopInv) {
                    if (heldItem == null || heldItem.getType() == Material.AIR) {
                        if (slotItem.getType() != Material.AIR) {
                            RecordingQueue.addToQueue(ActionFactory.createItemStack(REMOVE, slotItem,
                                    slotItem.getAmount(), slot, null, containerLoc, player));
                            Prism.debug("ACTION: " + event.getAction().name());
                        }
                    } else {
                        int amount = 0;
                        int maxStack = heldItem.getMaxStackSize();
                        if (slotItem.getType() == Material.AIR && heldItem.getAmount() <= maxStack) {
                            amount = heldItem.getAmount();
                        }
                        if (slotItem.getType().equals(heldItem.getType())) {
                            int slotQty = slotItem.getAmount();
                            amount = Math.min(maxStack - slotQty,heldItem.getAmount());
                        }
                        if (amount > 0) {
                            RecordingQueue.addToQueue(ActionFactory.createItemStack(INSERT, heldItem, amount, slot,
                                    null, containerLoc, player));
                            Prism.debug("ACTION: " + event.getAction().name());

                        }
                        if (slotItem.getType() != Material.AIR && !slotItem.getType().equals(heldItem.getType())) {
                            // its a switch.
                            RecordingQueue.addToQueue(ActionFactory.createItemStack(INSERT,heldItem,
                                    heldItem.getAmount(),slot,null,containerLoc,player));
                            Prism.debug("ACTION: " + event.getAction().name());
                            RecordingQueue.addToQueue(ActionFactory.createItemStack(REMOVE,slotItem,
                                    slotItem.getAmount(),slot,null,containerLoc,player));
                        }
                    }
                }
                break;

            case RIGHT:
                if (isTopInv) {
                    if (heldItem == null || heldItem.getType() == Material.AIR) {
                        if (slotItem.getType() != Material.AIR) {
                            RecordingQueue.addToQueue(ActionFactory.createItemStack(REMOVE, slotItem,
                                    (slotItem.getAmount() + 1) / 2, slot, null, containerLoc, player));
                            Prism.debug("ACTION: " + event.getAction().name());

                        }
                    } else {
                        if ((slotItem.getType() == Material.AIR || slotItem.equals(heldItem))
                                && slotItem.getAmount() < slotItem.getType().getMaxStackSize()) {
                            RecordingQueue.addToQueue(ActionFactory.createItemStack(INSERT, slotItem, 1, slot, null,
                                    containerLoc, player));
                            Prism.debug("ACTION: " + event.getAction().name());

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
                        Prism.debug("ACTION: " + event.getAction().name());

                    }

                    if (swapItem != null && swapItem.getType() != Material.AIR) {
                        RecordingQueue.addToQueue(ActionFactory.createItemStack(INSERT, swapItem, swapItem.getAmount(),
                                slot, null, containerLoc, player));
                        Prism.debug("ACTION: " + event.getAction().name());

                    }
                }
                break;

            // HALF 'N HALF
            case DOUBLE_CLICK: {
                int amount = (heldItem == null) ? 0 :
                        heldItem.getType().getMaxStackSize() - heldItem.getAmount();

                ItemStack[] contents = event.getInventory().getStorageContents();
                int length = contents.length;

                for (int i = 0; i < length; ++i) {
                    ItemStack is = contents[i];

                    int size = 0;
                    if (is != null && (is.getType() != Material.AIR || is.equals(heldItem))) {
                        size += is.getAmount();
                    }
                    amount = recordDeductTransfer(REMOVE,size,amount,heldItem,containerLoc,i,player,event);
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
                            //noinspection ConstantConditions  Until intellij sorts it checks
                            if (is == null || is.getType() == Material.AIR) {
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
                        Prism.debug("ACTION: " + event.getAction().name());

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
                            amount = recordDeductTransfer(INSERT,stackSize - is.getAmount(),amount,slotItem,
                                    containerLoc,i,player,event);
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
                                amount = recordDeductTransfer(INSERT,stackSize,amount,slotItem,
                                        containerLoc,i,player,event);
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
                    Prism.debug("ACTION: " + event.getAction().name());

                }
                break;

            case CONTROL_DROP:
                if (slotItem.getType() != Material.AIR && slotItem.getAmount() > 0) {
                    RecordingQueue.addToQueue(ActionFactory.createItemStack(REMOVE, slotItem, slotItem.getAmount(),
                            slot, null, containerLoc, player));
                    Prism.debug("ACTION: " + event.getAction().name());

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

    private int recordDeductTransfer(String act, int size, int amount, ItemStack heldItem, Location containerLoc,
                                     int slotLocation, Player player, InventoryClickEvent event) {
        int transferred = Math.min(size, amount);
        int newAmount = amount - transferred;
        if (transferred > 0) {
            RecordingQueue.addToQueue(ActionFactory.createItemStack(act, heldItem, transferred, slotLocation, null,
                    containerLoc, player));
            Prism.debug("ACTION: " + event.getAction().name());

        }
        return newAmount;
    }

    /**
     * Tracks item breakage. Cant be rolled back.  At this point the item damage is not 0 however it will be set 0 after
     * event completes - Reported item durability will be the durability before the event.
     * @param event PlayerItemBreakEvent.
     */
    @EventHandler(ignoreCancelled = true,priority = EventPriority.MONITOR)
    public void onItemBreak(PlayerItemBreakEvent event) {
        if (!trackingBreaks) {
            return;
        }
        ItemStack item = event.getBrokenItem();
        Handler h = ActionFactory.createItemStack(BREAK,item,null, event.getPlayer().getLocation(),event.getPlayer());
        RecordingQueue.addToQueue(h);
    }

    private boolean notTrackingInsertAndRemove() {
        this.trackingInsert = Prism.getIgnore().event(INSERT);
        this.trackingRemove = Prism.getIgnore().event(REMOVE);
        this.trackingBreaks = Prism.getIgnore().event(BREAK);

        return !trackingInsert && !trackingRemove;
    }

}
