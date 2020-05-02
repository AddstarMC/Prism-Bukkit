package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.settings.Settings;
import me.botsko.prism.utils.InventoryUtils;
import me.botsko.prism.utils.ItemUtils;
import me.botsko.prism.wands.InspectorWand;
import me.botsko.prism.wands.ProfileWand;
import me.botsko.prism.wands.QueryWandBase;
import me.botsko.prism.wands.RestoreWand;
import me.botsko.prism.wands.RollbackWand;
import me.botsko.prism.wands.Wand;
import org.bukkit.ChatColor;
import org.bukkit.Material;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.PlayerInventory;

import java.util.List;
import java.util.Objects;

public class WandCommand implements SubHandler {

    private final Prism plugin;

    /**
     * Constructor.
     * @param plugin Prism
     */
    public WandCommand(Prism plugin) {
        this.plugin = plugin;
    }

     // TODO break this down.
    @Override
    public void handle(CallInfo call) {
        String type = "i";
        final boolean isInspect = call.getArg(0).equalsIgnoreCase("inspect") || call.getArg(0).equalsIgnoreCase("i");
        if (!isInspect) {
            if (call.getArgs().length < 2) {
                call.getPlayer().sendMessage(
                        Prism.messenger.playerError("You need to specify a wand type. Use '/prism ?' for help."));
                return;
            }
            type = call.getArg(1);
        }

        Wand oldwand = null;
        if (Prism.playersWithActiveTools.containsKey(call.getPlayer().getName())) {
            // Pull the wand in use
            oldwand = Prism.playersWithActiveTools.get(call.getPlayer().getName());
        }

        // Always remove the old one
        Prism.playersWithActiveTools.remove(call.getPlayer().getName());

        // Determine default mode
        String mode = plugin.getConfig().getString("prism.wands.default-mode");

        // Check if the player has a personal override
        if (plugin.getConfig().getBoolean("prism.wands.allow-user-override")) {
            final String personalMode = Settings.getSetting("wand.mode", call.getPlayer());
            if (personalMode != null) {
                mode = personalMode;
            }
        }

        // Determine which item we're using.
        String toolKey = null;
        if (mode.equals("item")) {
            toolKey = plugin.getConfig().getString("prism.wands.default-item-mode-id");
        } else if (mode.equals("block")) {
            toolKey = plugin.getConfig().getString("prism.wands.default-block-mode-id");
        }

        // Check if the player has a personal override
        if (plugin.getConfig().getBoolean("prism.wands.allow-user-override")) {
            final String personalToolKey = Settings.getSetting("wand.item", call.getPlayer());
            if (personalToolKey != null) {
                toolKey = personalToolKey;
            }
        }

        Material item_material = null;

        if (toolKey != null) {
            item_material = Material.matchMaterial(toolKey);
        }

        String wandOn = "";
        String item_name = "";
        StringBuilder parameters = new StringBuilder();
        if (item_material != null) {
            item_name = Prism.getItems().getAlias(item_material, null);
            wandOn += " on a " + item_name;
        }

        for (int i = (isInspect ? 1 : 2); i < call.getArgs().length; i++) {
            if (parameters.length() == 0) {
                parameters.append(" using:" + ChatColor.GRAY);
            }
            parameters.append(" ").append(call.getArg(i));
        }

        if (ItemUtils.isBadWand(item_material)) {
            call.getPlayer().sendMessage(
                    Prism.messenger.playerError("Sorry, but you may not use " + item_name + " for a wand."));
            return;
        }

        boolean enabled = false;
        Wand wand = null;

        /*
          Inspector wand
         */
        if (type.equalsIgnoreCase("i") || type.equalsIgnoreCase("inspect")) {
            if (!call.getPlayer().hasPermission("prism.lookup")
                    && !call.getPlayer().hasPermission("prism.wand.inspect")) {
                call.getPlayer().sendMessage(Prism.messenger.playerError("You do not have permission for this."));
                return;
            }
            if (oldwand instanceof InspectorWand) {
                call.getPlayer().sendMessage(Prism.messenger
                        .playerHeaderMsg("Inspection wand " + ChatColor.RED + "disabled" + ChatColor.WHITE + "."));
            } else {
                wand = new InspectorWand(plugin);
                call.getPlayer().sendMessage(Prism.messenger.playerHeaderMsg("Inspection wand " + ChatColor.GREEN
                        + "enabled" + ChatColor.WHITE + wandOn + parameters + "."));
                enabled = true;
            }
        }

        /*
          Profile wand
         */
        else if (type.equalsIgnoreCase("p") || type.equalsIgnoreCase("profile")) {
            if (!call.getPlayer().hasPermission("prism.lookup")
                    && !call.getPlayer().hasPermission("prism.wand.profile")) {
                call.getPlayer().sendMessage(Prism.messenger.playerError("You do not have permission for this."));
                return;
            }
            if (oldwand instanceof ProfileWand) {
                call.getPlayer().sendMessage(Prism.messenger
                        .playerHeaderMsg("Profile wand " + ChatColor.RED + "disabled" + ChatColor.WHITE + "."));
            } else {
                wand = new ProfileWand();
                call.getPlayer().sendMessage(Prism.messenger.playerHeaderMsg(
                        "Profile wand " + ChatColor.GREEN + "enabled" + ChatColor.WHITE + wandOn + "."));
                enabled = true;
            }
        }

        /*
          Rollback wand
         */
        else if (type.equalsIgnoreCase("rollback") || type.equalsIgnoreCase("rb")) {
            if (!call.getPlayer().hasPermission("prism.rollback")
                    && !call.getPlayer().hasPermission("prism.wand.rollback")) {
                call.getPlayer().sendMessage(Prism.messenger.playerError("You do not have permission for this."));
                return;
            }
            if (oldwand instanceof RollbackWand) {
                call.getPlayer().sendMessage(Prism.messenger
                        .playerHeaderMsg("Rollback wand " + ChatColor.RED + "disabled" + ChatColor.WHITE + "."));
            } else {
                wand = new RollbackWand(plugin);
                call.getPlayer().sendMessage(Prism.messenger.playerHeaderMsg(
                        "Rollback wand " + ChatColor.GREEN + "enabled" + ChatColor.WHITE + wandOn + parameters + "."));
                enabled = true;
            }
        }

        /*
          Restore wand
         */
        else if (type.equalsIgnoreCase("restore") || type.equalsIgnoreCase("rs")) {
            if (!call.getPlayer().hasPermission("prism.restore")
                    && !call.getPlayer().hasPermission("prism.wand.restore")) {
                call.getPlayer().sendMessage(Prism.messenger.playerError("You do not have permission for this."));
                return;
            }
            // If disabling this one
            if (oldwand instanceof RestoreWand) {
                call.getPlayer().sendMessage(Prism.messenger
                        .playerHeaderMsg("Restore wand " + ChatColor.RED + "disabled" + ChatColor.WHITE + "."));
            } else {
                wand = new RestoreWand(plugin);
                call.getPlayer().sendMessage(Prism.messenger.playerHeaderMsg(
                        "Restore wand " + ChatColor.GREEN + "enabled" + ChatColor.WHITE + wandOn + parameters + "."));
                enabled = true;
            }
        }

        /*
          Off
         */
        else if (type.equalsIgnoreCase("off")) {
            call.getPlayer().sendMessage(Prism.messenger
                    .playerHeaderMsg("Current wand " + ChatColor.RED + "disabled" + ChatColor.WHITE + "."));
        }

        // Not a valid wand
        else {
            call.getPlayer().sendMessage(Prism.messenger.playerError("Invalid wand type. Use /prism ? for help."));
            return;
        }

        final PlayerInventory inv = call.getPlayer().getInventory();
        if (enabled) {

            if (item_material == null) {
                if (Objects.equals(mode, "block")) {
                    item_material = Material.SPRUCE_LOG;
                } else if (Objects.equals(mode, "item")) {
                    item_material = Material.STICK;
                } else {
                    item_material = Material.AIR;
                }
            }

            wand.setWandMode(mode);
            wand.setItem(item_material);

            Prism.debug("Wand activated for player - mode: " + mode + " Item:" + item_material);

            // Move any existing item to the hand, otherwise give it to them
            if (plugin.getConfig().getBoolean("prism.wands.auto-equip")) {
                if (!InventoryUtils.moveItemToHand(inv, item_material)) {
                    // Store the item they're holding, if any
                    wand.setOriginallyHeldItem(inv.getItemInMainHand());
                    // They don't have the item, so we need to give them an item
                    if (InventoryUtils.handItemToPlayer(inv, new ItemStack(item_material, 1))) {
                        wand.setItemWasGiven(true);
                    } else {
                        call.getPlayer().sendMessage(
                                Prism.messenger.playerError("Can't fit the wand item into your inventory."));
                    }
                }
                InventoryUtils.updateInventory(call.getPlayer());
            }

            // Let's build the QueryParameters for it if it's a Query wand.
            if (wand instanceof QueryWandBase) {
                if (!((QueryWandBase) wand).setParameters(call.getPlayer(), call.getArgs(), (isInspect ? 1 : 2))) {
                    // This
                    // returns
                    // if
                    // it
                    // was
                    // successful
                    call.getPlayer().sendMessage(Prism.messenger.playerError("Notice: Only some parameters used.."));
                }
            }

            // Store
            Prism.playersWithActiveTools.put(call.getPlayer().getName(), wand);
        } else {
            if (oldwand != null) {
                oldwand.disable(call.getPlayer());
            }
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}