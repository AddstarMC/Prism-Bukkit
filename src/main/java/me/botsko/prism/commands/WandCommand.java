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
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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

    @Override
    public void handle(CallInfo call) {
        String type = "i";
        final boolean isInspect = call.getArg(0).equalsIgnoreCase("inspect") || call.getArg(0).equalsIgnoreCase("i");
        if (!isInspect) {
            if (call.getArgs().length < 2) {
                call.getPlayer().sendMessage(
                        Prism.messenger.playerError("You need to specify a wand type. Use '/prism ?' for help."));

            } else {
                type = call.getArg(1);
            }
        }

        Wand oldWand = null;
        if (Prism.playersWithActiveTools.containsKey(call.getPlayer().getName())) {
            // Pull the wand in use
            oldWand = Prism.playersWithActiveTools.get(call.getPlayer().getName());
        }

        // Always remove the old one
        Prism.playersWithActiveTools.remove(call.getPlayer().getName());

        // Determine default mode
        String mode = plugin.getConfig().getString("prism.wands.default-mode","inspect");

        // Check if the player has a personal override
        if (plugin.getConfig().getBoolean("prism.wands.allow-user-override")) {
            final String personalMode = Settings.getSetting("wand.mode", call.getPlayer());
            if (personalMode != null) {
                mode = personalMode;
            }
        }

        // Determine which item we're using.
        String toolKey = null;
        if ("item".equals(mode)) {
            toolKey = plugin.getConfig().getString("prism.wands.default-item-mode-id");
        } else if ("block".equals(mode)) {
            toolKey = plugin.getConfig().getString("prism.wands.default-block-mode-id");
        }

        // Check if the player has a personal override
        if (plugin.getConfig().getBoolean("prism.wands.allow-user-override")) {
            final String personalToolKey = Settings.getSetting("wand.item", call.getPlayer());
            if (personalToolKey != null) {
                toolKey = personalToolKey;
            }
        }

        Material itemMaterial = null;

        if (toolKey != null) {
            itemMaterial = Material.matchMaterial(toolKey);
        }

        String wandOn = "";
        String itemName = "";
        StringBuilder parameters = new StringBuilder();
        if (itemMaterial != null) {
            itemName = Prism.getItems().getAlias(itemMaterial, null);
            wandOn += " on a " + itemName;
        }

        for (int i = (isInspect ? 1 : 2); i < call.getArgs().length; i++) {
            if (parameters.length() == 0) {
                parameters.append(" using:").append(ChatColor.GRAY);
            }
            parameters.append(" ").append(call.getArg(i));
        }

        if (ItemUtils.isBadWand(itemMaterial)) {
            call.getPlayer().sendMessage(
                    Prism.messenger.playerError("Sorry, but you may not use " + itemName + " for a wand."));
            return;
        }

        boolean enabled = false;
        Wand wand = null;
        switch (type.toLowerCase()) {
            case "p":
            case "profile":
                if (!call.getPlayer().hasPermission("prism.lookup")
                        && !call.getPlayer().hasPermission("prism.wand.profile")) {
                    call.getPlayer().sendMessage(Prism.messenger.playerError("You do not have permission for this."));
                    return;
                }
                if (oldWand instanceof ProfileWand) {
                    call.getPlayer().sendMessage(Prism.messenger
                            .playerHeaderMsg("Profile wand " + ChatColor.RED + "disabled" + ChatColor.WHITE + "."));
                } else {
                    wand = new ProfileWand();
                    call.getPlayer().sendMessage(Prism.messenger.playerHeaderMsg(
                            "Profile wand " + ChatColor.GREEN + "enabled" + ChatColor.WHITE + wandOn + "."));
                    enabled = true;
                }
                break;
            case "rb":
            case "rollback":
                if (!call.getPlayer().hasPermission("prism.rollback")
                        && !call.getPlayer().hasPermission("prism.wand.rollback")) {
                    call.getPlayer().sendMessage(Prism.messenger.playerError("You do not have permission for this."));
                    return;
                }
                if (oldWand instanceof RollbackWand) {
                    call.getPlayer().sendMessage(Prism.messenger
                            .playerHeaderMsg("Rollback wand " + ChatColor.RED + "disabled" + ChatColor.WHITE + "."));
                } else {
                    wand = new RollbackWand(plugin);
                    call.getPlayer().sendMessage(Prism.messenger.playerHeaderMsg(
                            "Rollback wand " + ChatColor.GREEN + "enabled" + ChatColor.WHITE + wandOn
                                    + parameters + "."));
                    enabled = true;
                }
                break;
            case "restore":
            case "rs":
                if (!call.getPlayer().hasPermission("prism.restore")
                        && !call.getPlayer().hasPermission("prism.wand.restore")) {
                    call.getPlayer().sendMessage(Prism.messenger.playerError("You do not have permission for this."));
                    return;
                }
                // If disabling this one
                if (oldWand instanceof RestoreWand) {
                    call.getPlayer().sendMessage(Prism.messenger
                            .playerHeaderMsg("Restore wand " + ChatColor.RED + "disabled" + ChatColor.WHITE + "."));
                } else {
                    wand = new RestoreWand(plugin);
                    call.getPlayer().sendMessage(Prism.messenger.playerHeaderMsg(
                            "Restore wand " + ChatColor.GREEN + "enabled" + ChatColor.WHITE + wandOn
                                    + parameters + "."));
                    enabled = true;
                }
                break;
            case "i":
            case "inspect":
                if (!call.getPlayer().hasPermission("prism.lookup")
                        && !call.getPlayer().hasPermission("prism.wand.inspect")) {
                    call.getPlayer().sendMessage(Prism.messenger.playerError("You do not have permission for this."));
                    return;
                }
                if (oldWand instanceof InspectorWand) {
                    call.getPlayer().sendMessage(Prism.messenger
                            .playerHeaderMsg("Inspection wand " + ChatColor.RED + "disabled" + ChatColor.WHITE + "."));
                } else {
                    wand = new InspectorWand(plugin);
                    call.getPlayer().sendMessage(Prism.messenger.playerHeaderMsg("Inspection wand " + ChatColor.GREEN
                            + "enabled" + ChatColor.WHITE + wandOn + parameters + "."));
                    enabled = true;
                }
                break;
            case "off":
                call.getPlayer().sendMessage(Prism.messenger
                        .playerHeaderMsg("Current wand " + ChatColor.RED + "disabled" + ChatColor.WHITE + "."));
                break;
            default:
                call.getPlayer().sendMessage(Prism.messenger.playerError("Invalid wand type. Use /prism ? for help."));
                return;
        }
        setupWand(enabled,call,itemMaterial,wand,mode,isInspect,oldWand);
    }

    private void setupWand(boolean enabled, @NotNull  CallInfo call, @Nullable Material itemMaterial,
                           @Nullable Wand wand, String mode, boolean isInspect, @Nullable Wand oldWand) {
        final PlayerInventory inv = call.getPlayer().getInventory();


        if (enabled && wand != null) {

            if (itemMaterial == null) {
                if (Objects.equals(mode, "block")) {
                    itemMaterial = Material.SPRUCE_LOG;
                } else if (Objects.equals(mode, "item")) {
                    itemMaterial = Material.STICK;
                } else {
                    itemMaterial = Material.AIR;
                }
            }

            wand.setWandMode(mode);
            wand.setItem(itemMaterial);

            Prism.debug("Wand activated for player - mode: " + mode + " Item:" + itemMaterial);

            // Move any existing item to the hand, otherwise give it to them
            if (plugin.getConfig().getBoolean("prism.wands.auto-equip")) {
                if (!InventoryUtils.moveItemToHand(inv, itemMaterial)) {
                    // Store the item they're holding, if any
                    wand.setOriginallyHeldItem(inv.getItemInMainHand());
                    // They don't have the item, so we need to give them an item
                    if (InventoryUtils.handItemToPlayer(inv, new ItemStack(itemMaterial, 1))) {
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
                    Prism.debug("Wand Parameters:" + ((QueryWandBase) wand).getParameters().toString());
                    call.getPlayer().sendMessage(Prism.messenger.playerError("Notice: Only some parameters used.."));
                }
            }

            // Store
            Prism.playersWithActiveTools.put(call.getPlayer().getName(), wand);
        } else {
            if (oldWand != null) {
                oldWand.disable(call.getPlayer());
            }
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}