package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.settings.Settings;
import me.botsko.prism.utils.ItemUtils;
import me.botsko.prism.wands.Wand;
import org.bukkit.ChatColor;
import org.bukkit.Material;

import java.util.ArrayList;
import java.util.List;

public class SetmyCommand implements SubHandler {

    /**
     *
     */
    private final Prism plugin;

    /**
     * @param plugin
     * @return
     */
    public SetmyCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command
     */
    @Override
    public void handle(CallInfo call) {

        String setType = null;
        if (call.getArgs().length >= 2) {
            setType = call.getArg(1);
        }

		/*
		  Inspector wand
		 */
        if (setType != null && !setType.equalsIgnoreCase("wand")) {
            call.getPlayer().sendMessage(Prism.messenger.playerError("Invalid arguments. Use /prism ? for help."));
            return;
        }

        if (!plugin.getConfig().getBoolean("prism.wands.allow-user-override")) {
            call.getPlayer().sendMessage(
                    Prism.messenger.playerError("Sorry, but personalizing the wand is currently not allowed."));
        }

        // Check for any wand permissions. @todo There should be some central
        // way to handle this - some way to centralize it at least
        if (!call.getPlayer().hasPermission("prism.rollback") && !call.getPlayer().hasPermission("prism.restore")
                && !call.getPlayer().hasPermission("prism.wand.*")
                && !call.getPlayer().hasPermission("prism.wand.inspect")
                && !call.getPlayer().hasPermission("prism.wand.profile")
                && !call.getPlayer().hasPermission("prism.wand.rollback")
                && !call.getPlayer().hasPermission("prism.wand.restore")) {
            call.getPlayer().sendMessage(Prism.messenger.playerError("You do not have permission for this."));
            return;
        }

        // Disable any current wand
        if (Prism.playersWithActiveTools.containsKey(call.getPlayer().getName())) {
            final Wand oldwand = Prism.playersWithActiveTools.get(call.getPlayer().getName());
            oldwand.disable(call.getPlayer());
            Prism.playersWithActiveTools.remove(call.getPlayer().getName());
            call.getPlayer().sendMessage(Prism.messenger
                    .playerHeaderMsg("Current wand " + ChatColor.RED + "disabled" + ChatColor.WHITE + "."));
        }

        String setSubType = null;
        if (call.getArgs().length >= 3) {
            setSubType = call.getArg(2).toLowerCase();
        }

		/*
		  Set your custom wand mode to "hand", "item", or "block"
		 */
        if (setSubType != null && setSubType.equals("mode")) {

            String setWandMode = null;
            if (call.getArgs().length >= 4) {
                setWandMode = call.getArg(3);
            }
            if (setWandMode != null
                    && (setWandMode.equals("hand") || setWandMode.equals("item") || setWandMode.equals("block"))) {
                Settings.saveSetting("wand.mode", setWandMode, call.getPlayer());
                // Delete the item so we don't confuse people.
                Settings.deleteSetting("wand.item", call.getPlayer());
                call.getPlayer().sendMessage(Prism.messenger.playerHeaderMsg(
                        "Changed your personal wand to " + ChatColor.GREEN + setWandMode + ChatColor.WHITE + " mode."));
                return;
            }
            call.getPlayer().sendMessage(Prism.messenger.playerError("Invalid arguments. Use /prism ? for help."));
            return;
        }

		/*
		  Set your custom wand item for either "item" or "block" modes
		 */
        if (setSubType != null && setSubType.equals("item")) {
            if (call.getArgs().length >= 4) {
                String wandString = call.getArg(3);
                Material setWand = Material.matchMaterial(wandString);

                // If non-material, check for name
                if (setWand == null) {
                    final ArrayList<Material> itemMaterials = Prism.getItems().getMaterialsByAlias(wandString);
                    if (itemMaterials.size() > 0) {
                        setWand = itemMaterials.get(0);
                    } else {
                        call.getPlayer()
                                .sendMessage(Prism.messenger.playerError("There's no item matching that name."));
                        return;
                    }
                }

                if (ItemUtils.isBadWand(setWand)) {
                    call.getPlayer().sendMessage(
                            Prism.messenger.playerError("Sorry, but you may not use " + wandString + " for a wand."));
                    return;
                }

                Settings.saveSetting("wand.item", wandString, call.getPlayer());
                call.getPlayer().sendMessage(Prism.messenger.playerHeaderMsg(
                        "Changed your personal wand item to " + ChatColor.GREEN + wandString + ChatColor.WHITE + "."));
                return;
            }
        }
        call.getPlayer().sendMessage(Prism.messenger.playerError("Invalid arguments. Use /prism ? for help."));
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}