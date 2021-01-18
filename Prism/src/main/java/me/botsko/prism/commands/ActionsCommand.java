package me.botsko.prism.commands;

import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionTypeImpl;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.command.CommandSender;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map.Entry;
import java.util.TreeMap;

public class ActionsCommand implements SubHandler {

    /**
     * Handle the command.
     */
    @Override
    public void handle(CallInfo call) {
        help(call.getSender());
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }

    @Override
    public String[] getHelp() {
        return new String[]{Il8nHelper.getRawMessage("help-action-list")};
    }

    @Override
    public String getRef() {
        return "/parameters.html#actions-list";
    }

    /**
     * Display param help.
     *
     * @param sender CommandSender
     */
    private void help(CommandSender sender) {

        Prism.messenger.sendMessage(sender,
                Prism.messenger.playerHeaderMsg(
                        Component.text("--- " + Il8nHelper.getRawMessage("action-list-header") + "---")
                                .color(NamedTextColor.GOLD)));
        // Build short list
        final List<String> shortNames = new ArrayList<>();
        final TreeMap<String, ActionTypeImpl> actions = Prism.getActionRegistry().getRegisteredAction();
        for (final Entry<String, ActionTypeImpl> entry : actions.entrySet()) {
            if (entry.getKey().contains("prism")) {
                continue;
            }
            if (shortNames.contains(entry.getValue().getShortName())) {
                continue;
            }
            shortNames.add(entry.getValue().getShortName());
        }
        // Sort alphabetically
        Collections.sort(shortNames);

        // Build display of shortname list
        StringBuilder actionList = new StringBuilder();
        int i = 1;
        for (final String shortName : shortNames) {
            actionList.append(shortName).append(i < shortNames.size() ? ", " : "");
            i++;
        }
        Prism.messenger.sendMessage(sender, Prism.messenger
                .playerMsg(Il8nHelper.getMessage("action-alias", ": ").color(NamedTextColor.LIGHT_PURPLE)
                        .append(Component.text(actionList.toString()))));
        // Build display of full actions
        actionList = new StringBuilder();
        i = 1;
        for (final Entry<String, ActionTypeImpl> entry : actions.entrySet()) {
            if (entry.getKey().contains("prism")) {
                continue;
            }
            actionList.append(entry.getKey()).append(i < actions.size() ? ", " : "");
            i++;
        }
        Prism.messenger.sendMessage(sender, Prism.messenger
                .playerMsg(Il8nHelper.getMessage("full-action-alias", ": ")
                        .append(Component.text(actionList.toString()))));

    }
}