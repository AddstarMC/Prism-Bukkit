package me.botsko.prism.commands;

import me.botsko.prism.Il8n;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.command.CommandSender;

import java.util.List;

public class HelpCommand implements SubHandler {

    private final boolean failed;

    public HelpCommand(boolean failed) {
        this.failed = failed;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void handle(CallInfo call) {
        help(call.getSender());
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }

    /**
     * Displays help.
     *
     * @param s CommandSender
     */
    protected void help(CommandSender s) {
        Audience sender = Prism.getAudiences().sender(s);
        if (failed) {
            sender.sendMessage(
                    Prism.messenger.playerHeaderMsg(Il8n.getMessage("prism-disabled-header")
                            .color(NamedTextColor.GOLD)));
            sender.sendMessage(
                    Prism.messenger.playerMsg(Il8n.getMessage("prism-disabled-content")
                            .color(NamedTextColor.GOLD)));
            sender.sendMessage(
                    Prism.messenger.playerSubduedHeaderMsg(Il8n.getMessage("discord", ":")
                            .color(NamedTextColor.WHITE)
                            .append(Component.text("https://discord.gg/Y9Qx3V"))));
            sender.sendMessage(
                    Prism.messenger.playerSubduedHeaderMsg(Il8n.getMessage("wiki", ":")
                            .color(NamedTextColor.WHITE)
                            .append(Component.text("https://github.com/AddstarMC/Prism-Bukkit/wiki"))));
            return;
        }
        sender.sendMessage(Prism.messenger.playerHeaderMsg(
                Component.text("--- Basic Usage ---").color(NamedTextColor.GOLD)));
        sender.sendMessage(Prism.messenger.playerHelp("i", Il8n.getRawMessage("help-inspector-wand")));
        sender.sendMessage(Prism.messenger.playerHelp("(l|lookup) (params)", Il8n.getRawMessage("help-lookup")));
        sender.sendMessage(Prism.messenger.playerHelp("tp (#|id:#)", Il8n.getRawMessage("help-teleport")));
        sender.sendMessage(Prism.messenger.playerHelp("near", Il8n.getRawMessage("help-near")));
        sender.sendMessage(Prism.messenger.playerHelp("pg (#|next|prev)", Il8n.getRawMessage("help-pg-nav")));
        sender.sendMessage(Prism.messenger.playerHelp("params", Il8n.getRawMessage("help-params")));
        sender.sendMessage(Prism.messenger.playerHelp("actions", Il8n.getRawMessage("help-action-list")));
        sender.sendMessage(Prism.messenger.playerHelp("flags", Il8n.getRawMessage("help-flag-list")));
        sender.sendMessage(Prism.messenger.playerHelp("(preview|pv) (rollback|rb) (params)",
                Il8n.getRawMessage("help-rollback-preview")));
        sender.sendMessage(Prism.messenger.playerHelp("(preview|pv) (restore|rs) (params)",
                Il8n.getRawMessage("help-restore-preview")));
        sender.sendMessage(Prism.messenger.playerHelp("(preview|pv) apply", Il8n.getRawMessage("help-apply-preview")));
        sender.sendMessage(Prism.messenger.playerHelp("(preview|pv) cancel",
                Il8n.getRawMessage("help-cancel-preview")));
        sender.sendMessage(Prism.messenger.playerHelp("(rollback|rb) (params)", Il8n.getRawMessage("help-rollback")));
        sender.sendMessage(Prism.messenger.playerHelp("(restore|rs) (params)", Il8n.getRawMessage("help-restore")));
        sender.sendMessage(Prism.messenger.playerHelp("(w|wand) profile", Il8n.getRawMessage("help-profile-wand")));
        sender.sendMessage(Prism.messenger.playerHelp("(w|wand) rollback", Il8n.getRawMessage("help-rollback-wand")));
        sender.sendMessage(Prism.messenger.playerHelp("(w|wand) restore", Il8n.getRawMessage("help-restore-wand")));
        sender.sendMessage(Prism.messenger.playerHelp("(w|wand) off", Il8n.getRawMessage("help-wand-off")));
        sender.sendMessage(Prism.messenger.playerHelp("undo", Il8n.getRawMessage("help-undo")));
        sender.sendMessage(Prism.messenger.playerHelp("ex (r)", Il8n.getRawMessage("help-extinguish-radius")));
        sender.sendMessage(Prism.messenger.playerHelp("drain (r)", Il8n.getRawMessage("help-drain-radius")));

    }
}