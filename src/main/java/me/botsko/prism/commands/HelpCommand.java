package me.botsko.prism.commands;

import me.botsko.prism.Il8n;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.text.TextComponent;
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
        Audience sender = Prism.getAudiences().audience(s);
        if (failed) {
            sender.sendMessage(Prism.messenger.playerHeaderMsg(ChatColor.GOLD + "--- Prism Disabled ---"));
            sender.sendMessage(Prism.messenger.playerMsg("Prism is running in a disabled mode because the"
                    + " database could not be connected. \n Please seek help by running /prism debug and perusing"
                    + " the contents to help with errors.  "));
            sender.sendMessage(
                    Prism.messenger.playerSubduedHeaderMsg("Discord: " + ChatColor.WHITE
                            + "https://discord.gg/Y9Qx3V"));
            sender.sendMessage(
                    Prism.messenger.playerSubduedHeaderMsg("Wiki: " + ChatColor.WHITE
                            + "https://github.com/AddstarMC/Prism-Bukkit/wiki\n"));
            return;
        }
        sender.sendMessage(Prism.messenger.playerHeaderMsg(
                TextComponent.of("--- Basic Usage ---").color(NamedTextColor.GOLD)));
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