package me.botsko.prism.commands;

import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.Executor;
import me.botsko.prism.commandlibs.SubCommand;
import me.botsko.prism.commandlibs.SubHandler;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.identity.Identity;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.TextComponent;
import net.kyori.adventure.text.event.ClickEvent;
import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.command.CommandSender;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.function.Consumer;

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

    @Override
    public String getHelp() {
        return "Displays Help";
    }

    @Override
    public String getRef() {
        return "";
    }

    /**
     * Displays help.
     *
     * @param s CommandSender
     */
    protected void help(CommandSender s) {
        Audience sender = Prism.getAudiences().sender(s);
        if (failed) {
            sender.sendMessage(Identity.nil(),
                  Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("prism-disabled-header")
                        .color(NamedTextColor.GOLD))
                        .append(Component.newline())
                        .append(
                              Prism.messenger.playerMsg(Il8nHelper.getMessage("prism-disabled-content"))
                                    .color(NamedTextColor.GOLD))
                        .append(Component.newline())
                        .append(
                              Prism.messenger.playerSubduedHeaderMsg(Il8nHelper.getMessage("discord", ":")
                                    .color(NamedTextColor.WHITE)
                                    .append(Component.text("https://discord.gg/Y9Qx3V"))))
                        .append(Component.newline())
                        .append(
                              Prism.messenger.playerSubduedHeaderMsg(Il8nHelper.getMessage("wiki", ":")
                                    .color(NamedTextColor.WHITE)
                                    .append(Component.text("https://prism-bukkit.readthedocs.io/")))));
            return;
        }
        TextComponent component = Prism.messenger.playerHeaderMsg(
                Component.text("--- Basic Usage ---").color(NamedTextColor.GOLD));
        Collection<SubCommand> commands = Prism.getInstance().getCommands().getSubCommands();
        for (SubCommand command:commands) {
            if (command.getHelp().length > 1) {
                for (String message : command.getHelp()) {
                    component.append(Prism.messenger.playerHelp(Arrays.toString(command.getAliases()), message))
                            .clickEvent(ClickEvent.clickEvent(ClickEvent.Action.OPEN_URL, command.getWebLink()))
                            .append(Component.newline());
                }
            } else {
                component.append(Prism.messenger.playerHelp(
                        Arrays.toString(command.getAliases()), command.getHelp()[0]))
                        .clickEvent(ClickEvent.clickEvent(ClickEvent.Action.OPEN_URL, command.getWebLink()))
                        .append(Component.newline());
            }
        }
        sender.sendMessage(Identity.nil(),
              Prism.messenger.playerHeaderMsg(
                    Component.text("--- Basic Usage ---").color(NamedTextColor.GOLD))
                    .append(
                          Prism.messenger.playerHelp("i", Il8nHelper.getRawMessage("help-inspector-wand")))
                    .append(Prism.messenger.playerHelp("near",
                          Il8nHelper.getRawMessage("help-near")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("pg (#|next|prev)", Il8nHelper.getRawMessage("help-pg-nav")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("params", Il8nHelper.getRawMessage("help-params")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("actions", Il8nHelper.getRawMessage("help-action-list")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("flags", Il8nHelper.getRawMessage("help-flag-list")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("(preview|pv) (rollback|rb) (params)",
                                Il8nHelper.getRawMessage("help-rollback-preview")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("(preview|pv) (restore|rs) (params)",
                                Il8nHelper.getRawMessage("help-restore-preview")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("(preview|pv) apply",
                          Il8nHelper.getRawMessage("help-apply-preview")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("(preview|pv) cancel",
                          Il8nHelper.getRawMessage("help-cancel-preview")))
                    .append(Component.newline())
                    .append(Prism.messenger
                          .playerHelp("(rollback|rb) (params)", Il8nHelper.getRawMessage("help-rollback")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("(restore|rs) (params)",
                          Il8nHelper.getRawMessage("help-restore")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("(w|wand) profile",
                          Il8nHelper.getRawMessage("help-profile-wand")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("(w|wand) rollback",
                          Il8nHelper.getRawMessage("help-rollback-wand")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("(w|wand) restore",
                          Il8nHelper.getRawMessage("help-restore-wand")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("(w|wand) off", Il8nHelper.getRawMessage("help-wand-off")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("undo", Il8nHelper.getRawMessage("help-undo")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("ex (r)",
                          Il8nHelper.getRawMessage("help-extinguish-radius")))
                    .append(Component.newline())
                    .append(Prism.messenger.playerHelp("drain (r)",
                          Il8nHelper.getRawMessage("help-drain-radius"))));
    }
}