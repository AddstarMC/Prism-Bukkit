package me.botsko.prism.commands;

import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.event.ClickEvent;
import net.kyori.adventure.text.format.NamedTextColor;

import java.util.List;
import java.util.regex.Pattern;

public class AboutCommand implements SubHandler {

    private final Prism plugin;

    public AboutCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command.
     */
    @Override
    public void handle(CallInfo call) {
        Prism.messenger.sendMessage(call.getSender(),
                Prism.messenger.playerHeaderMsg(
                        Il8nHelper.getMessage("about-header")
                                .replaceText(Pattern.compile("<author>"),
                                    builder -> Component.text()
                                             .content("The AddstarMC Network")
                                             .color(NamedTextColor.GOLD))
                                .replaceText(Pattern.compile("<version>"),
                                    builder -> Component.text().content(plugin.getPrismVersion()))));
        Prism.messenger.sendMessage(call.getSender(), Prism.messenger.playerSubduedHeaderMsg(
                Component.text().content("Help: ")
                        .append(Component.text("/pr ?")
                                .color(NamedTextColor.WHITE))
                        .build()));
        Prism.messenger.sendMessage(call.getSender(),
                Prism.messenger.playerSubduedHeaderMsg(
                        Component.text().content("Discord: ")
                                .append(Component.text("https://discord.gg/Y9Qx3V")
                                        .color(NamedTextColor.WHITE))
                                .clickEvent(ClickEvent.openUrl("https://discord.gg/Y9Qx3V"))
                                .build()));
        Prism.messenger.sendMessage(call.getSender(),
                Prism.messenger.playerSubduedHeaderMsg(
                        Component.text().content("Wiki: ")
                                .append(Component.text("https://github.com/AddstarMC/Prism-Bukkit/wiki")
                                        .color(NamedTextColor.WHITE))
                                .clickEvent(ClickEvent.openUrl("https://github.com/AddstarMC/Prism-Bukkit/wiki"))
                                .build()));
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}