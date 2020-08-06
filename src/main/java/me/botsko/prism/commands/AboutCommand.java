package me.botsko.prism.commands;

import me.botsko.prism.Il8n;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import net.kyori.adventure.text.TextComponent;
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
                        Il8n.getMessage("about-header")
                                .replaceText(Pattern.compile("<author>"),
                                        builder -> TextComponent.builder()
                                                .content("The AddstarMC Network")
                                                .color(NamedTextColor.GOLD))
                                .replaceText(Pattern.compile("<version>"),
                                        builder -> TextComponent.builder().content(plugin.getPrismVersion()))));
        Prism.messenger.sendMessage(call.getSender(), Prism.messenger.playerSubduedHeaderMsg(
                TextComponent.builder().content("Help: ")
                        .append(TextComponent.of("/pr ?")
                                .color(NamedTextColor.WHITE))
                        .build()));
        Prism.messenger.sendMessage(call.getSender(),
                Prism.messenger.playerSubduedHeaderMsg(
                        TextComponent.builder().content("Discord: ")
                                .append(TextComponent.of("https://discord.gg/Y9Qx3V")
                                        .color(NamedTextColor.WHITE))
                                .clickEvent(ClickEvent.openUrl("https://discord.gg/Y9Qx3V"))
                                .build()));
        Prism.messenger.sendMessage(call.getSender(),
                Prism.messenger.playerSubduedHeaderMsg(
                        TextComponent.builder().content("Wiki: ")
                                .append(TextComponent.of("https://github.com/AddstarMC/Prism-Bukkit/wiki")
                                        .color(NamedTextColor.WHITE))
                                .clickEvent(ClickEvent.openUrl("https://github.com/AddstarMC/Prism-Bukkit/wiki"))
                                .build()));
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}