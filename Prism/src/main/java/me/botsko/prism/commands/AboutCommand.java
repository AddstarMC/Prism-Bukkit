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
                Component.text("Help: ")
                        .append(Component.text("/pr ?")
                                .color(NamedTextColor.WHITE))));
        Prism.messenger.sendMessage(call.getSender(),
                Prism.messenger.playerSubduedHeaderMsg(
                        Component.text().content("Discord: ")
                                .append(Component.text(Il8nHelper.getRawMessage("discord-url"))
                                        .color(NamedTextColor.WHITE))
                                .clickEvent(ClickEvent.openUrl(Il8nHelper.getRawMessage("discord-url")))
                                .build()));
        Prism.messenger.sendMessage(call.getSender(),
                Prism.messenger.playerSubduedHeaderMsg(
                        Component.text().content("Wiki: ")
                                .append(Component.text(Il8nHelper.getRawMessage("wiki-url"))
                                        .color(NamedTextColor.WHITE))
                                .clickEvent(ClickEvent.openUrl(Il8nHelper.getRawMessage("wiki-url")))
                                .build()));
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }

    @Override
    public String[] getHelp() {
        return new String[]{Il8nHelper.getRawMessage("help-about")};
    }

    @Override
    public String getRef() {
        return ".html";
    }
}