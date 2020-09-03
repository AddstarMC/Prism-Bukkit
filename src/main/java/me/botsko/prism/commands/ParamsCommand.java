package me.botsko.prism.commands;

import me.botsko.prism.Il8n;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.TextComponent;
import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.command.CommandSender;

import java.util.List;
import java.util.regex.Pattern;

public class ParamsCommand implements SubHandler {

    @Override
    public void handle(CallInfo call) {
        help(call.getSender());
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }


    private void help(CommandSender s) {
        TextComponent.Builder builder = TextComponent.builder()
                .append(Prism.messenger.playerHeaderMsg(Il8n.getMessage("params-header")
                        .color(NamedTextColor.GOLD)).append(TextComponent.newline()))
                .append(Prism.messenger.playerMsg(colourParamHelp(
                        Il8n.getMessage("params-radius-help")))).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(
                        colourParamHelp(Il8n.getMessage("params-radius-help2")))).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(
                        colourParamHelp(Il8n.getMessage("params-radius-help3")))).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(
                        colourParamHelp(Il8n.getMessage("params-radius-help4")))).append(TextComponent.newline())
                .append(TextComponent.of("---").color(NamedTextColor.GRAY)).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of(" a[action]: ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-action-help")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("b:[block] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-block-help")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("before:[time] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-before-help")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("e:[entity] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-entity-help")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("id:[#] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-id-help")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("k:[text] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-keyword-help")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("p:[player] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-player-help")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("since:[time] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-since-help")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("t:[time] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-time-help")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("w:[world] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-world-help")).append(TextComponent.newline())
                .append(Il8n.getMessage("params-prefix-help")).append(TextComponent.newline())
                .append(Il8n.getMessage("params-partial-help")).append(TextComponent.newline())
                .append(Il8n.getMessage("params-actionlist-help")).append(TextComponent.newline());
        Prism.messenger.sendMessage(s,builder.build());
    }

    private static Component colourParamHelp(TextComponent message) {
        Pattern pattern = Pattern.compile("([abtrkpew]|id|since|before){1}:([\\[,<,a-z,0-9,>,|,:,\\],#]*)");
        return message.replaceText(pattern, builder -> builder.color(NamedTextColor.LIGHT_PURPLE));
    }
}