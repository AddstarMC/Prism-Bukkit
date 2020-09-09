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
                .append(Il8n.getMessage("params-help-action")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("b:[block] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-help-block")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("before:[time] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-help-before")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("e:[entity] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-help-entity")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("id:[#] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-help-id")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("k:[text] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-help-keyword")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("p:[player] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-help-player")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("since:[time] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-help-since")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("t:[time] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-help-time")).append(TextComponent.newline())
                .append(Prism.messenger.playerMsg(TextComponent.of("w:[world] ").color(NamedTextColor.LIGHT_PURPLE)))
                .append(Il8n.getMessage("params-help-world")).append(TextComponent.newline())
                .append(Il8n.getMessage("params-help-prefix")).append(TextComponent.newline())
                .append(Il8n.getMessage("params-help-partial")).append(TextComponent.newline())
                .append(Il8n.getMessage("params-help-actionlist")).append(TextComponent.newline());
        Prism.messenger.sendMessage(s,builder.build());
    }

    private static Component colourParamHelp(TextComponent message) {
        Pattern pattern = Pattern.compile("([abtrkpew]|id|since|before){1}:([\\[,<,a-z,0-9,>,|,:,\\],#]*)");
        return message.replaceText(pattern, builder -> builder.color(NamedTextColor.LIGHT_PURPLE));
    }
}