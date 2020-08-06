package me.botsko.prism.commands;

import me.botsko.prism.Il8n;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.TextComponent;
import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.ChatColor;
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
        Audience sender = Prism.getAudiences().audience(s);
        sender.sendMessage(Prism.messenger.playerHeaderMsg(Il8n.getMessage("params-header")
                .color(NamedTextColor.GOLD)));
        sender.sendMessage(Prism.messenger.playerMsg(colourParamHelp(
                Il8n.getMessage("params-radius-help"))));
        sender.sendMessage(Prism.messenger.playerMsg(
                colourParamHelp(Il8n.getMessage("params-radius-help2"))));
        sender.sendMessage(Prism.messenger.playerMsg(
                colourParamHelp(Il8n.getMessage("params-radius-help3"))));
        sender.sendMessage(Prism.messenger.playerMsg(
                colourParamHelp(Il8n.getMessage("params-radius-help4"))));
        sender.sendMessage(Prism.messenger.playerMsg(
                TextComponent.builder().content("---").color(NamedTextColor.GRAY).build()));
        sender.sendMessage(Prism.messenger.playerMsg(ChatColor.LIGHT_PURPLE + "a:[action]" + ChatColor.WHITE
                + " Like 'block-break' (See below for full list). No default."));
        sender.sendMessage(Prism.messenger.playerMsg(ChatColor.LIGHT_PURPLE + "b:[block]" + ChatColor.WHITE
                + " Like 'grass' or '2' or '2:0'. No default."));
        sender.sendMessage(Prism.messenger.playerMsg(ChatColor.LIGHT_PURPLE + "before:[time]" + ChatColor.WHITE
                + " Events prior to x long ago."));
        sender.sendMessage(Prism.messenger.playerMsg(ChatColor.LIGHT_PURPLE + "e:[entity]" + ChatColor.WHITE
                + " Like 'pig'. No default."));
        sender.sendMessage(Prism.messenger.playerMsg(ChatColor.LIGHT_PURPLE + "id:[#]" + ChatColor.WHITE
                + " Record id. Useful for single item rollbacks/restores without a wand."));
        sender.sendMessage(Prism.messenger.playerMsg(ChatColor.LIGHT_PURPLE + "k:[text]" + ChatColor.WHITE
                + " Keyword search. Mainly for command/chat logging."));
        sender.sendMessage(Prism.messenger.playerMsg(ChatColor.LIGHT_PURPLE + "p:[player]" + ChatColor.WHITE
                + " Like 'viveleroi'. No default."));
        sender.sendMessage(Prism.messenger.playerMsg(ChatColor.LIGHT_PURPLE + "since:[time]" + ChatColor.WHITE
                + " Events since to x long ago (same as t:)."));
        sender.sendMessage(Prism.messenger.playerMsg(ChatColor.LIGHT_PURPLE + "t:[time]" + ChatColor.WHITE
                + " Events since x long ago. Like 1s(seconds), 20m(minutes), 1h(hour), 7d(days), 2w(weeks). "
                + "Default based on config."));
        sender.sendMessage(Prism.messenger.playerMsg(ChatColor.LIGHT_PURPLE + "w:[world]" + ChatColor.WHITE
                + " Defaults to your current world."));
        sender.sendMessage(Prism.messenger.playerMsg("Prefix action, player, or entity names with ! to exclude. "
                + "Like p:!viveleroi"));
        sender.sendMessage(Prism.messenger.playerMsg("Prefix player names with ~ for partial match. Like p:~vive"));

        sender.sendMessage(Prism.messenger.playerMsg(ChatColor.GRAY + "Use " + ChatColor.WHITE + "/pr actions"
                + ChatColor.GRAY + " to view list of actions."));

    }

    private static Component colourParamHelp(TextComponent message) {
        Pattern pattern = Pattern.compile("([abtrkpew]|id|since|before){1}:([\\[,<,a-z,0-9,>,|,:,\\],#]*)");
        return message.replaceText(pattern, builder -> builder.color(NamedTextColor.LIGHT_PURPLE));
    }
}