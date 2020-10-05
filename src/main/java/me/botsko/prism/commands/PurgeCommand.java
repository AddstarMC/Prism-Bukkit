package me.botsko.prism.commands;

import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.text.ReplaceableTextComponent;
import net.kyori.adventure.audience.Audience;
import net.kyori.adventure.text.Component;
import net.kyori.adventure.text.format.NamedTextColor;
import org.bukkit.Bukkit;

import java.util.List;
import java.util.Objects;

/**
 * Created for the Ark: Survival Evolved.
 * Created by Narimm on 5/06/2017.
 */
public class PurgeCommand implements SubHandler {

    private final Prism plugin;

    public PurgeCommand(Prism plugin) {
        this.plugin = plugin;
    }

    @Override
    public void handle(CallInfo call) {
        Audience sender = Prism.getAudiences().sender(call.getSender());
        if (call.getArgs().length < 1) {
            Prism.messenger.sendMessage(call.getSender(),
                    Prism.messenger.playerHeaderMsg(Component.text().content("Prism")
                            .append(Component.text(" v" + plugin.getPrismVersion()).color(NamedTextColor.GRAY))
                            .build()));
            sender.sendMessage(Prism.messenger.playerSubduedHeaderMsg(ReplaceableTextComponent.builder("purge-report")
                    .replace("<taskCount>", plugin.getSchedulePool().getTaskCount())
                    .replace("<purgesComplete>", plugin.getSchedulePool().getCompletedTaskCount())
                    .replace("<poolString>", plugin.getSchedulePool().toString())
                    .build()));
        } else {
            if (Objects.equals(call.getArgs()[0], "execute")) {
                sender.sendMessage(
                        Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("purge-execute")));
                Bukkit.getScheduler().runTaskAsynchronously(plugin, plugin.getPurgeManager());
            }
        }

    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}
