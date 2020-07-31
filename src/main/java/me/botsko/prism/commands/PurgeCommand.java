package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import net.kyori.adventure.audience.Audience;
import org.bukkit.Bukkit;
import org.bukkit.ChatColor;

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
        Audience sender = Prism.getAudiences().audience(call.getSender());
        if (call.getArgs().length < 1) {
            sender.sendMessage(
                    Prism.messenger.playerHeaderMsg("Prism" + ChatColor.GRAY + " v"
                            + plugin.getPrismVersion()));
            sender.sendMessage(Prism.messenger.playerSubduedHeaderMsg("Purges Scheduled: "
                    + ChatColor.WHITE + plugin.getSchedulePool().getTaskCount()));
            sender.sendMessage(
                    Prism.messenger.playerSubduedHeaderMsg("Purges Run : " + ChatColor.WHITE
                            + plugin.getSchedulePool().getCompletedTaskCount()));
            sender.sendMessage(
                    Prism.messenger.playerSubduedHeaderMsg("Pool String: " + ChatColor.WHITE
                            + plugin.getSchedulePool().toString()));
        } else {
            if (Objects.equals(call.getArgs()[0], "execute")) {
                sender.sendMessage(
                        Prism.messenger.playerHeaderMsg("Prism" + ChatColor.GRAY + " Executing Purge Run"));
                Bukkit.getScheduler().runTaskAsynchronously(plugin, plugin.getPurgeManager());
            }
        }

    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}
