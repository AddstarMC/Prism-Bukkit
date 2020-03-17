package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import org.bukkit.ChatColor;

import java.util.List;

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
        call.getSender().sendMessage(Prism.messenger.playerHeaderMsg(
                "Prism - originally by Viveleroi; maintained by " + ChatColor.GOLD
                        + "The AddstarMC Network" + ChatColor.GRAY + " v"
                        + plugin.getPrismVersion()));
        call.getSender().sendMessage(Prism.messenger.playerSubduedHeaderMsg("Help: " + ChatColor.WHITE + "/pr ?"));
        call.getSender().sendMessage(
                Prism.messenger.playerSubduedHeaderMsg("Discord: " + ChatColor.WHITE + "https://discord.gg/Y9Qx3V"));
        call.getSender().sendMessage(
                Prism.messenger.playerSubduedHeaderMsg("Wiki: " + ChatColor.WHITE + "https://github.com/AddstarMC/Prism-Bukkit/wiki"));
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}