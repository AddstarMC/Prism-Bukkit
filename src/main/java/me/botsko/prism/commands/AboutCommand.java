package me.botsko.prism.commands;

import com.zaxxer.hikari.HikariDataSource;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import org.bukkit.ChatColor;
import org.bukkit.Material;

import javax.sql.DataSource;
import java.util.List;

public class AboutCommand implements SubHandler {

    /**
     * The plugin.
     */
    private final Prism plugin;

    /**
     * Get the plugin information.
     *
     * @param plugin the Plugin
     */
    AboutCommand(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * Handle the command.
     */
    @Override
    public void handle(CallInfo call) {
        call.getSender().sendMessage(Prism.messenger.playerHeaderMsg(
              "Prism - By " + ChatColor.GOLD + "AddstarMC Team" + ChatColor.GRAY + " v"
                    + plugin.getPrismVersion()));
        call.getSender().sendMessage(Prism.messenger.playerSubduedHeaderMsg("Help: "
              + ChatColor.WHITE + "/pr ?"));
        call.getSender().sendMessage(
              Prism.messenger.playerSubduedHeaderMsg("Discord: " + ChatColor.WHITE
                    + "https://discord.gg/ThD8dK"));
        call.getSender().sendMessage(
              Prism.messenger.playerSubduedHeaderMsg("Wiki: " + ChatColor.WHITE
                    + "https://github.com/AddstarMC/Prism-Bukkit/wiki"));
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}