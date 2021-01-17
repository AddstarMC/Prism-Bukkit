package me.botsko.prism.database;

import org.bukkit.command.CommandSender;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 5/04/2019.
 */
public interface BlockReportQuery extends SelectQuery {
    void report(CommandSender sender);
}
