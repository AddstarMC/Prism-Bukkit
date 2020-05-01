package me.botsko.prism.commands;

import com.zaxxer.hikari.HikariDataSource;
import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.database.PrismDataSource;
import org.apache.commons.lang.exception.ExceptionUtils;
import org.bukkit.Bukkit;
import org.bukkit.Material;
import org.bukkit.command.CommandSender;
import org.bukkit.plugin.Plugin;
import org.kitteh.pastegg.PasteBuilder;
import org.kitteh.pastegg.PasteContent;
import org.kitteh.pastegg.PasteFile;
import org.kitteh.pastegg.Visibility;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by narimm on 3/03/2020.
 */
public class DebugCommand implements SubHandler {

    @Override
    public void handle(CallInfo call) {
        Bukkit.getScheduler().runTaskAsynchronously(Prism.getInstance(), () -> createPaste(
                call.getSender()));
    }

    private String getFile(Path file) {
        try {
            return new String(Files.readAllBytes(file), StandardCharsets.UTF_8);
        } catch (IOException e) {
            return ExceptionUtils.getFullStackTrace(e);
        }

    }

    private String getMainInfo() {
        StringBuilder mainInfo = new StringBuilder();
        mainInfo.append(Bukkit.getName()).append(" version: ").append(Bukkit.getServer()
                .getVersion()).append('\n');
        mainInfo.append("Plugin version: ").append(Prism.getInstance().getDescription()
                .getVersion()).append('\n');
        mainInfo.append("Java version: ").append(System.getProperty("java.version")).append('\n');
        mainInfo.append('\n');
        mainInfo.append("Plugins:\n");
        for (Plugin plugin : Bukkit.getPluginManager().getPlugins()) {
            mainInfo.append(' ').append(plugin.getName()).append(" - ").append(
                    plugin.getDescription().getVersion()).append('\n');
            mainInfo.append("  ").append(plugin.getDescription().getAuthors()).append('\n');
        }
        return mainInfo.toString();
    }

    private String getDataSourceInfo() {
        PrismDataSource dataSource = Prism.getPrismDataSource();
        StringBuilder out = new StringBuilder();
        String name = dataSource.getClass().getName();
        out.append("DataSource Name: ").append(name).append("/n");
        if (dataSource instanceof HikariDataSource) {
            out.append("Running: ").append(((HikariDataSource) dataSource).isRunning())
                    .append("Total Connections: ")
                    .append(((HikariDataSource) dataSource)
                            .getHikariPoolMXBean().getTotalConnections())
                    .append("/n")
                    .append("Total Connections: ")
                    .append(((HikariDataSource) dataSource)
                            .getHikariPoolMXBean().getActiveConnections())
                    .append("/n");
        }
        out.append("Illegal Blocks: /n");
        for (Material mat : Prism.getIllegalBlocks()) {
            out.append("   ").append(mat.name()).append("/n");
        }
        out.append("Worlds Tracked: ").append(Prism.prismWorlds.size()).append("/n");
        out.append("Players Tracked: ").append(Prism.prismPlayers.size()).append("/n");
        out.append("Players with Tools: ").append(Prism.playersWithActiveTools.size())
                .append("/n");
        return out.toString();
    }


    private void createPaste(CommandSender sender) {
        Path dataPath = Prism.getInstance().getDataFolder().toPath();
        Path prismConfig = dataPath.resolve("config.yml");
        Path hikariProps = dataPath.resolve("hikari.properties");
        PasteBuilder.PasteResult result = new PasteBuilder().name("Prism Debug Output")
                .visibility(Visibility.UNLISTED)
                .setApiKey(Prism.getPasteKey())
                .expireIn(24 * 60 * 60 * 1000) //1 day
                .addFile(new PasteFile("Main Info",
                        new PasteContent(PasteContent.ContentType.TEXT, getMainInfo())))
                .addFile(new PasteFile("config.yml",
                        new PasteContent(PasteContent.ContentType.TEXT, getFile(prismConfig))))
                .addFile(new PasteFile("hikari.properties",
                        new PasteContent(PasteContent.ContentType.TEXT, getFile(hikariProps))))
                .addFile(new PasteFile("dataSource Properties",
                        new PasteContent(PasteContent.ContentType.TEXT, getDataSourceInfo())))
                .build();
        if (result.getPaste().isPresent()) {
            sender.sendMessage(" Output Complete: paste available on https://paste.gg/"
                    + result.getPaste().get().getId());
        } else {
            sender.sendMessage("Could not Report results.  Please ask support for other methods"
                    + " of providing information");
        }
    }

    @Override
    public List<String> handleComplete(CallInfo call) {
        return null;
    }
}
