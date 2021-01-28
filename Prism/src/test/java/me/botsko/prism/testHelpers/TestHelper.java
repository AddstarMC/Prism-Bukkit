package me.botsko.prism.testHelpers;

import be.seeseemelk.mockbukkit.MockBukkit;
import be.seeseemelk.mockbukkit.ServerMock;
import me.botsko.prism.Prism;
import me.botsko.prism.PrismTestPlugin;
import me.botsko.prism.database.PrismDataSource;
import org.bstats.bukkit.Metrics;
import org.bukkit.plugin.java.JavaPlugin;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Collection;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 22/11/2020.
 */
public class TestHelper {

    private static JavaPlugin plugin;

    public static ServerMock setup() {
        ServerMock server = MockBukkit.getOrCreateMock();
        server.addSimpleWorld("Normal");
        Metrics metrics = null;
        plugin = MockBukkit.load(PrismTestPlugin.class);
        server.getScheduler().performTicks(300);
        return server;
    }

    public static void shutdown() {
        MockBukkit.getMock().getScheduler().cancelTasks(plugin);
        MockBukkit.getMock().getPluginManager().disablePlugins();
        MockBukkit.getMock().shutdown();
        MockBukkit.unmock();
    }
}
