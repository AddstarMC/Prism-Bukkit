package me.botsko.prism.testHelpers;

import be.seeseemelk.mockbukkit.MockBukkit;
import be.seeseemelk.mockbukkit.ServerMock;
import me.botsko.prism.PrismTestPlugin;
import org.bstats.bukkit.Metrics;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 22/11/2020.
 */
public class TestHelper {

    private PrismTestPlugin plugin;

    public ServerMock setup() {
        ServerMock server = MockBukkit.getOrCreateMock();
        server.addSimpleWorld("Normal");
        Metrics metrics = null;
        plugin = MockBukkit.load(PrismTestPlugin.class);
        server.getScheduler().performTicks(20);
        return server;
    }

    public void shutdown() {
        Path path = plugin.getDataFolder().toPath();
        MockBukkit.getMock().getScheduler().cancelTasks(plugin);
        MockBukkit.getMock().getPluginManager().disablePlugins();
        try {

            Files.list(path).forEach(path1 -> {
                try {
                    Files.delete(path1);
                } catch (IOException e) {
                    e.printStackTrace();
                }
            });
            Files.delete(path);

        } catch (IOException e) {
            e.printStackTrace();
        }
        MockBukkit.getMock().shutdown();
        MockBukkit.unmock();
    }

    public static boolean isEnabled(TestHelper helper) {
        return  helper.plugin.isEnabled();
    }

    public static void shutdownHelper(TestHelper helper) {
        helper.shutdown();
    }
}
