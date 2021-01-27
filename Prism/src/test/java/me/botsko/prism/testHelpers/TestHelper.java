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
        server.getScheduler().waitAsyncTasksFinished();
        server.getScheduler().performTicks(100);
        return server;
    }

    public static void shutdown() {
        PrismDataSource dataSource = Prism.getPrismDataSource();
        if (dataSource != null) {
            Collection<String> sql = new ArrayList<>();
            sql.add("DROP TABLE prism_actions");
            sql.add("DROP TABLE prism_data");
            sql.add("DROP TABLE prism_data_extra");
            sql.add("DROP TABLE prism_meta");
            sql.add("DROP TABLE prism_players");
            sql.add("DROP TABLE prism_worlds");
            sql.add("DROP TABLE prism_id_map");
            sql.add("DROP TABLE prism_players");
            sql.forEach(s -> {

                try (
                        Connection conn = Prism.getPrismDataSource()
                                .getDataSource().getConnection()
                ) {
                    PreparedStatement statement = conn.prepareStatement(s);
                    statement.execute();
                } catch (SQLException e) {
                    System.out.println(e.getMessage());
                }
            });
        }
        MockBukkit.getMock().getScheduler().cancelTasks(plugin);
        MockBukkit.getMock().getPluginManager().disablePlugins();
        MockBukkit.getMock().shutdown();
        MockBukkit.unmock();
    }
}
