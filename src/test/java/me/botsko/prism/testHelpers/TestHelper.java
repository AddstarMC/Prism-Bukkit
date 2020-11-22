package me.botsko.prism.testHelpers;

import be.seeseemelk.mockbukkit.MockBukkit;
import be.seeseemelk.mockbukkit.ServerMock;
import me.botsko.prism.Prism;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 22/11/2020.
 */
public class TestHelper {

    public static ServerMock setup() {
        ServerMock server = MockBukkit.getOrCreateMock();
        server.addSimpleWorld("Normal");
        MockBukkit.load(Prism.class);
        server.getScheduler().waitAsyncTasksFinished();
        server.getScheduler().performTicks(100);
        return server;
    }

    public static void shutdown() {
        MockBukkit.getMock().getPluginManager().disablePlugins();
        MockBukkit.unmock();
        List<String> sql = new ArrayList<>();
        sql.add("SET FOREIGN_KEY_CHECKS=0;");
        sql.add("DROP TABLE IF EXISTS prism_actions;");
        sql.add("DROP TABLE IF EXISTS prism_data;");
        sql.add("DROP TABLE IF EXISTS prism_data_extra;");
        sql.add("DROP TABLE IF EXISTS prism_meta;");
        sql.add("DROP TABLE IF EXISTS prism_players;");
        sql.add("DROP TABLE IF EXISTS prism_worlds;");
        sql.add("DROP TABLE IF EXISTS prism_id_map;");
        sql.add("DROP TABLE IF EXISTS prism_players;");
        sql.forEach(s -> {
            try (
                    Connection conn = Prism.getPrismDataSource()
                            .getDataSource().getConnection()
            ) {
                PreparedStatement statement = conn.prepareStatement(s);
                statement.execute();
            } catch (SQLException e) {
                e.printStackTrace();
            }
        });
    }
}
