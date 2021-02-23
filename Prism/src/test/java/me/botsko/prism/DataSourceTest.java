package me.botsko.prism;

import me.botsko.prism.actionlibs.ActionRegistry;
import me.botsko.prism.config.ConfigHandler;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.settings.Settings;
import me.botsko.prism.testHelpers.TestPrismDataSource;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.MemoryConfiguration;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.spongepowered.configurate.ConfigurateException;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.yaml.YamlConfigurationLoader;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

/**
 * Created for the Prism-Bukkit Project.
 *
 * @author Narimm
 * @since 2.1.8
 */
public class DataSourceTest {

    private static PrismLogHandler logHandler;

    @BeforeAll
    static void beforeAll() {
        logHandler = new PrismLogHandler();
    }

    @Test
    void createDataSource() {
        try {
            Path path  = Files.createTempFile("config",".yml");
            ConfigHandler handler =new ConfigHandler();
            handler.loadConfiguration(path);
            ConfigurationNode dataConfig = handler.getDataSourceConfig();

        dataConfig.node("type").set("derby");
            dataConfig.node("prefix").set("prism_");
        TestPrismDataSource.updateDefaultConfig(dataConfig);
        PrismDataSource dataSource = new TestPrismDataSource(dataConfig);
        dataSource.createDataSource();
            ActionRegistry registry = new ActionRegistry();
            dataSource.setupDatabase(registry);
            Settings.setDataSource(dataSource);
            Assertions.assertEquals(0,getClientDbSchemaVersion());
            DatabaseUpdater updater = new DatabaseUpdater(dataSource);
            updater.applyUpdates(dataSource);
            Assertions.assertEquals(8,getClientDbSchemaVersion());
            DatabaseMetaData meta = dataSource.getDataSource().getConnection().getMetaData();
            String[] tbls = {"TABLE"};
            ResultSet set = meta.getTables(null,null,"PRISM_%",tbls);
            List<String> tables = new ArrayList<>();
            while (set.next()) {
                int count = set.getMetaData().getColumnCount()+1;
                for (int i = 1; i < count; i++) {
                    String data = set.getString(i);
                    String name = set.getMetaData().getColumnName(i);
                    if (name.equals("TABLE_NAME")) {
                        tables.add(data);
                    }
                    System.out.println(name + " : " + data);
                }

            }

            for (String table : tables) {
                PreparedStatement statement = dataSource.getDataSource().getConnection()
                        .prepareStatement("SELECT * FROM " + table);
                ResultSet rs = statement.executeQuery();
                System.out.println("************" + table + "******************");
                while (rs.next()) {
                    int count = rs.getMetaData().getColumnCount()+1;
                    for (int i = 1; i < count; i++) {
                        String data = rs.getString(i);
                        String name = rs.getMetaData().getColumnName(i);
                        if (name.equals("TABLE_NAME")) {
                            tables.add(data);
                        }
                        System.out.println(name + " : " + data);
                    }
                }

            }
        } catch (IOException e) {
            e.printStackTrace();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    private int getClientDbSchemaVersion() {
        final String schema_ver = Settings.getSetting("schema_ver");
        if (schema_ver != null) {
            return Integer.parseInt(schema_ver);
        }
        return 0;
    }
}
