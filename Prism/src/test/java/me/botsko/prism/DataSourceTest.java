package me.botsko.prism;

import me.botsko.prism.actionlibs.ActionRegistry;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.settings.Settings;
import me.botsko.prism.testHelpers.TestPrismDataSource;
import org.bukkit.configuration.ConfigurationSection;
import org.bukkit.configuration.MemoryConfiguration;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

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
        ConfigurationSection section = new MemoryConfiguration();
        section.set("type","derby");
        section.set("prefix","prism_");
        TestPrismDataSource.updateDefaultConfig(section);
        PrismDataSource dataSource = new TestPrismDataSource(section);
        dataSource.createDataSource();
        ActionRegistry registry = new ActionRegistry();
        dataSource.setupDatabase(registry);
        Settings.setDataSource(dataSource);
        Assertions.assertEquals(0,getClientDbSchemaVersion());
        DatabaseUpdater updater = new DatabaseUpdater(dataSource);
        updater.applyUpdates(dataSource);
        Assertions.assertEquals(8,getClientDbSchemaVersion());
        try {
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
