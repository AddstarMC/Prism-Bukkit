package me.botsko.prism.database;

import me.botsko.prism.Prism;
import me.botsko.prism.database.mysql.MySqlPrismDataSource;
import me.botsko.prism.database.sql.SQLPrismDataSourceUpdater;
import org.bukkit.configuration.Configuration;
import org.bukkit.configuration.ConfigurationSection;

import java.sql.Connection;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 4/04/2019.
 */
public class PrismDatabaseFactory {

    private static PrismDataSource database = null;

    public static void createDefaultConfig(Configuration configuration) {
        ConfigurationSection mysql;
        if (configuration.contains("prism.mysql")) {
            mysql = configuration.getConfigurationSection("prism.mysql");
        } else {
            mysql = configuration.createSection("prism.mysql");
        }
        MySqlPrismDataSource.updateDefaultConfig(mysql);
        addDatabaseDefaults(mysql);
    }

    private static void addDatabaseDefaults(ConfigurationSection section) {
        section.addDefault("database.max-pool-connections", 20);
        section.addDefault("database.min-idle-connections", 5);
        section.addDefault("database.max-wait", 30000);
        section.addDefault("database.max-failures-before-wait", 5);
        section.addDefault("database.actions-per-insert-batch", 300);
        // queue
        section.addDefault("database.force-write-queue-on-shutdown", true);
    }
    public static PrismDataSource createDataSource(Configuration  configuration) {
        if(configuration == null) return null;
        String dataSource = configuration.getString("datasource","mysql");
        if(dataSource == null)return null;
        switch (dataSource) {
            case "mysql":
                Prism.log("Attempting to configure datasource as " + dataSource);
                ConfigurationSection section = configuration.getConfigurationSection("prism.mysql");
                database = new MySqlPrismDataSource(section);
                return database;
            case "derby":
                Prism.warn("ERROR: This version of Prism no longer supports Derby. Please use MySQL.");
                return null;
            case "sqlite":
                Prism.warn("ERROR: This version of Prism no longer supports SQLite. Please use MySQL.");
                return null;
            default:
                Prism.log("Attempting to configure datasource as " + null);
                return null;
        }

    }
    public static PrismDataSourceUpdater createUpdater(Configuration configuration){
        if(configuration == null) return null;
        String dataSource = configuration.getString("datasource","mysql");
        if(dataSource == null)return null;
        switch (dataSource) {
            case "mysql":
            case "derby":
            case "sqlite":
                return new SQLPrismDataSourceUpdater((MySqlPrismDataSource) database);
            default:
                return null;
        }
    }

    public static Connection getConnection() {
        return database.getConnection();
    }

}
