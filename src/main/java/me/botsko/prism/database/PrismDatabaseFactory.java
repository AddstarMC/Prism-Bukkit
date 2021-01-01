package me.botsko.prism.database;

import me.botsko.prism.Prism;
import me.botsko.prism.database.mysql.MySqlPrismDataSource;
import me.botsko.prism.database.mysql.PrismHikariDataSource;
import me.botsko.prism.database.sql.SqlPrismDataSourceUpdater;
import org.bukkit.configuration.ConfigurationSection;

import java.sql.Connection;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 4/04/2019.
 */
public class PrismDatabaseFactory {

    private static PrismDataSource database = null;

    /**
     * Create a config.
     * @param configuration ConfigurationSection
     */
    public static void createDefaultConfig(ConfigurationSection configuration) {
        ConfigurationSection mysql;
        if (configuration.contains("prism.mysql")) {
            mysql = configuration.getConfigurationSection("prism.mysql");
            if (mysql == null) {
                mysql = configuration.createSection("prism.mysql");
            }
        } else {
            mysql = configuration.createSection("prism.mysql");
        }
        MySqlPrismDataSource.updateDefaultConfig(mysql);
        addDatabaseDefaults(mysql);
    }

    private static void addDatabaseDefaults(ConfigurationSection section) {
        section.addDefault("database.max-failures-before-wait", 5);
        section.addDefault("database.actions-per-insert-batch", 300);
        section.addDefault("database.force-write-queue-on-shutdown", true);
    }

    /**
     * Constuct Data source.
     * @param configuration ConfigurationSection
     * @return PrismDataSource
     */
    public static PrismDataSource createDataSource(ConfigurationSection configuration) {
        if (configuration == null) {
            return null;
        }
        String dataSource = configuration.getString("datasource");
        if (dataSource == null) {
            return null;
        }
        ConfigurationSection section;
        switch (dataSource) {
            case "mysql":
                Prism.log("Attempting to configure datasource as " + dataSource);
                section = configuration.getConfigurationSection("prism.mysql");
                database = new MySqlPrismDataSource(section);
                return database;
            case "derby":
                Prism.warn("ERROR: This version of Prism no longer supports Derby. Please use MySQL.");
                return null;
            case "sqlite":
                Prism.warn("ERROR: This version of Prism no longer supports SQLite. Please use MySQL.");
                return null;
            case "hikari":
                Prism.log("Attempting to configure datasource as hikari");
                section = configuration.getConfigurationSection("prism.datasource");
                database = new PrismHikariDataSource(null);
                Prism.log("HIKARI: prism will configure itself using the hikari parameters");
            default:
                Prism.log("Attempting to configure datasource as null");
                return null;
        }

    }

    /**
     * Create updater for datasource.
     * @param configuration ConfigurationSection
     * @return PrismDataSourceUpdater
     */
    public static PrismDataSourceUpdater createUpdater(ConfigurationSection configuration) {
        if (configuration == null) {
            return null;
        }
        String dataSource = configuration.getString("datasource", "mysql");
        if (dataSource == null) {
            return null;
        }
        switch (dataSource) {
            case "mysql":
            case "derby":
            case "sqlite":
                return new SqlPrismDataSourceUpdater((MySqlPrismDataSource) database);
            default:
                return null;
        }
    }

    public static Connection getConnection() {
        return database.getConnection();
    }

}
