package me.botsko.prism.database;

import me.botsko.prism.Prism;
import me.botsko.prism.database.SQL.SQLPrismDataSource;
import me.botsko.prism.database.derby.DerbyPrismDataSource;
import me.botsko.prism.database.mysql.MySQLPrismDataSource;
import me.botsko.prism.database.SQL.SQLPrismDataSourceUpdater;
import me.botsko.prism.database.sqlite.SQLitePrismDataSource;
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
        ConfigurationSection mysql = configuration.createSection("prism.mysql");
        MySQLPrismDataSource.updateDefaultConfig(mysql);
        addTomcatJDBCDefaults(mysql);
        ConfigurationSection derby = configuration.createSection("prism.derby");
        DerbyPrismDataSource.updateDefaultConfig(derby);
        addTomcatJDBCDefaults(derby);
        ConfigurationSection sqlite = configuration.createSection("prism.sqlite");
        SQLitePrismDataSource.updateDefaultConfig(sqlite);
        addTomcatJDBCDefaults(sqlite);
    }

    private static void addTomcatJDBCDefaults(ConfigurationSection section) {
        section.addDefault("database.max-pool-connections", 20);
        section.addDefault("database.pool-initial-size", 10);
        section.addDefault("database.max-idle-connections", 10);
        section.addDefault("database.max-wait", 30000);
        section.addDefault("database.max-failures-before-wait", 5);
        section.addDefault("database.actions-per-insert-batch", 300);
        // queue
        section.addDefault("database.force-write-queue-on-shutdown", true);
    }
    public static PrismDataSource createDataSource(Configuration  configuration) {
        if(configuration == null) return null;
        String dataSource = configuration.getString("dataSource","mysql");
        if(dataSource == null)return null;
        switch (dataSource) {
            case "mysql":
                Prism.log("Attempting to configure datasource as " + dataSource);
                database = new MySQLPrismDataSource(configuration.getConfigurationSection("prism.mysql"));
                return database;
            case "derby":
                Prism.log("Attempting to configure datasource as " + dataSource);
                database = new DerbyPrismDataSource(configuration.getConfigurationSection("prism.derby"));
                return database;
            case "sqlite":
                Prism.log("Attempting to configure datasource as " + dataSource);
                database = new SQLitePrismDataSource(configuration.getConfigurationSection("prism.sqlite"));
                return database;
            default:
                Prism.log("Attempting to configure datasource as " + null);
                return null;
        }
    }
    public static PrismDataSourceUpdater createUpdater(Configuration configuration){
        if(configuration == null) return null;
        String dataSource = configuration.getString("dataSource","mysql");
        if(dataSource == null)return null;
        switch (dataSource) {
            case "mysql":
                return new SQLPrismDataSourceUpdater((MySQLPrismDataSource) database);
            default:
                return null;
        }
    }

    public static Connection getConnection() {
        return database.getConnection();
    }

}
