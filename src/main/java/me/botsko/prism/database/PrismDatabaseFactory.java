package me.botsko.prism.database;

import me.botsko.prism.database.mysql.MySQLPrismDataSource;
import me.botsko.prism.database.mysql.MySQLPrismDataSourceUpdater;
import org.bukkit.configuration.Configuration;
import org.bukkit.configuration.ConfigurationSection;

import java.sql.Connection;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 4/04/2019.
 */
public class PrismDatabaseFactory {

    private static PrismDataSource database = null;

    public static PrismDataSource createDataSource(Configuration  configuration) {
        if(configuration == null) return null;
        String dataSource = configuration.getString("dataSource","mysql");
        if(dataSource == null)return null;
        switch (dataSource) {
            case "mysql":
                database = new MySQLPrismDataSource(configuration.getConfigurationSection("prism.mysql"));
            default:
                return null;
        }
    }
    public static PrismDataSourceUpdater createUpdater(Configuration configuration){
        if(configuration == null) return null;
        String dataSource = configuration.getString("dataSource","mysql");
        if(dataSource == null)return null;
        switch (dataSource) {
            case "mysql":
                return new MySQLPrismDataSourceUpdater((MySQLPrismDataSource) database);
            default:
                return null;
        }
    }

    public static Connection getConnection() {
        return database.getConnection();
    }

}
