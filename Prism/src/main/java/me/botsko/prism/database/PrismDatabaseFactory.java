package me.botsko.prism.database;

import me.botsko.prism.Prism;
import me.botsko.prism.database.mysql.MySqlPrismDataSource;
import me.botsko.prism.database.mysql.PrismHikariDataSource;
import me.botsko.prism.database.sql.SqlPrismDataSource;
import me.botsko.prism.database.sql.SqlPrismDataSourceUpdater;
import org.bukkit.configuration.ConfigurationSection;
import org.jetbrains.annotations.Nullable;

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
    public static void createDefaultConfig(final ConfigurationSection configuration) {
        ConfigurationSection dataSourceSection;
        ConfigurationSection  dataSourceProperties;
        if (configuration.isConfigurationSection("datasource")) {
            dataSourceSection = configuration.getConfigurationSection("datasource");
            dataSourceSection.addDefault("type","mysql");
            if (!dataSourceSection.isConfigurationSection("properties")) {
                dataSourceProperties = dataSourceSection.createSection("properties");
            } else {
                dataSourceProperties = dataSourceSection.getConfigurationSection("properties");
            }
        } else {
            String type = configuration.getString("datasource");//gets the old datasource.
            dataSourceSection = configuration.createSection("datasource");
            if (type != null) {
                dataSourceSection.set("type", type);
            } else {
                dataSourceSection.addDefault("type", "mysql");
            }
            dataSourceProperties = dataSourceSection.createSection("properties");
        }
        String dataType = dataSourceSection.getString("type","mysql");
        updateDataSourceProperties(dataType,dataSourceProperties);
        addDatabaseDefaults(configuration);
    }

    private static void updateDataSourceProperties(@Nullable final String type,
                                                   final ConfigurationSection configuration) {
        String test = type;
        if (test  == null) {
            test = "mysql";
        }
        switch (test) {
            case "mysql":
                MySqlPrismDataSource.updateDefaultConfig(configuration);
                break;
            case "hikari":
            default:
                SqlPrismDataSource.updateDefaultConfig(configuration);
        }
    }

    private static void addDatabaseDefaults(ConfigurationSection section) {
        section.addDefault("query.max-failures-before-wait", 5);
        section.addDefault("query.actions-per-insert-batch", 300);
        section.addDefault("query.force-write-queue-on-shutdown", true);
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
        String dataSource;
        ConfigurationSection dataSourceProperties;

        if (configuration.isConfigurationSection("datasource")) {
            ConfigurationSection dataSourceSection = configuration.getConfigurationSection("datasource");
            if (dataSourceSection != null) {  //in case they didnt update the config.
                dataSource = dataSourceSection.getString("type");
                dataSourceProperties = dataSourceSection.getConfigurationSection("properties");
            } else {
                //old config style
                dataSource = configuration.getString("datasource");
                dataSourceProperties = configuration.getConfigurationSection("prism." + dataSource);
            }
        } else {
            //old config style
            dataSource = configuration.getString("datasource");
            dataSourceProperties = configuration.getConfigurationSection("prism." + dataSource);
        }
        if (dataSource == null) {
            return null;
        }
        switch (dataSource) {
            case "mysql":
                Prism.log("Attempting to configure datasource as mysql");
                database = new MySqlPrismDataSource(dataSourceProperties);
                break;
            case "sqlite":
                Prism.warn("ERROR: This version of Prism no longer supports SQLite.");
                break;
            case "derby":
                Prism.warn("ERROR: This version of Prism no longer supports Derby. Please use Hikari.");
            case "hikari":
            default:
                Prism.log("Attempting to configure datasource as " + dataSource);
                Prism.warn("ERROR: This version of Prism no longer supports " + dataSource);
                Prism.log("Attempting to configure datasource as hikari");
                database = new PrismHikariDataSource(dataSourceProperties);
                Prism.log("HIKARI: prism will configure itself using the hikari parameters");
                break;
        }
        return database;
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
        String dataSource = configuration.getString("type", "mysql");
        if (dataSource == null) {
            return null;
        }
        switch (dataSource) {
            case "mysql":
            case "derby":
            case "sqlite":
                return new SqlPrismDataSourceUpdater(database);
            default:
                return null;
        }
    }

    public static Connection getConnection() {
        return database.getConnection();
    }

}
