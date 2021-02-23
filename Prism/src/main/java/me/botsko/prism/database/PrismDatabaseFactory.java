package me.botsko.prism.database;

import me.botsko.prism.PrismLogHandler;
import me.botsko.prism.database.mysql.MySqlPrismDataSource;
import me.botsko.prism.database.sql.SqlPrismDataSource;
import me.botsko.prism.database.sql.derby.DerbySqlPrismDataSource;
import me.botsko.prism.settings.Settings;
import org.bukkit.configuration.ConfigurationSection;
import org.jetbrains.annotations.Nullable;
import org.spongepowered.configurate.ConfigurationNode;
import org.spongepowered.configurate.serialize.SerializationException;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 4/04/2019.
 */
public class PrismDatabaseFactory {

    /**
     * Create a config.
     *
     * @param dataSourceSection ConfigurationSection
     */
    public static void createDefaultConfig(final ConfigurationNode dataSourceSection) throws SerializationException {
        ConfigurationNode dataSourceProperties = dataSourceSection.node("properties");
        String dataType = dataSourceSection.node("type").getString("mysql");
        dataSourceSection.node("type").set(dataType);
        updateDataSourceProperties(dataType, dataSourceProperties);
    }

    private static void updateDataSourceProperties(final String type,
                                                   final ConfigurationNode configuration) {
        try {
            switch (type) {
                case "mysql":
                    MySqlPrismDataSource.updateDefaultConfig(configuration);
                    break;
                case "hikari":
                default:
                    SqlPrismDataSource.updateDefaultConfig(configuration);
            }
        }catch (SerializationException exception) {
            PrismLogHandler.warn(exception.getMessage());
        }
    }


    /**
     * Constuct Data source.
     *
     * @param dataSourceSection ConfigurationSection
     * @return PrismDataSource
     */
    public static PrismDataSource createDataSource(ConfigurationNode dataSourceSection) {
        PrismDataSource database;
        String dataSource = dataSourceSection.node("type").getString("mysql");

        ConfigurationNode dataSourceProperties = dataSourceSection.node("properties");

        switch (dataSource) {
            case "mysql":
                PrismLogHandler.log("Attempting to configure datasource as mysql");
                database = new MySqlPrismDataSource(dataSourceProperties);
                break;
            case "sqlite":
                PrismLogHandler.warn("ERROR: This version of Prism no longer supports SQLite.");
                database = new NullDataSource();
                break;
            case "hikari":
            case "derby":
                database = new DerbySqlPrismDataSource(dataSourceProperties);
                PrismLogHandler.log("Attempting to configure datasource as " + dataSource);
                PrismLogHandler.log("HIKARI: prism will configure itself using the hikari parameters");
                break;
            default:
                PrismLogHandler.warn("ERROR: This version of Prism no longer supports " + dataSource);
                PrismLogHandler.log("Attempting to configure datasource as hikari using derby");
                database = new DerbySqlPrismDataSource(dataSourceProperties);
                PrismLogHandler.log("HIKARI: prism will configure itself using the hikari parameters");
                break;
        }
        database.createDataSource();
        Settings.setDataSource(database);
        return database;
    }
}
