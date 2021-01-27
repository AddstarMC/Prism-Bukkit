package me.botsko.prism.database.sql;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.hikari.pool.HikariPool;
import me.botsko.prism.Prism;
import me.botsko.prism.PrismLogHandler;
import me.botsko.prism.database.PrismDataSource;
import org.bukkit.configuration.ConfigurationSection;

import java.io.File;



/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 1/01/2021.
 */
public abstract class PrismHikariDataSource extends SqlPrismDataSource {

    private static final File propFile = new File(Prism.getInstance().getDataFolder(),
            "hikari.properties");
    protected static final HikariConfig dbConfig;

    static {
        if (propFile.exists()) {
            PrismLogHandler.log("Configuring Hikari from " + propFile.getName());
            dbConfig = new HikariConfig(propFile.getPath());
        } else {
            PrismLogHandler.log("You may need to adjust these settings for your setup.");
            PrismLogHandler.log("To set a table prefix you will need to create a config entry under");
            PrismLogHandler.log("prism:");
            PrismLogHandler.log("  datasource:");
            PrismLogHandler.log("    prefix: your-prefix");
            String jdbcUrl = "jdbc:mysql://localhost:3306/prism?useUnicode=true&characterEncoding=UTF-8&useSSL=false";
            PrismLogHandler.log("Default jdbcUrl: " + jdbcUrl);
            PrismLogHandler.log("Default Username: username");
            PrismLogHandler.log("Default Password: password");
            PrismLogHandler.log("You will need to provide the required jar libraries that support your database.");
            dbConfig = new HikariConfig();
            dbConfig.setJdbcUrl(jdbcUrl);
            dbConfig.setUsername("username");
            dbConfig.setPassword("password");
            HikariHelper.createPropertiesFile(propFile, dbConfig, false);
        }
    }

    /**
     * Create a dataSource.
     *
     * @param section Config
     */
    public PrismHikariDataSource(ConfigurationSection section) {
        super(section);
        name = "hikari";
    }

    @Override
    public PrismDataSource createDataSource() {
        try {
            database = new HikariDataSource(dbConfig);
            createSettingsQuery();
            return this;
        } catch (HikariPool.PoolInitializationException e) {
            PrismLogHandler.warn("Hikari Pool did not Initialize: " + e.getMessage());
            database = null;
        }
        return this;

    }

    @Override
    public void setFile() {

    }
}
