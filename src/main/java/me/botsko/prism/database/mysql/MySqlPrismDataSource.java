package me.botsko.prism.database.mysql;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import com.zaxxer.hikari.util.PropertyElf;
import me.botsko.prism.Prism;
import me.botsko.prism.database.SelectQuery;
import me.botsko.prism.database.sql.SQLPrismDataSource;
import me.botsko.prism.database.sql.SQLSelectQueryBuilder;
import org.bukkit.configuration.ConfigurationSection;

import javax.annotation.Nonnull;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Properties;
import java.util.Set;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by Narimm on 5/04/2019.
 */
public class MySqlPrismDataSource extends SQLPrismDataSource {

    private static final File propFile = new File(Prism.getInstance().getDataFolder(),
          "hikari.properties");
    private static HikariConfig dbConfig;

    static {
        if (propFile.exists()) {
            Prism.log("Configuring Hikari from " + propFile.getName());
            Prism.log("This file will not save the jdbcURL, username or password - these are loaded"
                  + " by default from the standard prism configuration file.  If you set these "
                  + "explicitly in the properties file the settings in the standard config will be"
                  + "ignored.");
            dbConfig = new HikariConfig(propFile.getPath());
        } else {
            dbConfig = new HikariConfig();
        }
    }

    private boolean nonStandardSql;

    /**
     * Setup the Datasource.
     *
     * @param section a {@link ConfigurationSection}
     */
    public MySqlPrismDataSource(ConfigurationSection section) {
        super(section);
        nonStandardSql = section.getBoolean("useNonStandardSql", true);
        name = "mysql";
    }

    /**
     * The adds the new requirements to an old configuration file.
     * @param section a {@link ConfigurationSection}
     */
    public static void updateDefaultConfig(ConfigurationSection section) {
        section.addDefault("hostname", "127.0.0.1");
        section.addDefault("username", "root");
        section.addDefault("password", "");
        section.addDefault("databaseName", "minecraft");
        section.addDefault("prefix", "prism_");
        section.addDefault("port", "3306");
        section.addDefault("useNonStandardSql", true);
        setupDefaultProperties(section);
    }

    private static void setupDefaultProperties(@Nonnull ConfigurationSection section) {
        int maxPool = section.getInt("database.max-pool-connections", 10);
        int minIdle = section.getInt("database.min-idle-connections", 10);
        if (!propFile.exists()) {
            dbConfig.addDataSourceProperty("maximumPoolSize", maxPool);
            dbConfig.addDataSourceProperty("minimumIdle", minIdle);
            dbConfig.setMaximumPoolSize(maxPool);
            dbConfig.setMinimumIdle(minIdle);
        }
        if (!propFile.exists()) {
            dbConfig.setPoolName("prism");
            Properties prop = new Properties();
            Set<String> keys = PropertyElf.getPropertyNames(HikariConfig.class);
            for (String k : keys) {
                if ("jbdcUrl".equals(k) || "username".equals(k) || "password".equals(k)
                || "dataSourceProperties".equals(k) || "healthCheckProperties".equals(k)) {
                    continue;
                }
                Object out = PropertyElf.getProperty(k, dbConfig);
                if (out != null) {
                    prop.setProperty(k, out.toString());
                }
            }
            Properties datasourceProps = dbConfig.getDataSourceProperties();
            for (String name : datasourceProps.stringPropertyNames()) {
                String val = datasourceProps.getProperty(name);
                if (val != null) {
                    prop.setProperty("dataSource." + name, val);
                }
            }
            try {
                OutputStream out = new FileOutputStream(propFile);
                prop.store(out, "Prism Hikari Datasource Properties for"
                      + " advanced database Configuration");
                Prism.log("Database Configuration saved to - " + propFile.getPath());
            } catch (IOException e) {
                Prism.log("Could not save Hikari.properties - " + e.getMessage());
            }
        }
    }

    @Override
    public MySqlPrismDataSource createDataSource() {
        if (dbConfig.getJdbcUrl() == null) {
            final String dns = "jdbc:mysql://" + this.section.getString("hostname") + ":"
                    + this.section.getString("port") + "/" + this.section.getString("databaseName")
                    + "?useUnicode=true&characterEncoding=UTF-8&useSSL=false";
            dbConfig.setJdbcUrl(dns);
            dbConfig.setUsername(this.section.getString("username"));
            dbConfig.setPassword(this.section.getString("password"));
        }
        if (Prism.getInstance().monitoring) {
            dbConfig.setMetricRegistry(Prism.monitor.getRegistry());
            dbConfig.addHealthCheckProperty("connectivityCheckTimeoutMs", "1000");
            dbConfig.addHealthCheckProperty("expected99thPercentileMs", "10");
            dbConfig.setHealthCheckRegistry(Prism.monitor.getHealthRegistry());
        }
        database = new HikariDataSource(dbConfig);
        createSettingsQuery();
        return this;
    }

    @Override
    public void setFile() {
        //not required here.
    }

    @Override
    public SelectQuery createSelectQuery() {
        if (nonStandardSql) {
            return new MySQLSelectQueryBuilder(this);
        } else {
            return new SQLSelectQueryBuilder(this);
        }
    }
}
