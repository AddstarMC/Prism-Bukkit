package me.botsko.prism.database.mysql;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import me.botsko.prism.database.sql.*;
import me.botsko.prism.database.SelectQuery;
import org.bukkit.configuration.ConfigurationSection;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 5/04/2019.
 */
public class MySQLPrismDataSource extends SQLPrismDataSource {

    private boolean nonStandardSQL;
    private static HikariConfig dbconfig = new HikariConfig();

    public MySQLPrismDataSource(ConfigurationSection section) {
        super(section);
        nonStandardSQL = this.section.getBoolean("useNonStandardSql", true);
        name = "mysql";
    }

    public static void updateDefaultConfig(ConfigurationSection section) {
        section.addDefault("hostname", "127.0.0.1");
        section.addDefault("username", "root");
        section.addDefault("password", "");
        section.addDefault("databaseName", "minecraft");
        section.addDefault("prefix", "prism_");
        section.addDefault("port", "3306");
        section.addDefault("useNonStandardSql", true);
    }
    @Override
    public MySQLPrismDataSource createDataSource() {
        final String dns = "jdbc:mysql://" + this.section.getString("hostname") + ":"
                + this.section.getString("port") + "/" + this.section.getString("databaseName")
                + "?useUnicode=true&characterEncoding=UTF-8&useSSL=false";

        dbconfig.setPoolName("prism");
        dbconfig.setMaximumPoolSize(this.section.getInt("database.max-pool-connections"));
        dbconfig.setMinimumIdle(this.section.getInt("database.min-idle-connections"));
        dbconfig.setJdbcUrl(dns);
        dbconfig.setUsername(this.section.getString("username"));
        dbconfig.setPassword(this.section.getString("password"));
        database = new HikariDataSource(dbconfig);
        createSettingsQuery();
        return this;
    }

    @Override
    public void setFile() {
        //not required here.
    }

    @Override
    public SelectQuery createSelectQuery() {
        if (nonStandardSQL) {
            return new MySQLSelectQueryBuilder(this);
        } else {
            return new SQLSelectQueryBuilder(this);
        }
    }
}
