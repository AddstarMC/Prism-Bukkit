package me.botsko.prism.database.mysql;

import me.botsko.prism.database.SQL.*;
import me.botsko.prism.database.SelectQuery;
import org.bukkit.configuration.ConfigurationSection;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 5/04/2019.
 */
public class MySQLPrismDataSource extends SQLPrismDataSource {

    private boolean nonStandardSQL;

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
        org.apache.tomcat.jdbc.pool.DataSource pool = null;
        final String dns = "jdbc:mysql://" + this.section.getString("hostname") + ":"
                + this.section.getString("port") + "/" + this.section.getString("databaseName")
                + "?useUnicode=true&characterEncoding=UTF-8&useSSL=false";
        pool = new org.apache.tomcat.jdbc.pool.DataSource();
        pool.setDriverClassName("com.mysql.jdbc.Driver");
        pool.setUrl(dns);
        pool.setUsername(this.section.getString("username"));
        pool.setPassword(this.section.getString("password"));
        pool.setInitialSize(this.section.getInt("database.pool-initial-size"));
        pool.setMaxActive(this.section.getInt("database.max-pool-connections"));
        pool.setMaxIdle(this.section.getInt("database.max-idle-connections"));
        pool.setMaxWait(this.section.getInt("database.max-wait"));
        pool.setRemoveAbandoned(true);
        pool.setRemoveAbandonedTimeout(60);
        pool.setTestOnBorrow(true);
        pool.setValidationQuery("/* ping */SELECT 1");
        pool.setValidationInterval(30000);
        database = pool;
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
