package me.botsko.prism.database.sqlite;

import me.botsko.prism.database.sql.SQLPrismDataSource;
import org.bukkit.Bukkit;
import org.bukkit.configuration.ConfigurationSection;

import java.io.File;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 8/04/2019.
 */
public class SQLitePrismDataSource extends SQLPrismDataSource {

    private File sqLiteFile;

    public SQLitePrismDataSource(ConfigurationSection section) {
        super(section);
        name = "sqlite";
    }

    public void setFile() {
        File dataFolder = Bukkit.getServer().getPluginManager().getPlugin("Prism").getDataFolder();
        String fileName = this.section.getString("filePath", "prism.db");
        sqLiteFile = new File(dataFolder, fileName);
    }

    public static void updateDefaultConfig(ConfigurationSection section) {
        section.addDefault("username", "root");
        section.addDefault("password", "");
        section.addDefault("filePath", "prism.db");
    }

    @Override
    public SQLitePrismDataSource createDataSource() {
        org.apache.tomcat.jdbc.pool.DataSource pool;
        final String dns = "jdbc:sqlite:" + sqLiteFile;
        pool = new org.apache.tomcat.jdbc.pool.DataSource();
        pool.setDriverClassName("org.sqlite.JDBC");
        pool.setUrl(dns);
        pool.setUsername(this.section.getString("username"));
        pool.setPassword(this.section.getString("password"));
        pool.setInitialSize(this.section.getInt("database.pool-initial-size"));
        pool.setMaxActive(this.section.getInt("database.max-pool-connections"));
        pool.setMaxIdle(this.section.getInt("database.max-idle-connections"));
        pool.setMaxWait(this.section.getInt("database.max-wait"));
        database = pool;
        return this;
    }
}
