package me.botsko.prism.database.derby;

import me.botsko.prism.database.SQL.SQLPrismDataSource;
import me.botsko.prism.database.mysql.MySQLPrismDataSource;
import org.bukkit.Bukkit;
import org.bukkit.configuration.ConfigurationSection;

import java.io.File;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 8/04/2019.
 */
public class DerbyPrismDataSource extends SQLPrismDataSource {

    private File derby;

    public DerbyPrismDataSource(ConfigurationSection section) {
        super(section);
    }

    public void setFile() {
        String path = Bukkit.getServer().getPluginManager().getPlugin("Prism").getDataFolder().getAbsolutePath();
        String fileName = this.section.getString("filePath", "prism");
        derby = new File(path, fileName);
    }

    @Override
    public DerbyPrismDataSource createDataSource() {
        org.apache.tomcat.jdbc.pool.DataSource pool;
        final String dns = "jdbc:derby:" + derby;
        pool = new org.apache.tomcat.jdbc.pool.DataSource();
        pool.setDriverClassName("org.apache.derby.jdbc.EmbeddedDriver");
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

