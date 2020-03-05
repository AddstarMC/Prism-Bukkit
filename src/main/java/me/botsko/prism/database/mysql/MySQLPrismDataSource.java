package me.botsko.prism.database.mysql;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import me.botsko.prism.Prism;
import me.botsko.prism.database.SelectQuery;
import me.botsko.prism.database.sql.SQLPrismDataSource;
import me.botsko.prism.database.sql.SQLSelectQueryBuilder;
import org.bukkit.configuration.ConfigurationSection;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashMap;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 5/04/2019.
 */
public class MySQLPrismDataSource extends SQLPrismDataSource {

    private Boolean nonStandardSql;
    public static final HashMap<String, String> dbInfo = new HashMap<>();
    private static HikariConfig dbConfig = new HikariConfig();

    /**
     * Create a dataSource.
     *
     * @param section Config
     */
    public MySQLPrismDataSource(ConfigurationSection section) {
        super(section);
        nonStandardSql = this.section.getBoolean("useNonStandardSql", false);
        detectNonStandardSql();
        name = "mysql";
    }

    /**
     * Add default params to new Config.
     * //todo Version the config so we can remove the ones we dont need.
     *
     * @param section Config
     */
    public static void updateDefaultConfig(ConfigurationSection section) {
        section.addDefault("hostname", "127.0.0.1");
        section.addDefault("username", "root");
        section.addDefault("password", "");
        section.addDefault("databaseName", "minecraft");
        section.addDefault("prefix", "prism_");
        section.addDefault("port", "3306");
    }

    @Override
    public MySQLPrismDataSource createDataSource() {
        final String dns = "jdbc:mysql://" + this.section.getString("hostname") + ":"
                + this.section.getString("port") + "/" + this.section.getString("databaseName")
                + "?useUnicode=true&characterEncoding=UTF-8&useSSL=false";

        dbConfig.setPoolName("prism");
        dbConfig.setMaximumPoolSize(this.section.getInt("database.max-pool-connections"));
        dbConfig.setMinimumIdle(this.section.getInt("database.min-idle-connections"));
        dbConfig.setJdbcUrl(dns);
        dbConfig.setUsername(this.section.getString("username"));
        dbConfig.setPassword(this.section.getString("password"));
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

    private void detectNonStandardSql() {
        try (
                PreparedStatement st = getConnection().prepareStatement("SHOW VARIABLES");
                ResultSet rs = st.executeQuery()) {
            while (rs.next()) {
                dbInfo.put(rs.getString(1).toLowerCase(), rs.getString(2));
            }
        } catch (SQLException e) {
            Prism.debug(e.getMessage());
        }
        boolean anyValueSuccess;
        try (
                PreparedStatement st = getConnection().prepareStatement("SELECT ANY_VALUE(1)");
                ResultSet rs = st.executeQuery()) {
            nonStandardSql = true;
            anyValueSuccess = true;
            rs.next();
        } catch (SQLException e) {
            anyValueSuccess = false;
        }
        String version = dbInfo.get("version");
        String versionComment = dbInfo.get("version_comment");
        Prism.log("Prism detected you database is " + version + " / " + versionComment);
        Prism.log("You have set nonStandardSql to " + nonStandardSql);
        if (!anyValueSuccess && nonStandardSql) {
                Prism.log("This sounds like a configuration error.  If you have database access"
                        + "errors please set nonStandardSql to false");
                return;
        }
        if (!nonStandardSql) {
            Prism.log("Prism will use standard sql queries");
        }
    }
}
