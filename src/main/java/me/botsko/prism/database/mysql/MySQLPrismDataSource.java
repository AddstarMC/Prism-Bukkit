package me.botsko.prism.database.mysql;

import com.sun.media.jfxmedia.logging.Logger;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionRegistry;
import me.botsko.prism.database.*;
import org.bukkit.configuration.ConfigurationSection;

import javax.sql.DataSource;
import java.sql.*;
import java.util.HashMap;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 5/04/2019.
 */
public class MySQLPrismDataSource implements PrismDataSource {

    private static org.apache.tomcat.jdbc.pool.DataSource database = null;
    private static ConfigurationSection section;

    public String getPrefix() {
        return prefix;
    }

    private String prefix;

    public MySQLPrismDataSource(ConfigurationSection section) {
        this.section = section;
        prefix = section.getString("prefix");
        createDataSource();
    }


    @Override
    public MySQLPrismDataSource createDataSource() {
        org.apache.tomcat.jdbc.pool.DataSource pool = null;
        this.section = section;
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
        return this;
    }

    @Override
    public Connection getConnection() {
        try {
            if(database !=null)
                return database.getConnection();
        }catch (SQLException e){
            Logger.logMsg(3,"Could not retreive a connection");
            return null;
        }
        return null;
    }

    @Override
    public void rebuildDataSource(){
// Close pool connections when plugin disables
        if (database != null) {
            try {
                database.getConnection().close();
            }catch (SQLException e){
                handleDataSourceException(e);
            }
            database = null;
        }
        createDataSource();
    }

    protected boolean attemptToRescueConnection(SQLException e) throws SQLException {
        if (e.getMessage().contains("connection closed")) {
            rebuildDataSource();
            if (database != null) {
                final Connection conn = createDataSource().getConnection();
                if (conn != null && !conn.isClosed()) {
                    return true;
                }
            }
        }
        return false;
    }
    @Override
    public DataSource getDataSource() {
        return database;
    }

    @Override
    public void handleDataSourceException(SQLException e) {
        String prefix = section.getString("prism.mysql.prefix");
        // Attempt to rescue
        try {
            if (attemptToRescueConnection(e)) {
                return;
            }
        }
        catch (final SQLException e1) {
        }
        Prism.log("Database connection error: " + e.getMessage());
        if (e.getMessage().contains("marked as crashed")) {
            final String[] msg = new String[2];
            msg[0] = "If MySQL crashes during write it may corrupt it's indexes.";
            msg[1] = "Try running `CHECK TABLE " + prefix + "data` and then `REPAIR TABLE " + prefix + "data`.";
            Prism.logSection(msg);
        }
        e.printStackTrace();
    }

    public void setupDatabase(ActionRegistry actionRegistry) {
        String prefix = section.getString("prefix");
        Connection conn = null;
        Statement st = null;
        try {
            conn = getConnection();
            if (conn == null)
                return;

            // actions
            String query = "CREATE TABLE IF NOT EXISTS `" + prefix + "actions` ("
                    + "`action_id` int(10) unsigned NOT NULL AUTO_INCREMENT," + "`action` varchar(25) NOT NULL,"
                    + "PRIMARY KEY (`action_id`)," + "UNIQUE KEY `action` (`action`)"
                    + ") ENGINE=InnoDB  DEFAULT CHARSET=utf8;";
            st = conn.createStatement();
            st.executeUpdate(query);

            // data
            query = "CREATE TABLE IF NOT EXISTS `" + prefix + "data` ("
                    + "`id` bigint(20) unsigned NOT NULL AUTO_INCREMENT," + "`epoch` int(10) unsigned NOT NULL,"
                    + "`action_id` int(10) unsigned NOT NULL," + "`player_id` int(10) unsigned NOT NULL,"
                    + "`world_id` int(10) unsigned NOT NULL," + "`x` int(11) NOT NULL," + "`y` int(11) NOT NULL,"
                    + "`z` int(11) NOT NULL," + "`block_id` mediumint(5) DEFAULT NULL,"
                    + "`block_subid` mediumint(5) DEFAULT NULL," + "`old_block_id` mediumint(5) DEFAULT NULL,"
                    + "`old_block_subid` mediumint(5) DEFAULT NULL," + "PRIMARY KEY (`id`)," + "KEY `epoch` (`epoch`),"
                    + "KEY  `location` (`world_id`, `x`, `z`, `y`, `action_id`)"
                    + ") ENGINE=InnoDB  DEFAULT CHARSET=utf8;";
            st.executeUpdate(query);

            // extra prism data table (check if it exists first, so we can avoid
            // re-adding foreign key stuff)
            final DatabaseMetaData metadata = conn.getMetaData();
            ResultSet resultSet;
            resultSet = metadata.getTables(null, null, "" + prefix + "data_extra", null);
            if (!resultSet.next()) {

                // extra data
                query = "CREATE TABLE IF NOT EXISTS `" + prefix + "data_extra` ("
                        + "`extra_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,"
                        + "`data_id` bigint(20) unsigned NOT NULL," + "`data` text NULL," + "`te_data` text NULL,"
                        + "PRIMARY KEY (`extra_id`)," + "KEY `data_id` (`data_id`)"
                        + ") ENGINE=InnoDB  DEFAULT CHARSET=utf8;";
                st.executeUpdate(query);

                // add extra data delete cascade
                query = "ALTER TABLE `" + prefix + "data_extra` ADD CONSTRAINT `" + prefix
                        + "data_extra_ibfk_1` FOREIGN KEY (`data_id`) REFERENCES `" + prefix
                        + "data` (`id`) ON DELETE CASCADE ON UPDATE NO ACTION;";
                st.executeUpdate(query);
            }

            // meta
            query = "CREATE TABLE IF NOT EXISTS `" + prefix + "meta` ("
                    + "`id` int(10) unsigned NOT NULL AUTO_INCREMENT," + "`k` varchar(25) NOT NULL,"
                    + "`v` varchar(255) NOT NULL," + "PRIMARY KEY (`id`)" + ") ENGINE=InnoDB  DEFAULT CHARSET=utf8;";
            st.executeUpdate(query);

            // players
            query = "CREATE TABLE IF NOT EXISTS `" + prefix + "players` ("
                    + "`player_id` int(10) unsigned NOT NULL AUTO_INCREMENT," + "`player` varchar(255) NOT NULL,"
                    + "`player_uuid` binary(16) NOT NULL," + "PRIMARY KEY (`player_id`),"
                    + "UNIQUE KEY `player` (`player`)," + "UNIQUE KEY `player_uuid` (`player_uuid`)"
                    + ") ENGINE=InnoDB  DEFAULT CHARSET=utf8;";
            st.executeUpdate(query);

            // worlds
            query = "CREATE TABLE IF NOT EXISTS `" + prefix + "worlds` ("
                    + "`world_id` int(10) unsigned NOT NULL AUTO_INCREMENT," + "`world` varchar(255) NOT NULL,"
                    + "PRIMARY KEY (`world_id`)," + "UNIQUE KEY `world` (`world`)"
                    + ") ENGINE=InnoDB  DEFAULT CHARSET=utf8;";
            st.executeUpdate(query);

            // actions
            cacheActionPrimaryKeys(); // Pre-cache, so we know if we need to
            // populate db
            final String[] actions = actionRegistry.listAll();
            for (final String a : actions) {
                addActionName(a);
            }

            // id map
            query = "CREATE TABLE IF NOT EXISTS `" + prefix + "id_map` (" + "`material` varchar(63) NOT NULL,"
                    + "`state` varchar(255) NOT NULL," + "`block_id` mediumint(5) NOT NULL AUTO_INCREMENT,"
                    + "`block_subid` mediumint(5) NOT NULL DEFAULT 0," + "PRIMARY KEY (`material`, `state`),"
                    + "UNIQUE KEY (`block_id`, `block_subid`)" + ") ENGINE=InnoDB DEFAULT CHARSET=utf8;";
            st.executeUpdate(query);
        }
        catch (final SQLException e) {
            Prism.log("Database connection error: " + e.getMessage());
            e.printStackTrace();
        }
        finally {
            if (st != null)
                try {
                    st.close();
                }
                catch (final SQLException e) {
                }
            if (conn != null)
                try {
                    conn.close();
                }
                catch (final SQLException e) {
                }
        }
    }

    public void addActionName(String actionName) {
        String prefix = section.getString("prefix");

        if (Prism.prismActions.containsKey(actionName))
            return;

        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = database.getConnection();
            s = conn.prepareStatement("INSERT INTO " + prefix + "actions (action) VALUES (?)",
                    Statement.RETURN_GENERATED_KEYS);
            s.setString(1, actionName);
            s.executeUpdate();

            rs = s.getGeneratedKeys();
            if (rs.next()) {
                Prism.log("Registering new action type to the database/cache: " + actionName + " " + rs.getInt(1));
                Prism.prismActions.put(actionName, rs.getInt(1));
            }
            else {
                throw new SQLException("Insert statement failed - no generated key obtained.");
            }
        }
        catch (final SQLException e) {

        }
        finally {
            if (rs != null)
                try {
                    rs.close();
                }
                catch (final SQLException e) {
                }
            if (s != null)
                try {
                    s.close();
                }
                catch (final SQLException e) {
                }
            if (conn != null)
                try {
                    conn.close();
                }
                catch (final SQLException e) {
                }
        }
    }

    protected void cacheActionPrimaryKeys() {
        String prefix = section.getString("prefix");
        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {
            conn = getConnection();
            s = conn.prepareStatement("SELECT action_id, action FROM " + prefix + "actions");
            rs = s.executeQuery();

            while (rs.next()) {
                Prism.debug("Loaded " + rs.getString(2) + ", id:" + rs.getInt(1));
                Prism.prismActions.put(rs.getString(2), rs.getInt(1));
            }

            Prism.debug("Loaded " + Prism.prismActions.size() + " actions into the cache.");

        }
        catch (final SQLException e) {
            handleDataSourceException(e);
        }
        finally {
            if (rs != null)
                try {
                    rs.close();
                }
                catch (final SQLException e) {
                }
            if (s != null)
                try {
                    s.close();
                }
                catch (final SQLException e) {
                }
            if (conn != null)
                try {
                    conn.close();
                }
                catch (final SQLException e) {
                }
        }
    }

    public void cacheWorldPrimaryKeys(HashMap prismWorlds) {
        String prefix = section.getString("prism.mysql.prefix");

        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = getConnection();
            s = conn.prepareStatement("SELECT world_id, world FROM " + prefix + "worlds");
            rs = s.executeQuery();

            while (rs.next()) {
                prismWorlds.put(rs.getString(2), rs.getInt(1));
            }
            Prism.debug("Loaded " + prismWorlds.size() + " worlds into the cache.");
        }
        catch (final SQLException e) {
            handleDataSourceException(e);
        }
        finally {
            if (rs != null)
                try {
                    rs.close();
                }
                catch (final SQLException e) {
                }
            if (s != null)
                try {
                    s.close();
                }
                catch (final SQLException e) {
                }
            if (conn != null)
                try {
                    conn.close();
                }
                catch (final SQLException e) {
                }
        }
    }
    /**
     * Saves a world name to the database, and adds the id to the cache hashmap
     */
    public void addWorldName(String worldName) {
        String prefix = section.getString("prefix");

        if (Prism.prismWorlds.containsKey(worldName))
            return;

        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = database.getConnection();
            s = conn.prepareStatement("INSERT INTO " + prefix + "worlds (world) VALUES (?)",
                    Statement.RETURN_GENERATED_KEYS);
            s.setString(1, worldName);
            s.executeUpdate();

            rs = s.getGeneratedKeys();
            if (rs.next()) {
                Prism.log("Registering new world to the database/cache: " + worldName + " " + rs.getInt(1));
                Prism.prismWorlds.put(worldName, rs.getInt(1));
            }
            else {
                throw new SQLException("Insert statement failed - no generated key obtained.");
            }
        }
        catch (final SQLException e) {

        }
        finally {
            if (rs != null)
                try {
                    rs.close();
                }
                catch (final SQLException e) {
                }
            if (s != null)
                try {
                    s.close();
                }
                catch (final SQLException e) {
                }
            if (conn != null)
                try {
                    conn.close();
                }
                catch (final SQLException e) {
                }
        }
    }

    @Override
    public void dispose() {
        database.close();
        database = null;
    }

    @Override
    public SelectQuery createSelectQuery(Prism plugin) {
        return new MySQLSelectQueryBuilder(plugin);
    }

    @Override
    public SelectIDQuery createSelectIDQuery(Prism plugin) {
        return new MysqlSelectIDQueryBuilder(plugin);
    }

    @Override
    public DeleteQuery createDeleteQuery(Prism plugin) {
        return new MySQLDeleteQueryBuilder(plugin);
    }

    @Override
    public BlockReportQuery createBlockReportQuery(Prism plugin) {
        return new MySQLBlockReportQueryBuilder(plugin);
    }

    @Override
    public ActionReportQuery createActionReportQuery(Prism plugin) {
        return new MySQLActionReportQueryBuilder(plugin);
    }
}
