package me.botsko.prism.database.sql;

import com.zaxxer.hikari.HikariDataSource;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionRegistry;
import me.botsko.prism.database.*;
import org.bukkit.configuration.ConfigurationSection;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.HashMap;


import javax.annotation.Nonnull;
import javax.sql.DataSource;
import java.sql.*;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 8/04/2019.
 */
public abstract class SQLPrismDataSource implements PrismDataSource {

    @Override
    public boolean isPaused() {
        return paused;
    }

    public void setPaused(boolean paused) {
        this.paused = paused;
    }

    private boolean paused; //when set the datasource will not allow insertions;

    protected String name = "unconfigured";
    private Logger log;
    protected static HikariDataSource database = null;
    private SettingsQuery settingsQuery = null;
    protected ConfigurationSection section;

    public SQLPrismDataSource(ConfigurationSection section) {
        log = LoggerFactory.getLogger("Prism");
        this.section = section;
        setPrefix(section.getString("prefix"));
        setFile();
        createDataSource();
    }
    @Nonnull
    public String getName(){
        return name;
    }
    @Override
    public Logger getLog() {
        return log;
    }

    public void setPrefix(String prefix) {
        this.prefix = prefix;
    }

    private String prefix;


    @Override
    public String getPrefix() {
        return prefix;
    }

    @Override
    public Connection getConnection() {
        try {
            if (database != null)
                return database.getConnection();
        } catch (SQLException e) {
            log.info("Could not retreive a connection");
            return null;
        }
        return null;
    }

    @Override
    public void rebuildDataSource() {
// Close pool connections when plugin disables
        if (database != null) {
            try {
                database.getConnection().close();
            } catch (SQLException e) {
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
                return conn != null && !conn.isClosed();
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
        // Attempt to rescue
        try {
            if (attemptToRescueConnection(e)) {
                return;
            }
        } catch (final SQLException ignored) {
        }
        log.error("Database connection error: " + e.getMessage());
        if (e.getMessage().contains("marked as crashed")) {
            final String[] msg = new String[2];
            msg[0] = "If MySQL crashes during write it may corrupt it's indexes.";
            msg[1] = "Try running `CHECK TABLE " + getPrefix() + "data` and then `REPAIR TABLE " + getPrefix() + "data`.";
            Prism.logSection(msg);
        }
        e.printStackTrace();
    }

    public void setupDatabase(ActionRegistry actionRegistry) {
        Connection conn = null;
        Statement st = null;
        try {
            conn = getConnection();
            if (conn == null)
                return;

            // actions
            String query = "CREATE TABLE IF NOT EXISTS `" + getPrefix() + "actions` ("
                    + "`action_id` int(10) unsigned NOT NULL AUTO_INCREMENT," + "`action` varchar(25) NOT NULL,"
                    + "PRIMARY KEY (`action_id`)," + "UNIQUE KEY `action` (`action`)"
                    + ") ENGINE=InnoDB  DEFAULT CHARSET=utf8;";
            st = conn.createStatement();
            st.executeUpdate(query);

            // data
            query = "CREATE TABLE IF NOT EXISTS `" + getPrefix() + "data` ("
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
            resultSet = metadata.getTables(null, null, "" + getPrefix() + "data_extra", null);
            if (!resultSet.next()) {

                // extra data
                query = "CREATE TABLE IF NOT EXISTS `" + getPrefix() + "data_extra` ("
                        + "`extra_id` bigint(20) unsigned NOT NULL AUTO_INCREMENT,"
                        + "`data_id` bigint(20) unsigned NOT NULL," + "`data` text NULL," + "`te_data` text NULL,"
                        + "PRIMARY KEY (`extra_id`)," + "KEY `data_id` (`data_id`)"
                        + ") ENGINE=InnoDB  DEFAULT CHARSET=utf8;";
                st.executeUpdate(query);

                // add extra data delete cascade
                query = "ALTER TABLE `" + getPrefix() + "data_extra` ADD CONSTRAINT `" + getPrefix()
                        + "data_extra_ibfk_1` FOREIGN KEY (`data_id`) REFERENCES `" + getPrefix()
                        + "data` (`id`) ON DELETE CASCADE ON UPDATE NO ACTION;";
                st.executeUpdate(query);
            }

            // meta
            query = "CREATE TABLE IF NOT EXISTS `" + getPrefix() + "meta` ("
                    + "`id` int(10) unsigned NOT NULL AUTO_INCREMENT," + "`k` varchar(25) NOT NULL,"
                    + "`v` varchar(255) NOT NULL," + "PRIMARY KEY (`id`)" + ") ENGINE=InnoDB  DEFAULT CHARSET=utf8;";
            st.executeUpdate(query);

            // players
            query = "CREATE TABLE IF NOT EXISTS `" + getPrefix() + "players` ("
                    + "`player_id` int(10) unsigned NOT NULL AUTO_INCREMENT," + "`player` varchar(255) NOT NULL,"
                    + "`player_uuid` binary(16) NOT NULL," + "PRIMARY KEY (`player_id`),"
                    + "UNIQUE KEY `player` (`player`)," + "UNIQUE KEY `player_uuid` (`player_uuid`)"
                    + ") ENGINE=InnoDB  DEFAULT CHARSET=utf8;";
            st.executeUpdate(query);

            // worlds
            query = "CREATE TABLE IF NOT EXISTS `" + getPrefix() + "worlds` ("
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
            query = "CREATE TABLE IF NOT EXISTS `" + getPrefix() + "id_map` (" + "`material` varchar(63) NOT NULL,"
                    + "`state` varchar(255) NOT NULL," + "`block_id` mediumint(5) NOT NULL AUTO_INCREMENT,"
                    + "`block_subid` mediumint(5) NOT NULL DEFAULT 0," + "PRIMARY KEY (`material`, `state`),"
                    + "UNIQUE KEY (`block_id`, `block_subid`)" + ") ENGINE=InnoDB DEFAULT CHARSET=utf8;";
            st.executeUpdate(query);

            // rollback
            resultSet = metadata.getTables(null, null, getPrefix() + "data_rollback", null);
            if (!resultSet.next()) {
                query = "CREATE TABLE IF NOT EXISTS `" + getPrefix() + "data_rollback` ("
                        + "`rollback_id` bigint(20) UNSIGNED NOT NULL AUTO_INCREMENT,"
                        + "`data_id` bigint(20) UNSIGNED NOT NULL, `rollback` tinyint(1) DEFAULT NULL,"
                        + "PRIMARY KEY (`rollback_id`), UNIQUE KEY `data_id` (`data_id`)"
                        + ") ENGINE=InnoDB DEFAULT CHARSET=utf8;";
                st.executeUpdate(query);

                // rollback foreign key
                query = "ALTER TABLE `" + getPrefix() + "data_rollback` ADD CONSTRAINT `"
                        + getPrefix() + "data_rollback_ibfk_1` FOREIGN KEY (`data_id`) REFERENCES `"
                        + getPrefix() + "data` (`id`) ON DELETE CASCADE ON UPDATE NO ACTION;";
                st.executeUpdate(query);
            }
        } catch (final SQLException e) {
            Prism.log("Database connection error: " + e.getMessage());
            e.printStackTrace();
        } finally {
            if (st != null)
                try {
                    st.close();
                } catch (final SQLException ignored) {
                }
            if (conn != null)
                try {
                    conn.close();
                } catch (final SQLException ignored) {
                }
        }
    }

    public void addActionName(String actionName) {

        if (Prism.prismActions.containsKey(actionName))
            return;

        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = database.getConnection();
            s = conn.prepareStatement("INSERT INTO " + getPrefix() + "actions (action) VALUES (?)",
                    Statement.RETURN_GENERATED_KEYS);
            s.setString(1, actionName);
            s.executeUpdate();

            rs = s.getGeneratedKeys();
            if (rs.next()) {
                log.info("Registering new action type to the database/cache: " + actionName + " " + rs.getInt(1));
                Prism.prismActions.put(actionName, rs.getInt(1));
            } else {
                throw new SQLException("Insert statement failed - no generated key obtained.");
            }
        } catch (final SQLException ignored) {

        } finally {
            if (rs != null)
                try {
                    rs.close();
                } catch (final SQLException ignored) {
                }
            if (s != null)
                try {
                    s.close();
                } catch (final SQLException ignored) {
                }
            if (conn != null)
                try {
                    conn.close();
                } catch (final SQLException ignored) {
                }
        }
    }

    protected void cacheActionPrimaryKeys() {
        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {
            conn = getConnection();
            s = conn.prepareStatement("SELECT action_id, action FROM " + getPrefix() + "actions");
            rs = s.executeQuery();

            while (rs.next()) {
                log.debug("Loaded " + rs.getString(2) + ", id:" + rs.getInt(1));
                Prism.prismActions.put(rs.getString(2), rs.getInt(1));
            }

            Prism.debug("Loaded " + Prism.prismActions.size() + " actions into the cache.");

        } catch (final SQLException e) {
            handleDataSourceException(e);
        } finally {
            if (rs != null)
                try {
                    rs.close();
                } catch (final SQLException ignored) {
                }
            if (s != null)
                try {
                    s.close();
                } catch (final SQLException ignored) {
                }
            if (conn != null)
                try {
                    conn.close();
                } catch (final SQLException ignored) {
                }
        }
    }

    public void cacheWorldPrimaryKeys(HashMap prismWorlds) {

        Connection conn = null;
        PreparedStatement s = null;
        ResultSet rs = null;
        try {

            conn = getConnection();
            s = conn.prepareStatement("SELECT world_id, world FROM " + getPrefix() + "worlds");
            rs = s.executeQuery();

            while (rs.next()) {
                prismWorlds.put(rs.getString(2), rs.getInt(1));
            }
            Prism.debug("Loaded " + prismWorlds.size() + " worlds into the cache.");
        } catch (final SQLException e) {
            handleDataSourceException(e);
        } finally {
            if (rs != null)
                try {
                    rs.close();
                } catch (final SQLException ignored) {
                }
            if (s != null)
                try {
                    s.close();
                } catch (final SQLException ignored) {
                }
            if (conn != null)
                try {
                    conn.close();
                } catch (final SQLException ignored) {
                }
        }
    }

    /**
     * Saves a world name to the database, and adds the id to the cache hashmap
     */
    public void addWorldName(String worldName) {

        if (Prism.prismWorlds.containsKey(worldName))
            return;
        String query = "INSERT INTO `" + getPrefix() + "worlds` (world) VALUES (?)";
        ResultSet rs = null;
        try (
                Connection conn = database.getConnection();
                PreparedStatement s = conn.prepareStatement(query, Statement.RETURN_GENERATED_KEYS)
        ) {
            s.setString(1, worldName);
            s.executeUpdate();
            rs = s.getGeneratedKeys();
            if (rs.next()) {
                log.info("Registering new world to the database/cache: " + worldName + " " + rs.getInt(1));
                Prism.prismWorlds.put(worldName, rs.getInt(1));
            } else {
                throw new SQLException("Insert statement failed - no generated key obtained.");
            }
        } catch (final SQLException ignored) {

        } finally {
            if (rs != null)
                try {
                    rs.close();
                } catch (final SQLException ignored) {
                }
        }
    }

    @Override
    public void dispose() {
        if (database != null)
            database.close();
        database = null;
    }

    @Override
    public SelectQuery createSelectQuery() {
        return new SQLSelectQueryBuilder(this);
    }

    @Override
    public SelectIDQuery createSelectIDQuery() {
        return new SQLSelectIDQueryBuilder(this);
    }

    @Override
    public DeleteQuery createDeleteQuery() {
        return new SQLDeleteQueryBuilder(this);
    }

    @Override
    public BlockReportQuery createBlockReportQuery() {
        return new SQLBlockReportQueryBuilder(this);
    }

    @Override
    public ActionReportQuery createActionReportQuery() {
        return new SQLActionReportQueryBuilder(this);
    }

    @Override
    public SettingsQuery createSettingsQuery() {
        if (settingsQuery == null)
            return new SQLSettingsQuery(this);
        else
            return settingsQuery;
    }

    @Override
    public SelectProcessActionQuery createProcessQuery() {
        return new SQLSelectProcessQuery(this);
    }

    public InsertQuery getDataInsertionQuery() {
        return new SQLInsertBuilder(this);
    }
}
