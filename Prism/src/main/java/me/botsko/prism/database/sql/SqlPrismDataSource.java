package me.botsko.prism.database.sql;

import com.zaxxer.hikari.HikariDataSource;
import me.botsko.prism.Il8nHelper;
import me.botsko.prism.Prism;
import me.botsko.prism.PrismLogHandler;
import me.botsko.prism.actionlibs.ActionRegistry;
import me.botsko.prism.api.actions.ActionType;
import me.botsko.prism.database.ActionReportQuery;
import me.botsko.prism.database.BlockReportQuery;
import me.botsko.prism.database.DeleteQuery;
import me.botsko.prism.database.IdMapQuery;
import me.botsko.prism.database.InsertQuery;
import me.botsko.prism.database.PlayerIdentificationQuery;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.database.PrismSqlConfig;
import me.botsko.prism.database.SelectIdQuery;
import me.botsko.prism.database.SelectProcessActionQuery;
import me.botsko.prism.database.SelectQuery;
import me.botsko.prism.database.SettingsQuery;
import org.spongepowered.configurate.ConfigurationNode;
import javax.annotation.Nonnull;
import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLDataException;
import java.sql.SQLException;
import java.sql.SQLIntegrityConstraintViolationException;
import java.sql.Statement;
import java.util.Map;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 8/04/2019.
 */
@SuppressWarnings("SqlResolve")
public abstract class SqlPrismDataSource<T extends PrismSqlConfig> implements PrismDataSource<T> {

    protected static HikariDataSource database = null;
    protected String name = "unconfigured";
    protected final  ConfigurationNode dataSourceConfig;
    private boolean paused; //when set the datasource will not allow insertions;
    private SettingsQuery settingsQuery = null;
    protected String prefix = "prism_";
    protected PlayerIdentificationQuery playerIdHelper;
    protected IdMapQuery idMapQuery;


    /**
     * Constructor.
     * @param node Config
     */
    public SqlPrismDataSource(ConfigurationNode node) {
        this.dataSourceConfig = node;
        setConfig();
    }

    protected abstract void setConfig();

    @Override
    public boolean isPaused() {
        return paused;
    }

    public void setPaused(boolean paused) {
        this.paused = paused;
    }

    @Nonnull
    public String getName() {
        return name;
    }

    @Override
    public String getPrefix() {
        return prefix;
    }

    /**
     * Set the prefix for the data source.
     * @param prefix String.
     */
    public void setPrefix(String prefix) {
        if (prefix == null) {
            this.prefix = "";
        }
        this.prefix = prefix;
    }

    @Override
    public Connection getConnection() {
        try {
            if (database != null) {
                return database.getConnection();
            }
        } catch (SQLException e) {
            PrismLogHandler.log("Could not retrieve a connection - with exception");
            return null;
        }
        PrismLogHandler.log("Could not retrieve a connection");
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
        PrismLogHandler.warn("Database connection error: " + e.getMessage());
        try {
            if (attemptToRescueConnection(e)) {
                return;
            }
        } catch (final SQLException ignored) {
            PrismLogHandler.warn("Database rescue was unsuccessful.");
        }
        if (e.getMessage().contains("marked as crashed")) {
            final String[] msg = new String[2];
            msg[0] = "If MySQL crashes during write it may corrupt it's indexes.";
            msg[1] = "Try running `CHECK TABLE " + getPrefix() + "data` and then `REPAIR TABLE "
                    + getPrefix() + "data`.";
            PrismLogHandler.logSection(msg);
        }
        e.printStackTrace();
    }

    /**
     * Add action to db.
     * @param action String
     */
    public void addActionName(ActionType action) {

        if (ActionRegistry.prismActions.containsKey(action)) {
            return;
        }
        PrismLogHandler.log(action.name + " not found in cache - inserting.");
        try (
                Connection conn = database.getConnection();
                PreparedStatement s = conn.prepareStatement("INSERT INTO " + prefix + "actions (action) VALUES (?)",
                        Statement.RETURN_GENERATED_KEYS)
        ) {
            s.setString(1, action.name);
            s.executeUpdate();
            ResultSet rs = s.getGeneratedKeys();
            if (rs.next()) {

                PrismLogHandler.log("Registering new action type to the database/cache: "
                        + action.name + " " + rs.getInt(1));
                ActionRegistry.prismActions.put(action, rs.getInt(1));
            } else {
                throw new SQLDataException("Insert statement failed - no generated key obtained.");
            }
            rs.close();
        } catch (SQLIntegrityConstraintViolationException | SQLDataException e) {
            PrismLogHandler.warn("Action : " + action.name + " / " + e.getMessage());
        } catch (final SQLException e) {
            handleDataSourceException(e);
        }
    }

    protected void cacheActionPrimaryKeys() {

        try (
                Connection conn = getConnection()
        ) {
            try (
                    PreparedStatement s = conn.prepareStatement("SELECT action_id, action FROM " + prefix
                            + "actions");
                    ResultSet rs = s.executeQuery()
                ) {
                while (rs.next()) {
                    PrismLogHandler.debug("Loaded " + rs.getString(2) + ", id:" + rs.getInt(1));
                    ActionType type = ActionType.getByName(rs.getString(2));
                    if (type != null) {
                        ActionRegistry.prismActions.put(type, rs.getInt(1));
                    }
                }
                PrismLogHandler.debug("Loaded " + ActionRegistry.prismActions.size() + " actions into the cache.");
            } catch (SQLException e) {
                PrismLogHandler.warn(e.getMessage(),e);
            }
        } catch (final SQLException e) {
            handleDataSourceException(e);
        }
    }


    /**
     * Cache the world keys.
     * @param prismWorlds Map
     */
    @Override
    public void cacheWorldPrimaryKeys(Map<String, Integer> prismWorlds) {

        try (
                Connection conn = getConnection();
                PreparedStatement s = conn.prepareStatement(
                        "SELECT world_id, world FROM " + prefix + "worlds");
                ResultSet rs = s.executeQuery()
        ) {
            while (rs.next()) {
                prismWorlds.put(rs.getString(2), rs.getInt(1));
            }
            PrismLogHandler.debug("Loaded " + prismWorlds.size() + " worlds into the cache.");
        } catch (final SQLException e) {
            handleDataSourceException(e);
        }
    }

    /**
     * Saves a world name to the database, and adds the id to the cache hashmap.
     */
    public void addWorldName(String worldName) {

        if (Prism.prismWorlds.containsKey(worldName)) {
            return;
        }
        String query = "INSERT INTO " + prefix + "worlds (world) VALUES (?)";
        try (
                Connection conn = database.getConnection();
                PreparedStatement s = conn.prepareStatement(query, Statement.RETURN_GENERATED_KEYS)
        ) {
            s.setString(1, worldName);
            s.executeUpdate();
            ResultSet rs = s.getGeneratedKeys();
            if (rs.next()) {
                PrismLogHandler.log("Registering new world to the database/cache: " + worldName + " " + rs.getInt(1));
                Prism.prismWorlds.put(worldName, rs.getInt(1));
            } else {
                throw new SQLException("Insert statement failed - no generated key obtained.");
            }
            rs.close();
        } catch (final SQLException e) {
            handleDataSourceException(e);
        }
    }

    @Override
    public void dispose() {
        if (database != null) {
            database.close();
        }
        database = null;
    }

    @Override
    public SelectQuery createSelectQuery() {
        return new SqlSelectQueryBuilder(this);
    }

    @Override
    public SelectIdQuery createSelectIdQuery() {
        return new SqlSelectIdQueryBuilder(this);
    }

    @Override
    public DeleteQuery createDeleteQuery() {
        return new SqlDeleteQueryBuilder(this);
    }

    @Override
    public BlockReportQuery createBlockReportQuery() {
        return new SqlBlockReportQueryBuilder(this);
    }

    @Override
    public ActionReportQuery createActionReportQuery() {
        return new SqlActionReportQueryBuilder(this);
    }

    @Override
    public SettingsQuery createSettingsQuery() {
        if (settingsQuery == null) {
            settingsQuery = new SqlSettingsQuery(this);
        }
        return settingsQuery;
    }

    public final void setDatabaseSchemaVersion(Integer ver) {
        createSettingsQuery().saveSetting("schema_ver", ver.toString(),null);
    }

    @Override
    public SelectProcessActionQuery createProcessQuery() {
        return new SqlSelectProcessQuery(this);
    }

    public InsertQuery getDataInsertionQuery() {
        return new SqlInsertBuilder(this);
    }

    @Override
    public boolean reportDataSource(StringBuilder builder, boolean toHandle) {
        if (database == null) {
            builder.append("Error: Database NULL ");
            if (toHandle) {
                builder.append(" -- Rebuilding ");
                rebuildDataSource();
            }
            return false;
        }
        try (Connection conn = database.getConnection()) {
            if (conn == null) {
                builder.append(Il8nHelper.getMessage("pool-no-valid"));
                return false;
            } else if (conn.isClosed()) {
                builder.append(Il8nHelper.getMessage("pool-connection-closed"));
                return false;
            } else if (conn.isValid(5)) {
                builder.append(Il8nHelper.getMessage("pool-valid-connection")).append(" ");
                builder.append(Il8nHelper.getMessage("recorder-restarting"));
                return true;
            }
        } catch (final SQLException e) {
            builder.append("Error: ").append(e.getMessage());
            if (toHandle) {
                handleDataSourceException(e);
            } else {
                e.printStackTrace();
            }
            return false;
        }
        return true;
    }
}
