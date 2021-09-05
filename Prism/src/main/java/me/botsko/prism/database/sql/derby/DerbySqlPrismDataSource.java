package me.botsko.prism.database.sql.derby;

import com.zaxxer.hikari.HikariConfig;
import me.botsko.prism.Prism;
import me.botsko.prism.PrismLogHandler;
import me.botsko.prism.actionlibs.ActionRegistryImpl;
import me.botsko.prism.api.actions.ActionType;
import me.botsko.prism.config.ConfigHandler;
import me.botsko.prism.database.IdMapQuery;
import me.botsko.prism.database.PlayerIdentificationQuery;
import me.botsko.prism.database.PrismDataSourceUpdater;
import me.botsko.prism.database.SelectIdQuery;
import me.botsko.prism.database.SelectProcessActionQuery;
import me.botsko.prism.database.SelectQuery;
import me.botsko.prism.database.sql.HikariHelper;
import me.botsko.prism.database.sql.PrismHikariDataSource;
import org.spongepowered.configurate.ConfigurationNode;

import java.io.File;
import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collection;
import java.util.HashSet;

/**
 * Created for the Prism-Bukkit Project.
 *
 * @author Narimm
 */
public class DerbySqlPrismDataSource extends PrismHikariDataSource<DerbySqlConfig> {

    private DerbySqlConfig config;

    /**
     * Constructor.
     *
     * @param node Config
     */
    public DerbySqlPrismDataSource(ConfigurationNode node) {
        super(node);
        name = "derby";
    }

    @Override
    protected void setUpHikariProperties() {
        if (propFile == null) {
            propFile = new File(Prism.getInstance().getDataFolder(), "hikari.properties");
        }
        String dbFile = new File(propFile.getParent(), config.databaseName).getPath();
        if (propFile.exists()) {
            PrismLogHandler.log("Configuring Hikari from " + propFile.getName());
            dbConfig = new HikariConfig(propFile.getPath());
            dbConfig.setUsername(config.userName);
            dbConfig.setPassword(config.password);
            if (!dbConfig.getDriverClassName().isEmpty()) {
                try {
                    Class.forName(dbConfig.getDriverClassName()); //force load the driver.
                } catch (ClassNotFoundException e) {
                    PrismLogHandler.log("You will need to provide the required jar libs that support your database.");
                }
            }
        } else {
            PrismLogHandler.log("You may need to adjust these settings for your setup.");
            dbConfig = new HikariConfig();
            try {
                Class.forName("org.apache.derby.jdbc.AutoloadedDriver");
                dbConfig.setDriverClassName("org.apache.derby.jdbc.AutoloadedDriver");
                dbConfig.setJdbcUrl("jdbc:derby:" + dbFile + ";create=true");
            } catch (ClassNotFoundException e) {
                PrismLogHandler.log("You will need to provide the required jar libraries that support your database.");
                dbConfig.setJdbcUrl("");
            }
            dbConfig.setUsername(config.userName);
            dbConfig.setPassword(config.password);
            HikariHelper.saveHikariConfig(propFile, dbConfig, false);
            reportJdbcDrivers();
        }
    }

    @Override
    protected void setConfig() {
        config = ConfigHandler.getDataSourceConfig(DerbySqlConfig.class, dataSourceConfig);
        prefix = config.getPrefix();

    }

    @Override
    public DerbySqlConfig getConfig() {
        return config;
    }

    @Override
    public Class<DerbySqlConfig> getConfigurationClass() {
        return DerbySqlConfig.class;
    }

    @Override
    public void setupDatabase(ActionRegistryImpl actionRegistry) {
        try (
              Connection conn = getConnection();
              Statement st = conn.createStatement()
        ) {
            Collection<String> tableNames = new HashSet<>();
            DatabaseMetaData metaDataForDatabaseConnection = conn.getMetaData();
            ResultSet schemas = metaDataForDatabaseConnection.getSchemas();
            boolean prism_exists = false;
            while (schemas.next()) {
                if (schemas.getString(1).equals("PRISM")) {
                    prism_exists = true;
                }
            }
            schemas.close();
            if (!prism_exists) {
                st.execute("CREATE SCHEMA PRISM AUTHORIZATION " + dbConfig.getUsername());
            }
            st.execute("SET CURRENT SCHEMA PRISM");
            ResultSet rs = metaDataForDatabaseConnection
                    .getTables(null, "PRISM",
                            null, new String[]{"TABLE"});

            //ResultSet rs = st.executeQuery("SHOW TABLES IN " + config.userName);
            while (rs.next()) {
                tableNames.add(rs.getString(3));
            }
            rs.close();
            try {
                if (
                        setupTable1(st, tableNames)
                                && setupTable2(st, tableNames)
                                && setupTable3(st, tableNames)
                                && setupTable4(st, tableNames)
                                && setupTable5(st, tableNames)
                                && setupTable6(st, tableNames)
                                && setupTable7(st, tableNames)) {
                    PrismLogHandler.log("Database Setup Complete");
                }
            } catch (SQLException e) {
                PrismLogHandler.warn(e.getMessage());
            }
            /* actions */
            cacheActionPrimaryKeys(); // Pre-cache, so we know if we need to
            // populate db
            final ActionType[] actions = ActionType.values();
            for (final ActionType a : actions) {
                addActionName(a);
            }
        } catch (final SQLException e) {
            handleDataSourceException(e);

            PrismLogHandler.log("Database connection error: " + e.getMessage());
            e.printStackTrace();
        }
    }

    private boolean setupTable1(Statement st, Collection<String> tableNames) throws SQLException {
        String table = prefix + "actions";
        if (!tableNames.contains(table.toUpperCase())) {
            String query = "CREATE TABLE " + table + " ("
                  + "action_id int NOT NULL GENERATED ALWAYS AS "
                  + "IDENTITY (START WITH 1, INCREMENT BY 1) PRIMARY KEY,"
                  + "action varchar(25) NOT NULL UNIQUE"
                  + ")";
            st.executeUpdate(query);
            return true;
        } else {
            PrismLogHandler.debug(table + " already exists.");
        }
        return false;
    }

    private boolean setupTable2(Statement st, Collection<String> tableNames) throws SQLException {
        String table2 = prefix + "data";
        if (!tableNames.contains(table2.toUpperCase())) {
            String query = "CREATE TABLE " + table2 + " ("
                  + "id bigint NOT NULL GENERATED ALWAYS AS "
                  + "IDENTITY (START WITH 1, INCREMENT BY 1) PRIMARY KEY,"
                  + "epoch int NOT NULL,"
                  + "action_id int NOT NULL,"
                  + "player_id int NOT NULL,"
                  + "world_id int NOT NULL,"
                  + "x int NOT NULL,"
                  + "y int NOT NULL,"
                  + "z int NOT NULL,"
                  + "block_id int DEFAULT NULL,"
                  + "block_subid int DEFAULT NULL,"
                  + "old_block_id int DEFAULT NULL,"
                  + "old_block_subid int DEFAULT NULL"
                  + ")";
            st.executeUpdate(query);
            st.executeUpdate("CREATE INDEX epoch ON " + table2 + " (epoch)");
            st.executeUpdate("CREATE INDEX location ON " + table2 + " (world_id, x, z, y, action_id)");
            st.executeUpdate("CREATE INDEX player ON " + table2 + " (player_id)");
            return true;

        } else {
            PrismLogHandler.debug(table2 + " already exists.");
        }
        return false;
    }

    private boolean setupTable3(Statement st, Collection<String> tableNames) throws SQLException {
        String table3 = prefix + "data_extra";
        if (!tableNames.contains(table3.toUpperCase())) {
            String query = "CREATE TABLE " + table3 + " ("
                  + "extra_id bigint NOT NULL GENERATED ALWAYS AS "
                  + "IDENTITY (START WITH 1, INCREMENT BY 1) PRIMARY KEY,"
                  + "data_id bigint NOT NULL CONSTRAINT " + prefix
                  + "data_extra_ibfk_1 REFERENCES " + prefix + "data" + " (id) ON DELETE CASCADE ON UPDATE NO ACTION,"
                  + "data varchar(32000), "
                  + "te_data varchar(32000)"
                  + ")";
            st.executeUpdate(query);
            return true;
        } else {
            PrismLogHandler.debug(table3 + " already exists.");
        }
        return false;
    }

    private boolean setupTable4(Statement st, Collection<String> tableNames) throws SQLException {
        String table4 = prefix + "meta";
        if (!tableNames.contains(table4.toUpperCase())) {
            String query = "CREATE TABLE " + table4 + " ("
                  + "id int NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1) PRIMARY KEY,"
                  + "k varchar(25) NOT NULL,"
                  + "v varchar(255) NOT NULL"
                  + ")";
            st.executeUpdate(query);
            return true;
        }
        return false;

    }

    private boolean setupTable5(Statement st, Collection<String> tableNames) throws SQLException {
        String table5 = prefix + "players";
        if (!tableNames.contains(table5.toUpperCase())) {
            String query = "CREATE TABLE " + table5 + " ("
                  + "player_id int NOT NULL GENERATED ALWAYS AS "
                  + "IDENTITY (START WITH 1, INCREMENT BY 1) PRIMARY KEY,"
                  + "player varchar(255) NOT NULL UNIQUE,"
                  + "player_uuid char(32) NOT NULL UNIQUE"
                  + ")";
            st.executeUpdate(query);
            return true;
        }
        return false;
    }

    private boolean setupTable6(Statement st, Collection<String> tableNames) throws SQLException {
        String table6 = prefix + "worlds";
        if (!tableNames.contains(table6.toUpperCase())) {
            String query = "CREATE TABLE " + table6 + " ("
                  + "world_id int NOT NULL GENERATED ALWAYS AS "
                  + "IDENTITY (START WITH 1, INCREMENT BY 1) PRIMARY KEY,"
                  + "world varchar(255) NOT NULL UNIQUE"
                  + ")";
            st.executeUpdate(query);
            return true;
        }
        return false;
    }

    private boolean setupTable7(Statement st, Collection<String> tableNames) throws SQLException {
        String table7 = prefix + "id_map";
        if (!tableNames.contains(table7.toUpperCase())) {
            String query = "CREATE TABLE " + table7 + " ("
                  + "material varchar(63) NOT NULL,"
                  + "state varchar(255) NOT NULL,"
                  + "block_id int NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1),"
                  + "block_subid int NOT NULL DEFAULT 0,"
                  + "PRIMARY KEY (material, state),"
                  + "UNIQUE (block_id, block_subid)" + ")";
            st.executeUpdate(query);
            return true;
        }
        return false;
    }

    @Override
    public PlayerIdentificationQuery getPlayerIdHelper() {
        if (playerIdHelper == null) {
            playerIdHelper = new DerbySqlPlayerIdentificationQuery();
        }
        return playerIdHelper;
    }

    @Override
    public IdMapQuery getIdMapQuery() {
        if (idMapQuery == null) {
            idMapQuery = new DerbySqlIdMapQuery(this);
        }
        return idMapQuery;
    }

    @Override
    public PrismDataSourceUpdater getUpdater() {
        return new DerbySqlDataSourceUpdater();
    }

    @Override
    public SelectProcessActionQuery createProcessQuery() {
        return new DerbySqlSelectProcessQuery(this);
    }

    @Override
    public SelectQuery createSelectQuery() {
        return new DerbySelectQueryBuilder(this);
    }

    @Override
    public SelectIdQuery createSelectIdQuery() {
        return new DerbySelectIdQueryBuilder(this);
    }
}
