package me.botsko.prism.database.sql.derby;

import me.botsko.prism.PrismLogHandler;
import me.botsko.prism.actionlibs.ActionRegistry;
import me.botsko.prism.database.IdMapQuery;
import me.botsko.prism.database.PlayerIdentificationQuery;
import me.botsko.prism.database.SelectProcessActionQuery;
import me.botsko.prism.database.SelectQuery;
import me.botsko.prism.database.sql.PrismHikariDataSource;
import me.botsko.prism.settings.Settings;
import org.bukkit.configuration.ConfigurationSection;

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
public class DerbySqlPrismDataSource extends PrismHikariDataSource {

    /**
     * Constructor.
     *
     * @param section Config
     */
    public DerbySqlPrismDataSource(ConfigurationSection section) {
        super(section);
        name = "derby";
    }

    @Override
    public void setupDatabase(ActionRegistry actionRegistry) {
        try (
                Connection conn = getConnection();
                Statement st = conn.createStatement()
        ) {
            DatabaseMetaData meta = conn.getMetaData();
            ResultSet res = meta.getSchemas();
            Collection<String> tableNames = new HashSet<>();
            while (res.next()) {
                String scheam = res.getString("TABLE_SCHEM");
                String cat = res.getString("TABLE_CATALOG");

                tableNames.add(name);
            }
            res.close();
            if (
                    setupTable1(st, tableNames)
                            && setupTable2(st, tableNames)
                            && setupTable3(st, tableNames)
                            && setupTable4(st, tableNames)
                            && setupTable5(st, tableNames)
                            && setupTable6(st, tableNames)
                            && setupTable7(st, tableNames)) {
                Settings.saveSetting("schema_ver", Integer.toString(8));
            }
            // actions
            cacheActionPrimaryKeys(); // Pre-cache, so we know if we need to
            // populate db
            final String[] actions = actionRegistry.listAll();
            for (final String a : actions) {
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
        if (!tableNames.contains(table)) {
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
        if (!tableNames.contains(table2)) {
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
        if (!tableNames.contains(table3)) {
            String query = "CREATE TABLE " + table3 + " ("
                    + "extra_id bigint NOT NULL GENERATED ALWAYS AS "
                    + "IDENTITY (START WITH 1, INCREMENT BY 1) PRIMARY KEY,"
                    + "data_id bigint NOT NULL CONSTRAINT " + prefix
                    + "data_extra_ibfk_1 REFERENCES " + prefix + "data" + " (id) ON DELETE CASCADE ON UPDATE NO ACTION,"
                    + "data long varchar,"
                    + "te_data long varchar"
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
        if (!tableNames.contains(table4)) {
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
        if (!tableNames.contains(table5)) {
            String query = "CREATE TABLE " + table5 + " ("
                    + "player_id int NOT NULL GENERATED ALWAYS AS "
                    + "IDENTITY (START WITH 1, INCREMENT BY 1) PRIMARY KEY,"
                    + "player varchar(255) NOT NULL UNIQUE,"
                    + "player_uuid char(16) NOT NULL UNIQUE"
                    + ")";
            st.executeUpdate(query);
            return true;
        }
        return false;
    }

    private boolean setupTable6(Statement st, Collection<String> tableNames) throws SQLException {
        String table6 = prefix + "worlds";
        if (!tableNames.contains(table6)) {
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
        if (!tableNames.contains(table7)) {
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
    public SelectProcessActionQuery createProcessQuery() {
        return new DerbySqlSelectProcessQuery(this);
    }

    @Override
    public SelectQuery createSelectQuery() {
        return new DerbySelectQueryBuilder(this);
    }


}
