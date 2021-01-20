package me.botsko.prism.database.sql;

import me.botsko.prism.Prism;
import me.botsko.prism.PrismLogHandler;
import me.botsko.prism.actionlibs.ActionRegistry;
import org.bukkit.configuration.ConfigurationSection;

import java.sql.Connection;
import java.sql.DatabaseMetaData;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.Collection;
import java.util.HashSet;

/**
 * Created for the Prism-Bukkit Project.
 *
 * @author Narimm on
 * 19/01/2021.
 */
public abstract class StandardSqlPrismDataSource extends SqlPrismDataSource {


    /**
     * Constructor.
     *
     * @param section Config
     */
    public StandardSqlPrismDataSource(ConfigurationSection section) {
        super(section);
    }

    @Override
    public void setupDatabase(ActionRegistry actionRegistry) {
        try (
                Connection  conn = getConnection();
                Statement st = conn.createStatement()
        ) {
            DatabaseMetaData meta = conn.getMetaData();
            ResultSet res = meta.getTables(null,null,null,new String[]{"TABLE"});
            Collection<String> tableNames = new HashSet<>();
            while (res.next()) {
                String name = res.getString("TABLE_NAME");
                tableNames.add(name);
            }
            res.close();
            String table1 = prefix + "actions";
            String query;
            if (!tableNames.contains(prefix + "actions")) {
                query = "CREATE TABLE " + table1 + " ("
                        + "action_id int NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1) PRIMARY KEY,"
                        + "action varchar(25) NOT NULL UNIQUE"
                        + ");";
                st.executeUpdate(query);
            } else {
                PrismLogHandler.debug(table1 + " already exists.");
            }
            String table2 = prefix + "data";
            if (!tableNames.contains(table2)) {
                query = "CREATE TABLE " + table2 + " ("
                        + "id int NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1) PRIMARY KEY,"
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
                        + "old_block_subid int DEFAULT NULL,"
                        + ");";
                st.executeUpdate(query);
                st.executeUpdate("CREATE INDEX epoch ON " + table2 + " (epoch);");
                st.executeUpdate("CREATE INDEX location ON " + table2 + " (world_id, x, z, y, action_id);");
                st.executeUpdate("CREATE INDEX player ON " + table2 + " (player_id);");
            } else {
                PrismLogHandler.debug(table2 + " already exists.");
            }
            String table3 = prefix+"data_extra";
            if (!tableNames.contains(table3)) {
                query = "CREATE TABLE " + table3 + " ("
                        + "extra_id int NOT NULL GENERATED ALWAYS AS IDENTITY (START WITH 1, INCREMENT BY 1) PRIMARY KEY,"
                        + "data_id int NOT NULL,"
                        + "data long varchar,"
                        + "te_data long varchar,"
                        + ");";
                st.executeUpdate(query);
                st.executeUpdate("CREATE INDEX data_id ON " + table3 + " (data_id);");

                // add extra data delete cascade
                query = "ALTER TABLE " + table3 + " ADD CONSTRAINT " + prefix + "data_extra_ibfk_1 " +
                        "FOREIGN KEY (data_id) REFERENCES "+ table2 + " (id) ON DELETE CASCADE ON UPDATE NO ACTION;";
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
        } catch (final SQLException e) {
            handleDataSourceException(e);

            me.botsko.prism.PrismLogHandler.log("Database connection error: " + e.getMessage());
            e.printStackTrace();
        }
    }

    /**
     * Add action to db.
     * @param actionName String
     */
    public void addActionName(String actionName) {

        if (Prism.prismActions.containsKey(actionName)) {
            return;
        }
        try (
                Connection conn = database.getConnection();
                PreparedStatement s = conn.prepareStatement("INSERT INTO " + prefix + "actions (action) VALUES (?)",
                        Statement.RETURN_GENERATED_KEYS)
        ) {
            s.setString(1, actionName);
            s.executeUpdate();
            ResultSet rs = s.getGeneratedKeys();
            if (rs.next()) {
                me.botsko.prism.PrismLogHandler.log("Registering new action type to the database/cache: " + actionName + " " + rs.getInt(1));
                Prism.prismActions.put(actionName, rs.getInt(1));
            } else {
                throw new SQLException("Insert statement failed - no generated key obtained.");
            }
            rs.close();
        } catch (final SQLException e) {
            handleDataSourceException(e);

        }
    }
}
