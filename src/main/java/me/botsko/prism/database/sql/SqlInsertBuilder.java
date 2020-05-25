package me.botsko.prism.database.sql;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.database.InsertQuery;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.database.QueryBuilder;
import me.botsko.prism.players.PlayerIdentification;
import me.botsko.prism.players.PrismPlayer;
import me.botsko.prism.utils.block.Utilities;
import me.botsko.prism.utils.IntPair;
import org.bukkit.Location;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by Narimm on 1/06/2019.
 */
public class SqlInsertBuilder extends QueryBuilder implements InsertQuery {
    final ArrayList<Handler> extraDataQueue = new ArrayList<>();
    private PreparedStatement batchStatement;
    private Connection batchConnection;

    /**
     * @param dataSource Data source
     */
    public SqlInsertBuilder(PrismDataSource dataSource) {
        super(dataSource);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public long insertActionIntoDatabase(Handler a) {
        int world_id = 0;
        long id = 0;
        String worldName = a.getLoc().getWorld().getName();
        if (Prism.prismWorlds.containsKey(worldName)) {
            world_id = Prism.prismWorlds.get(worldName);
        }
        int action_id = 0;
        if (Prism.prismActions.containsKey(a.getActionType().getName())) {
            action_id = Prism.prismActions.get(a.getActionType().getName());
        }

        PrismPlayer prismPlayer = PlayerIdentification.cachePrismPlayer(a.getSourceName());
        int player_id = prismPlayer.getId();

        if (world_id == 0 || action_id == 0 || player_id == 0) {
            Prism.debug("Sql data error: Handler:" + a.toString());
        }
        IntPair newIds = Prism.getItems().materialToIds(a.getMaterial(), Utilities.dataString(a.getBlockData()));
        IntPair oldIds = Prism.getItems().materialToIds(a.getOldMaterial(), Utilities.dataString(a.getOldBlockData()));

        Location l = a.getLoc();

        try (Connection con = dataSource.getConnection();
             PreparedStatement s = con.prepareStatement(getQuery(), Statement.RETURN_GENERATED_KEYS);
        ) {
            applytoInsert(s, a, action_id, player_id, world_id, newIds, oldIds, l);
            s.executeUpdate();
            ResultSet generatedKeys = s.getGeneratedKeys();
            if (generatedKeys.next()) {
                id = generatedKeys.getLong(1);
            }
            if (a.hasExtraData()) {
                String serialData = a.serialize();
                if (serialData != null && !serialData.isEmpty()) {

                    try (
                            PreparedStatement s2 = con.prepareStatement(
                                    "INSERT INTO `" + prefix + "data_extra` (data_id, data) VALUES (?, ?)", Statement.RETURN_GENERATED_KEYS)) {
                        s2.setLong(1, id);
                        s2.setString(2, serialData);
                        s2.executeUpdate();
                    }
                }
            }
        } catch (SQLException e) {
            e.printStackTrace();
        }
        return id;
    }

    @Override
    public void createBatch() throws SQLException {
        batchConnection = dataSource.getConnection();
        if (batchConnection == null) {
            throw new SQLException("No Connection to database");
        }
        batchConnection.setAutoCommit(false);
        batchStatement = batchConnection.prepareStatement(getQuery(), Statement.RETURN_GENERATED_KEYS);
    }

    @Override
    public boolean addInsertionToBatch(Handler a) throws SQLException {
        if (batchStatement == null) {
            return false;
        }
        int world_id = 0;
        String worldName = a.getLoc().getWorld().getName();
        if (Prism.prismWorlds.containsKey(worldName)) {
            world_id = Prism.prismWorlds.get(worldName);
        }
        int action_id = 0;
        if (Prism.prismActions.containsKey(a.getActionType().getName())) {
            action_id = Prism.prismActions.get(a.getActionType().getName());
        }

        PrismPlayer prismPlayer = PlayerIdentification.cachePrismPlayer(a.getSourceName());
        int player_id = prismPlayer.getId();

        IntPair newIds = Prism.getItems().materialToIds(a.getMaterial(),
                Utilities.dataString(a.getBlockData()));

        IntPair oldIds = Prism.getItems().materialToIds(a.getOldMaterial(),
                Utilities.dataString(a.getOldBlockData()));
        Location l = a.getLoc();
        applytoInsert(batchStatement, a, action_id, player_id, world_id, newIds, oldIds, l);
        batchStatement.addBatch();
        extraDataQueue.add(a);
        return true;
    }

    public void processBatch() throws SQLException {
        if (batchStatement == null) {
            Prism.debug("Batch insert was null");
            throw new SQLException("no batchstatement configured");
        }
        batchStatement.executeBatch();
        batchConnection.commit();
        Prism.debug("Batch insert was commit: " + System.currentTimeMillis());
        processExtraData(batchStatement.getGeneratedKeys());
        batchConnection.close();
    }

    public void processExtraData(ResultSet keys) throws SQLException {
        if (extraDataQueue.isEmpty())
            return;
        try (Connection conn = dataSource.getConnection();
             PreparedStatement s = conn.prepareStatement("INSERT INTO `" + prefix + "data_extra` (data_id,data) VALUES (?,?)", Statement.RETURN_GENERATED_KEYS);
        ) {
            conn.setAutoCommit(false);
            int i = 0;
            while (keys.next()) {
                // @todo should not happen
                if (i >= extraDataQueue.size()) {
                    Prism.log("Skipping extra data for " + prefix + "data.id " + keys.getLong(1)
                            + " because the queue doesn't have data for it.");
                    continue;
                }

                final Handler a = extraDataQueue.get(i);
                if (a.hasExtraData()) {
                    String serialData = a.serialize();

                    if (serialData != null && !serialData.isEmpty()) {
                        s.setLong(1, keys.getLong(1));
                        s.setString(2, serialData);
                        s.addBatch();
                    }
                } else {
                    Prism.debug("Skipping extra data for " + prefix + "data.id " + keys.getLong(1)
                            + " because the queue doesn't have data for it.");
                }

                i++;
            }

            // The main delay is here
            s.executeBatch();

            if (conn.isClosed()) {
                Prism.log(
                        "Prism database error. We have to bail in the middle of building extra data bulk insert query.");
            } else {
                conn.commit();
            }
            conn.close();
        } catch (final SQLException e) {
            e.printStackTrace();
            Prism.getPrismDataSource().handleDataSourceException(e);
        }
    }

    private void applytoInsert(PreparedStatement s, Handler a, int action_id, int player_id, int world_id, IntPair newIds, IntPair oldIds, Location l) throws SQLException {
        s.setLong(1, a.getUnixEpoch());
        s.setInt(2, action_id);
        s.setInt(3, player_id);
        s.setInt(4, world_id);
        s.setInt(5, newIds.first);
        s.setInt(6, newIds.second);
        s.setInt(7, oldIds.first);
        s.setInt(8, oldIds.second);
        s.setInt(9, l.getBlockX());
        s.setInt(10, l.getBlockY());
        s.setInt(11, l.getBlockZ());
    }

    private String getQuery() {

        String sql = "INSERT INTO " + prefix
                + "data (epoch,action_id,player_id,world_id,block_id,block_subid,old_block_id,old_block_subid,x,y,z) VALUES (?,?,?,?,?,?,?,?,?,?,?)";
        return sql;
    }
}
