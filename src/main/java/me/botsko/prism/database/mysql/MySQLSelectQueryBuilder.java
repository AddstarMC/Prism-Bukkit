package me.botsko.prism.database.mysql;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionType;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actionlibs.RecordingManager;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.database.QueryBuilder;
import me.botsko.prism.database.SQL.SQLSelectQueryBuilder;
import me.botsko.prism.database.SelectQuery;
import me.botsko.prism.measurement.TimeTaken;
import me.botsko.prism.players.PlayerIdentification;
import me.botsko.prism.utils.IntPair;
import me.botsko.prism.utils.ItemUtils;
import me.botsko.prism.utils.MaterialAliases.MaterialState;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.OfflinePlayer;
import org.bukkit.block.data.BlockData;
import org.bukkit.inventory.ItemStack;
import org.bukkit.util.Vector;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.*;
import java.util.Map.Entry;

public class MySQLSelectQueryBuilder extends SQLSelectQueryBuilder {

    /**
     *
     */
    public MySQLSelectQueryBuilder(PrismDataSource dataSource) {
        super(dataSource);
    }

    /**
     * @return
     */
    @Override
    protected String select() {

        String query = "";

        query += "SELECT ";
        if (shouldGroup) {
            columns.add("any_value(id) id");
            columns.add("any_value(epoch) epoch");
            columns.add("any_value(action_id) action_id");
            columns.add("any_value(player) player");
            columns.add("any_value(world_id) world_id");
        } else {
            columns.add("id");
            columns.add("epoch");
            columns.add("action_id");
            columns.add("player");
            columns.add("world_id");
        }

        if (shouldGroup) {
            columns.add("AVG(x)");
            columns.add("AVG(y)");
            columns.add("AVG(z)");
        } else {
            columns.add("x");
            columns.add("y");
            columns.add("z");
        }

        columns.add("any_value(block_id) block_id");
        columns.add("any_value(block_subid) block_subid");
        columns.add("any_value(old_block_id) old_block_id");
        columns.add("any_value(old_block_subid) old_block_subid");
        columns.add("any_value(data) data");
        columns.add("any_value(HEX(player_uuid)) AS uuid");

        if (shouldGroup) {
            columns.add("COUNT(*) counted");
        }

        // Append all columns
        if (columns.size() > 0) {
            query += TypeUtils.join(columns, ", ");
        }

        // From
        query += " FROM " + tableNameData + " ";

        // Joins
        query += "INNER JOIN " + prefix + "players p ON p.player_id = " + tableNameData + ".player_id ";
        query += "LEFT JOIN " + tableNameDataExtra + " ex ON ex.data_id = " + tableNameData + ".id ";

        return query;

    }

    /**
     * @return
     */
    @Override
    protected String order() {
        if (parameters == null) return " ";
        final String sort_dir = parameters.getSortDirection();

        if (shouldGroup) {
            return " ORDER BY MAX(" + tableNameData + ".epoch) " + sort_dir + ", AVG(x) ASC, AVG(z) ASC, AVG(y) ASC, any_value(id) " + sort_dir;
        }

        return " ORDER BY " + tableNameData + ".epoch " + sort_dir + ", x ASC, z ASC, y ASC, id " + sort_dir;
    }
}