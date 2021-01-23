package me.botsko.prism.database.sql.derby;

import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.database.sql.SqlSelectQueryBuilder;
import me.botsko.prism.utils.TypeUtils;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 23/01/2021.
 */
public class DerbySelectQueryBuilder extends SqlSelectQueryBuilder {

    public DerbySelectQueryBuilder(PrismDataSource dataSource) {
        super(dataSource);
    }

    @Override
    protected String select() {
        String query = "";
        query += "SELECT ";
        if (shouldGroup) {
            columns.add("MIN(id) id");
            columns.add("MIN(epoch) epoch");
            columns.add("MIN(action_id) action_id");
            columns.add("MIN(player) player");
            columns.add("MIN(world_id) world_id");
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

        if (shouldGroup) {
            columns.add("MIN(block_id) block_id");
            columns.add("MIN(block_subid) block_subid");
            columns.add("MIN(old_block_id) old_block_id");
            columns.add("MIN(old_block_subid) old_block_subid");
            columns.add("MIN(data) data");
            columns.add("MIN(player_uuid) AS uuid");
        } else {
            columns.add("block_id");
            columns.add("block_subid");
            columns.add("old_block_id");
            columns.add("old_block_subid");
            columns.add("data");
            columns.add("player_uuid AS uuid");
        }

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

}
