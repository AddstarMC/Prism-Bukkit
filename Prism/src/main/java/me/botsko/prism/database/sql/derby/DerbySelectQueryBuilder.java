package me.botsko.prism.database.sql.derby;

import me.botsko.prism.api.PrismParameters;
import me.botsko.prism.api.actions.PrismProcessType;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.database.sql.SqlSelectQueryBuilder;
import me.botsko.prism.utils.TypeUtils;
import org.jetbrains.annotations.Nullable;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 23/01/2021.
 */
public class DerbySelectQueryBuilder extends SqlSelectQueryBuilder {

    public DerbySelectQueryBuilder(PrismDataSource<?> dataSource) {
        super(dataSource);
    }

    @Override
    public String getQuery(@Nullable PrismParameters parameters, boolean shouldGroup) {
        return super.getQuery(parameters, shouldGroup);
    }

    @Override
    protected String select() {
        String query = "";
        query += "SELECT ";
        if (shouldGroup) {
            query += "r.id as id,r.epoch as epoch,r.action_id as action_id,r.player as player,"
                    + "r.world_id as world_id,r.x as x,r.y as y,r.z as z,r.block_id as block_id,"
                    + "r.block_subid as block_subid,r.old_block_id as old_block_id,"
                    + "r.old_block_subid as old_block_subid,ex.DATA as data,r.uuid as uuid,"
                    + "r.counted as counted FROM (SELECT MIN(id) id,MIN(epoch) epoch,"
                    + "MIN(action_id) AS action_id,MIN(player) player,MIN(world_id) world_id,"
                    + "AVG(x) as x,AVG(y) as y,AVG(z) as z,MIN(block_id) AS block_id, "
                    + "MIN(block_subid) AS block_subid,MIN(old_block_id) AS old_block_id,"
                    + "MIN(old_block_subid) AS old_block_subid,MIN(player_uuid) AS uuid,"
                    + "COUNT(*) AS counted,DATE({fn TIMESTAMPADD(SQL_TSI_SECOND, " + tableNameData
                    + ".epoch, TIMESTAMP('1970-01-01-00.00.00.000000')) }) as epochDate "
                    + "FROM " + tableNameData + " INNER JOIN " + prefix
                    + "players p ON p.player_id = " + tableNameData + ".player_id";
        } else {
            columns.add("id");
            columns.add("epoch");
            columns.add("action_id");
            columns.add("player");
            columns.add("world_id");
            columns.add("x");
            columns.add("y");
            columns.add("z");
            columns.add("block_id");
            columns.add("block_subid");
            columns.add("old_block_id");
            columns.add("old_block_subid");
            columns.add("data");
            columns.add("player_uuid AS uuid");
            query += TypeUtils.join(columns, ", ");
            query += " FROM " + tableNameData + " ";
            query += "INNER JOIN " + prefix + "players p ON p.player_id = " + tableNameData + ".player_id ";
            query += "LEFT JOIN " + tableNameDataExtra + " ex ON ex.data_id = " + tableNameData + ".id ";
        }

        return query;

    }

    @Override
    protected String group() {
        if (shouldGroup) {
            return " GROUP BY " + tableNameData + ".action_id, " + tableNameData + ".player_id, " + tableNameData
                    + ".block_id, DATE({fn TIMESTAMPADD(SQL_TSI_SECOND, " + tableNameData
                    + ".epoch, TIMESTAMP('1970-01-01-00.00.00.000000')) })";
        }
        return "";
    }

    @Override
    protected String limit() {
        if (parameters == null) {
            if (shouldGroup) {
                return ") as r LEFT JOIN " + tableNameDataExtra + " ex ON ex.data_id = r.id";
            } else {
                return "";
            }
        }
        if (parameters.getProcessType().equals(PrismProcessType.LOOKUP)) {
            final int limit = parameters.getLimit();
            if (limit > 0) {
                if (shouldGroup) {
                    return " FETCH NEXT " + limit + " ROWS ONLY) as r LEFT JOIN "
                            + tableNameDataExtra + " ex ON ex.data_id = r.id";
                } else {
                    return " FETCH NEXT " + limit + " ROWS ONLY";
                }
            } else {
                if (shouldGroup) {
                    return ") as r LEFT JOIN " + tableNameDataExtra + " ex ON ex.data_id = r.id";
                }
            }
        }
        if (shouldGroup) {
            return ") as r LEFT JOIN " + tableNameDataExtra + " ex ON ex.data_id = r.id";
        } else {
            return "";
        }
    }

}
