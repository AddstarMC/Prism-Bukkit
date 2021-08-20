package me.botsko.prism.database.sql;

import me.botsko.prism.api.actions.Handler;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.database.QueryBuilder;
import me.botsko.prism.database.UpdateQuery;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;

public class SqlUpdateQuery extends QueryBuilder implements UpdateQuery {

    /**
     * Build a query.
     *
     * @param dataSource
     */
    public SqlUpdateQuery(PrismDataSource dataSource) {
        super(dataSource);
    }

    @Override
    public void updateRollbacked(Handler... handlers) {
        ArrayList<Handler> rollbacked = new ArrayList<>();
        ArrayList<Handler> notRollbacked = new ArrayList<>();
        for (Handler handler : handlers) {
            if (handler.isRollbacked()) {
                rollbacked.add(handler);
            } else {
                notRollbacked.add(handler);
            }
        }
        try {
            Connection conn = dataSource.getConnection();
            Statement s = conn.createStatement();
            StringBuilder sqlBuilder = new StringBuilder("UPDATE " + prefix + "data SET rollbacked = 1 WHERE id in (");
            int size = rollbacked.size();
            if (size > 0) {
                sqlBuilder.append(rollbacked.get(0).getId());
                for (int i = 1; i < size; i++) {
                    sqlBuilder.append(", ").append(rollbacked.get(i).getId());
                }
                s.executeUpdate(sqlBuilder.append(")").toString());
            }

            sqlBuilder = new StringBuilder("UPDATE " + prefix + "data SET rollbacked = 0 WHERE id in (");
            size = notRollbacked.size();
            if (size > 0) {
                sqlBuilder.append(notRollbacked.get(0).getId());
                for (int i = 1; i < size; i++) {
                    sqlBuilder.append(", ").append(notRollbacked.get(i).getId());
                }
                s.executeUpdate(sqlBuilder.append(")").toString());
            }
            conn.close();
        } catch (SQLException e) {
            dataSource.handleDataSourceException(e);
        }
    }
}
