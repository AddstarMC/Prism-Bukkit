package me.botsko.prism.database.sql;

import me.botsko.prism.api.actions.Handler;
import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.database.QueryBuilder;
import me.botsko.prism.database.UpdateQuery;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

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
    public void updateRollbacked(Handler handler) {
        try {
            Connection conn = dataSource.getConnection();
            Statement s = conn.createStatement();
            String sql = "UPDATE " + prefix + "data SET rollbacked = " + (handler.isRollbacked() ? 1 : 0) + " WHERE id = " + handler.getId();
            s.executeUpdate(sql);
        } catch (SQLException e) {
            dataSource.handleDataSourceException(e);
        }
    }
}
