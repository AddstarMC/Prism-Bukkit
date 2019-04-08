package me.botsko.prism.database.SQL;

import me.botsko.prism.database.DeleteQuery;
import me.botsko.prism.database.PrismDataSource;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

public class SQLDeleteQueryBuilder extends SQLSelectQueryBuilder implements DeleteQuery {

    /**
     *
     */
    public SQLDeleteQueryBuilder(PrismDataSource dataSource) {
        super(dataSource);
    }

    /**
     *
     */
    @Override
    protected String select() {
        return "DELETE FROM " + tableNameData + " USING " + tableNameData + " LEFT JOIN " + tableNameDataExtra
                + " ex ON (" + tableNameData + ".id = ex.data_id) ";
    }

    /**
     *
     */
    @Override
    protected String group() {
        return "";
    }

    /**
     *
     */
    @Override
    protected String order() {
        return "";
    }

    /**
     *
     */
    @Override
    protected String limit() {
        return "";
    }

    @Override
    public int execute() {
        int total_rows_affected = 0;
        int cycle_rows_affected = 0;
        try (
                Connection connection = dataSource.getDataSource().getConnection();
                Statement s = connection.createStatement();
        ) {
            cycle_rows_affected = s.executeUpdate(getQuery(parameters, shouldGroup));
            total_rows_affected += cycle_rows_affected;
        } catch (final SQLException e) {
            dataSource.handleDataSourceException(e);
        }
        return total_rows_affected;
    }
}