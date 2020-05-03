package me.botsko.prism.database.sql;

import me.botsko.prism.database.DeleteQuery;
import me.botsko.prism.database.PrismDataSource;

import java.sql.Connection;
import java.sql.SQLException;
import java.sql.Statement;

public class SqlDeleteQueryBuilder extends SqlSelectQueryBuilder implements DeleteQuery {

    public SqlDeleteQueryBuilder(PrismDataSource dataSource) {
        super(dataSource);
    }

    @Override
    protected String select() {
        return "DELETE FROM " + tableNameData + " USING " + tableNameData + " LEFT JOIN " + tableNameDataExtra
                + " ex ON (" + tableNameData + ".id = ex.data_id) ";
    }

    @Override
    protected String group() {
        return "";
    }

    @Override
    protected String order() {
        return "";
    }

    @Override
    protected String limit() {
        return "";
    }

    @Override
    public int execute() {
        if (shouldPause) {
            dataSource.setPaused(true); //pause so that the database cannot process the queue.
        }
        int totalRowsAffected = 0;
        int cycleRowsAffected;

        try (
                Connection connection = dataSource.getDataSource().getConnection();
                Statement s = connection.createStatement()
        ) {
            cycleRowsAffected = s.executeUpdate(getQuery(parameters, shouldGroup));
            totalRowsAffected += cycleRowsAffected;
        } catch (final SQLException e) {
            dataSource.handleDataSourceException(e);
        }
        dataSource.setPaused(false);
        return totalRowsAffected;
    }
}