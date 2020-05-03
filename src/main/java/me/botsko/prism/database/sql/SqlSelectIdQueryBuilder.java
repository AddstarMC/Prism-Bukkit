package me.botsko.prism.database.sql;

import me.botsko.prism.database.PrismDataSource;
import me.botsko.prism.database.SelectIdQuery;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * THis Class will return an id set for a specific query OR it can return the min and max ID's
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 31/03/2019.
 */
public class SqlSelectIdQueryBuilder extends SqlSelectQueryBuilder implements SelectIdQuery {
    /**
     * Constructor.
     * @param dataSource PrismDataSource
     */
    private String select = "";

    public SqlSelectIdQueryBuilder(PrismDataSource dataSource) {
        super(dataSource);
        setMin();
    }

    @Override
    protected String select() {
        return select;
    }

    @Override
    protected String order() {
        return "";
    }

    public void setMax() {
        select = "SELECT max(id) FROM " + tableNameData + " ";
    }

    public void setMin() {
        select = "SELECT min(id) FROM " + tableNameData + " ";
    }

    @Override
    public long execute() {
        long id = 0;
        try (
                Connection connection = dataSource.getDataSource().getConnection();
                PreparedStatement s = connection.prepareStatement(getQuery(parameters, shouldGroup));
                ResultSet rs = s.executeQuery()
        ) {
            if (rs.first()) {
                id = rs.getLong(1);
            }
        } catch (final SQLException e) {
            dataSource.handleDataSourceException(e);
        }
        return id;
    }

}
