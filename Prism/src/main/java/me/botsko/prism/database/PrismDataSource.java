package me.botsko.prism.database;

import me.botsko.prism.actionlibs.ActionRegistry;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.Map;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 5/04/2019.
 */
public interface PrismDataSource {

    boolean isPaused();

    void setPaused(boolean paused);

    String getName();

    default String getPrefix() {
        return "prism_";
    }

    PrismDataSource createDataSource();

    void setupDatabase(ActionRegistry actionRegistry);

    Connection getConnection();

    void rebuildDataSource();


    DataSource getDataSource();

    void handleDataSourceException(SQLException e);

    void cacheWorldPrimaryKeys(Map<String, Integer> prismWorlds);

    void addWorldName(String worldName);

    void addActionName(String actionName);

    void dispose();

    SelectQuery createSelectQuery();

    SelectIdQuery createSelectIdQuery();

    DeleteQuery createDeleteQuery();

    BlockReportQuery createBlockReportQuery();

    ActionReportQuery createActionReportQuery();

    SettingsQuery createSettingsQuery();

    void setDatabaseSchemaVersion(Integer ver);

    SelectProcessActionQuery createProcessQuery();

    InsertQuery getDataInsertionQuery();

    PlayerIdentificationQuery getPlayerIdHelper();

    IdMapQuery getIdMapQuery();

    default boolean reportDataSource(StringBuilder builder) {
        return reportDataSource(builder,false);
    }

    boolean reportDataSource(StringBuilder builder, boolean toHandle);

    PrismDataSourceUpdater getUpdater();
}
