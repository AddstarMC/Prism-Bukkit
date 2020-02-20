package me.botsko.prism.database;

import me.botsko.prism.actionlibs.ActionRegistry;
import org.slf4j.Logger;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 5/04/2019.
 */
public interface PrismDataSource {

    boolean isPaused();

    void setPaused(boolean paused);

    String getName();

    String getPrefix();

    PrismDataSource createDataSource();

    Logger getLog();

    void setFile();

    void setupDatabase(ActionRegistry actionRegistry);

    Connection getConnection();

    void rebuildDataSource();

    DataSource getDataSource();

    void handleDataSourceException(SQLException e);

    void cacheWorldPrimaryKeys(HashMap prismWorlds);

    void addWorldName(String worldName);

    void addActionName(String actionName);

    void dispose();

    SelectQuery createSelectQuery();

    SelectIDQuery createSelectIDQuery();

    DeleteQuery createDeleteQuery();

    BlockReportQuery createBlockReportQuery();

    ActionReportQuery createActionReportQuery();

    SettingsQuery createSettingsQuery();

    SelectProcessActionQuery createProcessQuery();

    InsertQuery getDataInsertionQuery();
}
