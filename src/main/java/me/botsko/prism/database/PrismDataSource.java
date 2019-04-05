package me.botsko.prism.database;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionRegistry;
import me.botsko.prism.actionlibs.ActionsQuery;
import org.bukkit.configuration.Configuration;
import org.bukkit.configuration.ConfigurationSection;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.HashMap;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 5/04/2019.
 */
public interface PrismDataSource {
    public String getPrefix();
    public PrismDataSource createDataSource();

    public void setupDatabase(ActionRegistry actionRegistry);

    public Connection getConnection();

    public void rebuildDataSource();

    public DataSource getDataSource();

    public void handleDataSourceException(SQLException e);

    public void cacheWorldPrimaryKeys(HashMap prismWorlds);

    public void addWorldName(String worldName);
    public void addActionName(String actionName);
    public void dispose();

    public SelectQuery createSelectQuery(Prism plugin);
    public SelectIDQuery createSelectIDQuery(Prism plugin);
    public DeleteQuery createDeleteQuery(Prism plugin);
    public BlockReportQuery createBlockReportQuery(Prism plugin);
    public ActionReportQuery createActionReportQuery(Prism plugin);

}
