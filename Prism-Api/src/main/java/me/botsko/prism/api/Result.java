package me.botsko.prism.api;

import me.botsko.prism.api.actions.Handler;

import java.util.List;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 10/01/2021.
 */
public interface Result {

    List<Handler> getActionResults();

    List<Handler> getPaginatedActionResults();

    PrismParameters getParameters();

    int getTotalResults();

    long getQueryTime();

    long getLastTeleportIndex();

    void setLastTeleportIndex(long index);

    int getIndexOfFirstResult();

    int getPage();

    void setPage(int page);

    int getTotalPages();

}
