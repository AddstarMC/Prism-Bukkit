package me.botsko.prism.database;

import me.botsko.prism.api.actions.Handler;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 1/06/2019.
 */
public interface InsertQuery {
    /**
     * Returns the id of the action.
     * @param a Handler
     * @return long
     */
    long insertActionIntoDatabase(Handler a);

    void createBatch() throws Exception;

    boolean addInsertionToBatch(Handler a) throws Exception;

    void processBatch() throws Exception;

}
