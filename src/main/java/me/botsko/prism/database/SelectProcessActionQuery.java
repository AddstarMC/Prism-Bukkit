package me.botsko.prism.database;

import me.botsko.prism.actions.PrismProcessAction;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 6/04/2019.
 */
public interface SelectProcessActionQuery extends SelectQuery {

    PrismProcessAction executeProcessQuery();

    void isLastProcessID();

    long getLastProcessIdQuery();
}
