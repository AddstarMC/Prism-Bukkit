package me.botsko.prism.database;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 5/04/2019.
 */
public interface SelectIDQuery extends SelectQuery {

    public abstract void setMax();

    public abstract void setMin();
}
