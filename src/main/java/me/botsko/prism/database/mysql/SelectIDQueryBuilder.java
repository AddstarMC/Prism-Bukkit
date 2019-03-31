package me.botsko.prism.database.mysql;

import me.botsko.prism.Prism;

/**
 * THis Class will return an id set for a specific query OR it can return the min and max ID's
 *
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 31/03/2019.
 */
public class SelectIDQueryBuilder extends SelectQueryBuilder {
    /**
     * @param plugin
     */
    private String select = "";
    public SelectIDQueryBuilder(Prism plugin) {
        super(plugin);
        setMin();
    }

    @Override
    protected String select() {
        return select;
    }

    public void setMax(){
        select  = "SELECT max(id) FROM "+tableNameData+ " ";
    }

    public void setMin(){
        select  = "SELECT min(id) FROM "+tableNameData + " ";
    }

}
