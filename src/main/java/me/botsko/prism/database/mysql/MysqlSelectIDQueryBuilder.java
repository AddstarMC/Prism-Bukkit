package me.botsko.prism.database.mysql;

import me.botsko.prism.Prism;
import me.botsko.prism.database.SelectIDQuery;

/**
 * THis Class will return an id set for a specific query OR it can return the min and max ID's
 *
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 31/03/2019.
 */
public class MysqlSelectIDQueryBuilder extends MySQLSelectQueryBuilder implements SelectIDQuery {
    /**
     * @param plugin
     */
    private String select = "";
    public MysqlSelectIDQueryBuilder(Prism plugin) {
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
