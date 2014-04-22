package me.botsko.prism.database.mysql;

import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;

public class ActionReportQueryBuilder extends SelectQueryBuilder {

    /**
     * 
     * @param plugin
     */
    public ActionReportQueryBuilder(Prism plugin) {
        super( plugin );
    }

    /**
     * 
     * @param parameters
     * @param shouldGroup
     * @return
     */
    @Override
    public String getQuery(QueryParameters parameters, boolean shouldGroup) {

        this.parameters = parameters;
        this.shouldGroup = shouldGroup;

        // Reset
        columns = new ArrayList<String>();
        conditions = new ArrayList<String>();

        String query = select();

        query += ";";

        if( plugin.getConfig().getBoolean( "prism.debug" ) ) {
            Prism.debug( query );
        }

        return query;

    }

    /**
	 * 
	 */
    @Override
    public String select() {
        String prefix = Prism.config.getString("prism.mysql.prefix");

        final String sql = "SELECT COUNT(*), a.action " + "FROM " + prefix + "data "
                + "INNER JOIN " + prefix + "actions a ON a.action_id = " + prefix + "data.action_id " + where() + " "
                + "GROUP BY a.action_id " + "ORDER BY COUNT(*) DESC";

        return sql;

    }
}