package me.botsko.prism.database;

import me.botsko.prism.actionlibs.QueryParameters;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collection;

abstract public class QueryBuilder {
    protected final String tableNameData;
    protected final String tableNameDataExtra;
    protected PrismDataSource dataSource;
    protected Collection<String> columns = new ArrayList<>();
    protected Collection<String> conditions = new ArrayList<>();
    protected QueryParameters parameters;
    protected boolean shouldGroup;
    protected String prefix = "prism_";
    protected boolean shouldPause;

    /**
     *
     */
    public QueryBuilder(PrismDataSource dataSource) {
        this.dataSource = dataSource;
        prefix = this.dataSource.getPrefix();
        tableNameData = prefix + "data";
        tableNameDataExtra = prefix + "data_extra";
    }

    public void setParameters(QueryParameters parameters) {
        this.parameters = parameters;
    }

    public void setShouldGroup(boolean shouldGroup) {
        this.shouldGroup = shouldGroup;
    }

    /**
     * Setting this will cause the recording queue to be unable to process while the query is running.
     *
     * @param shouldPause
     */
    public void setShouldPause(boolean shouldPause) {
        this.shouldPause = shouldPause;
    }

    /**
     * @param parameters
     * @param shouldGroup
     * @return
     */
    public String getQuery(@Nullable QueryParameters parameters, boolean shouldGroup) {

        this.parameters = parameters;
        this.shouldGroup = shouldGroup;

        // Reset
        columns = new ArrayList<>();
        conditions = new ArrayList<>();

        String query = select() + where() + group() + order() + limit();

        query += ";";
        dataSource.getLog().debug(query);
        return query;

    }

    /**
     *
     */
    protected String select() {
        return "";
    }

    /**
     *
     */
    protected String where() {
        return "";
    }

    /**
     *
     */
    protected String group() {
        return "";
    }

    /**
     *
     */
    protected String order() {
        return "";
    }

    /**
     *
     */
    protected String limit() {
        return "";
    }

    /**
     * @param condition
     */
    protected void addCondition(String condition) {
        if (!condition.isEmpty()) {
            conditions.add(condition);
        }
    }
}