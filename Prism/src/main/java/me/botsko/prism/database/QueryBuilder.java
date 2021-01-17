package me.botsko.prism.database;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;

import javax.annotation.Nullable;
import java.util.ArrayList;
import java.util.Collection;

public abstract class QueryBuilder {
    protected final String tableNameData;
    protected final String tableNameDataExtra;
    protected PrismDataSource dataSource;
    protected Collection<String> columns = new ArrayList<>();
    protected Collection<String> conditions = new ArrayList<>();
    protected QueryParameters parameters;
    protected boolean shouldGroup;
    protected String prefix;
    protected boolean shouldPause;

    /**
     * Build a query.
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
     * @param shouldPause boolean
     */
    public void setShouldPause(boolean shouldPause) {
        this.shouldPause = shouldPause;
    }

    /**
     * Get the query.
     *
     * @param parameters  QueryParams.
     * @param shouldGroup if grouped.
     * @return String with query
     */
    public String getQuery(@Nullable QueryParameters parameters, boolean shouldGroup) {

        this.parameters = parameters;
        this.shouldGroup = shouldGroup;

        // Reset
        columns = new ArrayList<>();
        conditions = new ArrayList<>();

        String query = select() + where() + group() + order() + limit();

        query += ";";
        Prism.debug(query);
        return query;

    }

    protected String select() {
        return "";
    }

    protected String where() {
        return "";
    }

    protected String group() {
        return "";
    }

    protected String order() {
        return "";
    }

    protected String limit() {
        return "";
    }

    /**
     * Add a condition.
     * @param condition String.
     */
    protected void addCondition(String condition) {
        if (!condition.isEmpty()) {
            conditions.add(condition);
        }
    }
}