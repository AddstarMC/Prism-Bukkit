package me.botsko.prism.actionlibs;

import me.botsko.prism.actions.Handler;

import java.util.List;

public class QueryResult {

    protected final QueryParameters parameters;
    private final int totalResults;
    protected int page = 1;
    private final List<Handler> actionResults;
    private long queryTime;
    private int perPage = 5;
    private int totalPages = 0;
    private long lastTeleportIndex = 0;

    /**
     * Create a Query Result.
     *
     * @param actions    List of ActionHandlers
     * @param parameters params
     */
    public QueryResult(List<Handler> actions, QueryParameters parameters) {

        this.actionResults = actions;
        this.parameters = parameters;

        setQueryTime();

        // set counts
        totalResults = actionResults.size();
        setPerPage(perPage); // does the total pages calc

    }

    public void setQueryTime() {
        final java.util.Date date = new java.util.Date();
        this.queryTime = date.getTime();
    }

    public List<Handler> getActionResults() {
        return actionResults;
    }

    /**
     * Get a list Action ActionHandlers.
     *
     * @return Handler List.
     */
    public List<Handler> getPaginatedActionResults() {

        int limit = (page * perPage);
        final int offset = (limit - perPage);

        if (offset <= totalResults) {
            if (limit > totalResults) {
                limit = totalResults;
            }
            return actionResults.subList(offset, limit);
        }
        return null;
    }

    /**
     * Get the params.
     *
     * @return the parameters
     */
    public QueryParameters getParameters() {
        return parameters;
    }

    /**
     * The total number of results.
     *
     * @return int
     */
    public int getTotalResults() {
        return totalResults;
    }

    public long getQueryTime() {
        return queryTime;
    }

    public int getPerPage() {
        return perPage;
    }

    void setPerPage(int perPage) {
        this.perPage = perPage;
        totalPages = (int) Math.ceil(((double) totalResults / (double) perPage));
    }

    public long getLastTeleportIndex() {
        return lastTeleportIndex;
    }

    public void setLastTeleportIndex(long index) {
        this.lastTeleportIndex = index;
    }

    public int getIndexOfFirstResult() {
        final int index = (page * perPage) - perPage;
        return index + 1;
    }

    public int getPage() {
        return page;
    }

    public void setPage(int page) {
        this.page = page;
    }

    public int getTotalPages() {
        return totalPages;
    }
}