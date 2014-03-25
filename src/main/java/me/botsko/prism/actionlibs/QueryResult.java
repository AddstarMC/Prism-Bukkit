package me.botsko.prism.actionlibs;

import java.util.ArrayList;
import java.util.List;

import me.botsko.prism.actions.Handler;

public class QueryResult {

    /**
	 * 
	 */
    protected List<Handler> actionResults = new ArrayList<Handler>();

    /**
	 * 
	 */
    protected final QueryParameters parameters;

    /**
	 * 
	 */
    protected long queryTime;

    /**
	 * 
	 */
    protected final int total_results;

    /**
	 * 
	 */
    protected int per_page = 5;

    /**
	 * 
	 */
    protected int total_pages = 0;

    /**
	 * 
	 */
    protected int page = 1;

    /**
	 * 
	 */
    protected int lastTeleportIndex = 0;

    /**
     * 
     * @param actions
     */
    public QueryResult(List<Handler> actions, QueryParameters parameters) {

        this.actionResults = actions;
        this.parameters = parameters;

        setQueryTime();

        // set counts
        total_results = actionResults.size();
        setPerPage( per_page ); // does the total pages calc

    }

    /**
	 * 
	 */
    public void setQueryTime() {
        final java.util.Date date = new java.util.Date();
        this.queryTime = date.getTime();
    }

    /**
     * @return the actionResults
     */
    public List<Handler> getActionResults() {
        return actionResults;
    }

    /**
     * @return the actionResults
     */
    public List<Handler> getPaginatedActionResults() {

        int limit = ( page * per_page );
        final int offset = ( limit - per_page );

        if( offset <= total_results ) {
            if( limit > total_results ) {
                limit = total_results;
            }
            return actionResults.subList( offset, limit );
        }
        return null;
    }

    /**
     * @return the parameters
     */
    public QueryParameters getParameters() {
        return parameters;
    }

    /**
     * @return the total_results
     */
    public int getTotalResults() {
        return total_results;
    }

    /**
     * @return the queryTime
     */
    public long getQueryTime() {
        return queryTime;
    }

    /**
     * @return the per_page
     */
    public int getPerPage() {
        return per_page;
    }

    /**
	 * 
	 */
    public int getLastTeleportIndex() {
        return lastTeleportIndex;
    }

    /**
	 * 
	 */
    public int setLastTeleportIndex(int index) {
        return lastTeleportIndex = index;
    }

    /**
     * 
     * @return
     */
    public int getIndexOfFirstResult() {
        final int index = ( page * per_page ) - per_page;
        return index + 1;
    }

    /**
     * @param per_page
     *            the per_page to set
     */
    public void setPerPage(int per_page) {
        this.per_page = per_page;
        total_pages = (int) Math.ceil( ( (double) total_results / (double) per_page ) );
    }

    /**
     * @return the page
     */
    public int getPage() {
        return page;
    }

    /**
     * @param page
     *            the page to set
     */
    public void setPage(int page) {
        this.page = page;
    }

    /**
     * @return the total_pages
     */
    public int getTotal_pages() {
        return total_pages;
    }
}