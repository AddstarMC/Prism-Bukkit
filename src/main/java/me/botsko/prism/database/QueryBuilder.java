package me.botsko.prism.database;

import java.util.ArrayList;
import java.util.List;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;

import javax.annotation.Nullable;

abstract public class QueryBuilder {

	protected List<String> columns = new ArrayList<>();
	protected List<String> conditions = new ArrayList<>();

	protected final String tableNameData;
	protected final String tableNameDataExtra;

	public void setParameters(QueryParameters parameters) {
		this.parameters = parameters;
	}

	public void setShouldGroup(boolean shouldGroup) {
		this.shouldGroup = shouldGroup;
	}

	protected QueryParameters parameters;
	protected boolean shouldGroup;
	protected String prefix;
	/**
	 * 
	 */
	public QueryBuilder() {
		prefix = Prism.getPrismDataSource().getPrefix();
		tableNameData = prefix + "data";
		tableNameDataExtra = prefix + "data_extra";
	}

	/**
	 * 
	 * @param parameters
	 * @param shouldGroup
	 * @return
	 */
	public String getQuery(@Nullable QueryParameters parameters, boolean shouldGroup) {

		this.parameters = parameters;
		this.shouldGroup = shouldGroup;

		// Reset
		columns = new ArrayList<String>();
		conditions = new ArrayList<String>();

		String query = select() + where() + group() + order() + limit();

		query += ";";

		if (Prism.config.getBoolean("prism.debug")) {
			Prism.debug(query);
		}

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
	 * 
	 * @param condition
	 */
	protected void addCondition(String condition) {
		if (!condition.isEmpty()) {
			conditions.add(condition);
		}
	}
}