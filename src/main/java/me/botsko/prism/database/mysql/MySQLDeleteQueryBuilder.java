package me.botsko.prism.database.mysql;

import me.botsko.prism.Prism;
import me.botsko.prism.database.DeleteQuery;

public class MySQLDeleteQueryBuilder extends MySQLSelectQueryBuilder implements DeleteQuery {

	/**
	 * 
	 * @param plugin
	 */
	public MySQLDeleteQueryBuilder(Prism plugin) {
		super(plugin);
	}

	/**
	 * 
	 */
	@Override
	protected String select() {
		return "DELETE FROM " + tableNameData + " USING " + tableNameData + " LEFT JOIN " + tableNameDataExtra
				+ " ex ON (" + tableNameData + ".id = ex.data_id) ";
	}

	/**
	 * 
	 */
	@Override
	protected String group() {
		return "";
	}

	/**
	 * 
	 */
	@Override
	protected String order() {
		return "";
	}

	/**
	 * 
	 */
	@Override
	protected String limit() {
		return "";
	}
}