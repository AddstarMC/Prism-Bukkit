package me.botsko.prism.database.mysql;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map.Entry;
import java.util.Set;

import me.botsko.prism.database.SelectQuery;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.util.Vector;

import me.botsko.prism.utils.IntPair;
import me.botsko.prism.utils.MaterialAliases.MaterialState;
import me.botsko.prism.utils.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.database.QueryBuilder;

public abstract class SelectQueryBuilder extends QueryBuilder implements SelectQuery {

	/**
	 * 
	 * @param plugin
	 */
	public SelectQueryBuilder(Prism plugin) {
		super(plugin);
	}

	/**
	 * 
	 * @return
	 */
	@Override
	protected abstract String select();

	/**
	 * 
	 * @return
	 */
	@Override
	protected abstract String where() ;

	/**
	 * 
	 */
	protected abstract void worldCondition() ;

	/**
	 * 
	 */
	protected abstract void actionCondition();

	/**
	 * 
	 */
	protected abstract void playerCondition();

	/**
	 * 
	 */
	protected void radiusCondition() {
		buildRadiusCondition(parameters.getMinLocation(), parameters.getMaxLocation());
	}

	/**
	 * 
	 */
	protected abstract void blockCondition() ;

	/**
	 * 
	 */
	protected abstract void entityCondition() ;
	/**
	 * 
	 */
	protected abstract void timeCondition() ;

	/**
	 * 
	 */
	protected abstract void keywordCondition() ;

	/**
	 * 
	 */
	protected abstract void coordinateCondition();

	/**
	 * 
	 * @return
	 */
	protected abstract String buildWhereConditions();

	/**
	 * 
	 * @return
	 */
	@Override
	protected abstract String group();

	/**
	 * 
	 * @return
	 */
	@Override
	protected abstract String order() ;

	/**
	 * 
	 * @return
	 */
	@Override
	protected abstract  String limit();

	/**
	 * 
	 * @param origValues
	 * @param field_name
	 * @return
	 */
	protected abstract String buildMultipleConditions(HashMap<String, MatchRule> origValues, String field_name, String format);

	/**
	 * 
	 * @param fieldname
	 * @param arg_values
	 * @return
	 */
	protected abstract String buildGroupConditions(String fieldname, String[] arg_values, String matchFormat, String matchType,
			String dataFormat) ;

	/**
	 * 
	 * @param minLoc
	 * @param maxLoc
	 * @return
	 */
	protected abstract void buildRadiusCondition(Vector minLoc, Vector maxLoc) ;

	/**
	 * 
	 * @return
	 */
	protected abstract String buildTimeCondition(Long dateFrom, String equation) ;
}