package me.botsko.prism.actionlibs;


import me.botsko.prism.database.DeleteQuery;
import me.botsko.prism.database.SelectIDQuery;
import me.botsko.prism.database.SelectProcessActionQuery;
import me.botsko.prism.database.SelectQuery;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.PrismProcessAction;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;

public class ActionsQuery {

	/**
	 * 
	 */
	private final Prism plugin;

	/**
	 * 
	 */
	private final SelectQuery qb;

	/**
	 * 
	 */
	private boolean shouldGroup = false;

	/**
	 * 
	 * @param plugin
	 * @return
	 */
	public ActionsQuery(Prism plugin) {
		this.plugin = plugin;
		this.qb = Prism.getPrismDataSource().createSelectQuery();
	}

	/**
	 * 
	 * @return
	 */
	public QueryResult lookup(QueryParameters parameters) {
		return lookup(parameters, null);
	}

	/**
	 * 
	 * @return
	 */
	public QueryResult lookup(QueryParameters parameters, CommandSender sender) {

		Player player = null;
		if (sender instanceof Player) {
			player = (Player) sender;
		}
		// If lookup, determine if we need to group
		shouldGroup = false;
		if (parameters.getProcessType().equals(PrismProcessType.LOOKUP)) {
			// What to default to
			shouldGroup = plugin.getConfig().getBoolean("prism.queries.lookup-auto-group");
			// Any overriding flags passed?
			if (parameters.hasFlag(Flag.NO_GROUP) || parameters.hasFlag(Flag.EXTENDED)) {
				shouldGroup = false;
			}
		}
		qb.setParameters(parameters);
		qb.setShouldGroup(shouldGroup);
		QueryResult res = qb.executeSelect(plugin.eventTimer);
		// Pull results
		res.setPerPage(parameters.getPerPage());
		// Cache it if we're doing a lookup. Otherwise we don't
		// need a cache.
		if (parameters.getProcessType().equals(PrismProcessType.LOOKUP)) {
			String keyName = "console";
			if (player != null) {
				keyName = player.getName();
			}
			plugin.cachedQueries.remove(keyName);
			plugin.cachedQueries.put(keyName, res);
			// We also need to share these results with the -share-with players.
			for (final CommandSender sharedPlayer : parameters.getSharedPlayers()) {
				plugin.cachedQueries.put(sharedPlayer.getName(), res);
			}
		}

		plugin.eventTimer.recordTimedEvent("results object completed");

		// Return it
		return res;

	}

	/**
	 * 
	 * @param playername
	 */
	public long getUsersLastPrismProcessId(String playername) {
		SelectProcessActionQuery q =  Prism.getPrismDataSource().createProcessQuery();
		QueryParameters parameters = new QueryParameters();
		parameters.setKeyword(playername);
		q.setParameters(parameters);
		q.setShouldGroup(false);
		q.isLastProcessID();
		return  q.getLastProcessIdQuery();
	}

	/**
	 * 
	 * @param id
	 */
	public PrismProcessAction getPrismProcessRecord(long id) {
		SelectProcessActionQuery q =  Prism.getPrismDataSource().createProcessQuery();
		QueryParameters parameters = new QueryParameters();
		parameters.setId(id);
		q.setParameters(parameters);
		q.setShouldGroup(false);
		return q.executeProcessQuery();
	}
	/**
	 * Returns the minimum id found that meets the parameters
	 * @return
	 */
	public long getMinIDForQuery(QueryParameters parameters){
			final SelectIDQuery idQ = Prism.getPrismDataSource().createSelectIDQuery();
			idQ.setMin();
			parameters.setMinPrimaryKey(0);
			parameters.setMaxPrimaryKey(0);
			idQ.setParameters(parameters);
			return idQ.execute();
	}

	/**
	 * Returns the maximum id found that meets the parameters
	 * @return
	 */
	public long getMaxIDForQuery(QueryParameters parameters){
		final SelectIDQuery idQ = Prism.getPrismDataSource().createSelectIDQuery();
		idQ.setMax();
		parameters.setMinPrimaryKey(0);
		parameters.setMaxPrimaryKey(0);
		idQ.setParameters(parameters);
		return idQ.execute();
	}
	/**
	 *
	 * @return
	 */
	public int delete(QueryParameters parameters) {
			final DeleteQuery dqb =  Prism.getPrismDataSource().createDeleteQuery();
			dqb.setParameters(parameters);
			dqb.setShouldGroup(false);//make it clear that we dont want to group for deletes
			return dqb.execute();
	}
}