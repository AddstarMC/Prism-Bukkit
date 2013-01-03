package me.botsko.prism.api;

import org.bukkit.Location;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;

public class Api {
	
	private static Prism plugin;
	
	public Api(Prism plugin){
		Api.plugin = plugin;
	}
	
	public static QueryResult getBlockChanges(Location loc){
		
		QueryParameters params = new QueryParameters();
		params.setLoc(loc);
		
		ActionsQuery query = new ActionsQuery(plugin);
		return query.lookup(params);
		
	}

}
