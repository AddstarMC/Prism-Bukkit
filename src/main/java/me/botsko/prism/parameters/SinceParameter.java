package me.botsko.prism.parameters;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.bukkit.Bukkit;
import org.bukkit.command.CommandSender;
import org.bukkit.configuration.file.FileConfiguration;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.utils.DateUtil;

public class SinceParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 * @return
	 */
	public String getName(){
		return "Since";
	}
	
	
	/**
	 * 
	 * @return
	 */
	public String[] getHelp(){
		return new String[]{};
	}
	
	
	/**
	 * 
	 * @return
	 */
	public Pattern getArgumentPattern(){
		return Pattern.compile("(since|t):([\\w]+)");
	}
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, Matcher input, CommandSender sender ){
		if(input.group(2).equalsIgnoreCase("none")){
			query.setIgnoreTime(true);
		} else {
			Long date = DateUtil.translateTimeStringToDate(input.group(2));
			if(date != null){
				query.setSinceTime( date );
			} else {
				throw new IllegalArgumentException("Date/time for 'since' parameter value not recognized. Try /pr ? for help");
			}
		}
	}
	
	
	/**
	 * 
	 */
	public void defaultTo( QueryParameters query, CommandSender sender ){
		
		if( query.getProcessType().equals(PrismProcessType.DELETE) ) return;
	
		if( !query.getFoundArgs().containsKey("before") && !query.getFoundArgs().containsKey("since") ){
			
			FileConfiguration config = Bukkit.getPluginManager().getPlugin("Prism").getConfig();
			
			Long date = DateUtil.translateTimeStringToDate(config.getString("prism.queries.default-time-since"));
			if(date == 0){
				Prism.log("Error - date range configuration for prism.time-since is not valid");
				date = DateUtil.translateTimeStringToDate("3d");
			}
			query.setSinceTime(date);
			query.addDefaultUsed( "t:" + config.getString("prism.queries.default-time-since") );
		}
	}
}