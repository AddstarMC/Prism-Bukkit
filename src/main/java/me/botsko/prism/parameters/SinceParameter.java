package me.botsko.prism.parameters;

import org.bukkit.command.CommandSender;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.utils.DateUtil;

public class SinceParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, String input, CommandSender sender ){
		if(input.equalsIgnoreCase("none")){
			query.setIgnoreTime(true);
		} else {
			Long date = DateUtil.translateTimeStringToDate(input);
			if(date != null){
				query.setSinceTime( date );
			} else {
				throw new IllegalArgumentException("Date/time for 'since' parameter value not recognized. Try /pr ? for help");
			}
		}
	}
}