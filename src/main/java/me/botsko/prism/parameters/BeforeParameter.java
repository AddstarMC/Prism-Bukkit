package me.botsko.prism.parameters;

import java.util.regex.Matcher;

import org.bukkit.command.CommandSender;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.utils.DateUtil;

public class BeforeParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 */
	public void process( QueryParameters query, Matcher input, CommandSender sender ){
		Long date = DateUtil.translateTimeStringToDate(input.group(2));
		if(date != null){
			query.setBeforeTime( date );
		} else {
			throw new IllegalArgumentException("Date/time for 'before' parameter value not recognized. Try /pr ? for help");
		}
	}
}