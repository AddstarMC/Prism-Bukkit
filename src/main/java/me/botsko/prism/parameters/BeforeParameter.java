package me.botsko.prism.parameters;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.bukkit.command.CommandSender;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.utils.DateUtil;

public class BeforeParameter implements PrismParameterHandler {
	
	
	/**
	 * 
	 * @return
	 */
	public String getName(){
		return "Before";
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
		return Pattern.compile("(before):([\\w]+)");
	}
	
	
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
	
	
	/**
	 * 
	 */
	public void defaultTo( QueryParameters query, CommandSender sender ){
		return;
	}
}