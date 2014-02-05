package me.botsko.prism.parameters;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.bukkit.command.CommandSender;

import me.botsko.prism.actionlibs.QueryParameters;

public interface PrismParameterHandler {
	
	public String getName();
	
	public String[] getHelp();
	
	public Pattern getArgumentPattern();
	
	public void process( QueryParameters query, Matcher input, CommandSender sender );
	
	public void defaultTo( QueryParameters query, CommandSender sender );

}