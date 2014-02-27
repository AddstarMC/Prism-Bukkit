package me.botsko.prism.parameters;

import me.botsko.prism.actionlibs.QueryParameters;
import org.bukkit.command.CommandSender;

public interface PrismParameterHandler {
	
	public String getName();

	public String[] getHelp();

	public boolean applicable(QueryParameters query, String parameter, CommandSender sender);

	public void process(QueryParameters query, String parameter, CommandSender sender);

	public void defaultTo( QueryParameters query, CommandSender sender );

}