package me.botsko.prism.parameters;

import java.util.regex.Matcher;

import org.bukkit.command.CommandSender;

import me.botsko.prism.actionlibs.QueryParameters;

public interface PrismParameterHandler {
	
	public void process( QueryParameters query, Matcher input, CommandSender sender );

}