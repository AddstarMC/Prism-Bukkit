package me.botsko.prism.parameters;

import me.botsko.prism.actionlibs.QueryParameters;
import org.bukkit.command.CommandSender;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.regex.Pattern;

public abstract class SimplePrismParameterHandler implements PrismParameterHandler {
	private final String name;
	private final Pattern inputMatcher;
	private final Set<String> aliases;

	public SimplePrismParameterHandler(String name, String... aliases) {
		this(name, null, aliases);
	}

	public SimplePrismParameterHandler(String name, Pattern inputMatcher, String... aliases) {
		this.name = name;
		this.inputMatcher = inputMatcher;
		// Set aliases to name + aliases
		this.aliases = new HashSet<String>(Arrays.asList(aliases));
	}

	@Override
	public String getName() {
		return name;
	}

	@Override
	public String[] getHelp() {
		return new String[0];
	}

	protected abstract void process(QueryParameters query, String alias, String input, CommandSender sender);

	@Override
	public void process(QueryParameters query, String parameter, CommandSender sender) {
		// Should never fail, applicable is called first
		String[] split = parameter.split(":", 2);
		String alias = split[0];
		String input = split[1];
		if(inputMatcher != null && !inputMatcher.matcher(input).matches()) {
			throw new IllegalArgumentException("Invalid syntax for parameter " + input);
		}
		process(query, alias, input, sender);
	}

	@Override
	public boolean applicable(QueryParameters query, String parameter, CommandSender sender) {
		String[] split = parameter.split(":", 2);
		if(split.length != 2)
			return false;
		String alias = split[0];
		return aliases.contains(alias);
	}

	@Override
	public void defaultTo(QueryParameters query, CommandSender sender) {

	}
}
