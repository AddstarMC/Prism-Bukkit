package me.botsko.prism.parameters;

import me.botsko.prism.actionlibs.QueryParameters;
import org.bukkit.command.CommandSender;

import java.util.*;
import java.util.regex.Pattern;

public abstract class SimplePrismParameterHandler implements PrismParameterHandler {
	
	private final String name;
	private final Pattern inputMatcher;
	private final Set<String> aliases;

	
	/**
	 * 
	 * @param name
	 * @param aliases
	 */
	public SimplePrismParameterHandler(String name, String... aliases) {
		this(name, null, aliases);
	}

	
	/**
	 * 
	 * @param name
	 * @param inputMatcher
	 * @param aliases
	 */
	public SimplePrismParameterHandler(String name, Pattern inputMatcher, String... aliases) {
		this.name = name;
		this.inputMatcher = inputMatcher;
		// Set aliases to name + aliases
		this.aliases = new HashSet<String>(Arrays.asList(aliases));
		if( this.aliases.isEmpty() ){
			this.aliases.add( this.name.toLowerCase() );
		}
	}

	
	/**
	 * 
	 */
	@Override
	public final String getName() {
		return name;
	}

	
	/**
	 * 
	 */
	@Override
	public String[] getHelp() {
		return new String[0];
	}

	
	/**
	 * 
	 * @param query
	 * @param alias
	 * @param input
	 * @param sender
	 */
	protected abstract void process(QueryParameters query, String alias, String input, CommandSender sender);

	
	/**
	 * 
	 */
	@Override
	public final void process(QueryParameters query, String parameter, CommandSender sender) {
		// Should never fail, applicable is called first
		String[] split = parameter.split(":", 2);
		String alias = split[0];
		String input = split[1];
		if(inputMatcher != null && !inputMatcher.matcher(input).matches()) {
			throw new IllegalArgumentException("Invalid syntax for parameter " + input);
		}
		process(query, alias, input, sender);
	}

	
	/**
	 * 
	 */
	@Override
	public final boolean applicable(String parameter, CommandSender sender) {
		String[] split = parameter.split(":", 2);
		if(split.length != 2)
			return false;
		String alias = split[0];
		return aliases.contains(alias);
	}

	
	/**
	 * 
	 */
	@Override
	public void defaultTo(QueryParameters query, CommandSender sender) {

	}

    @Override
    public final List<String> tabComplete(String partialParameter, CommandSender sender) {
        // Should never fail, applicable is called first
        String[] split = partialParameter.split(":", 2);
        String alias = split[0];
        String input = split[1];
        List<String> completions = tabComplete(alias, input, sender);
        List<String> edited = new ArrayList<String>(completions.size());
        for (String completion : completions) {
            edited.add(alias + ":" + completion);
        }
        return edited;
    }

    protected List<String> tabComplete(String alias, String partialParameter, CommandSender sender) {
        return null;
    }
}
