package me.botsko.prism.parameters;

import me.botsko.prism.actionlibs.QueryParameters;
import org.bukkit.command.CommandSender;
import org.bukkit.permissions.Permissible;

import java.util.List;

public interface PrismParameterHandler {

    public String getName();

    public String[] getHelp();

    public boolean applicable(String parameter, CommandSender sender);

    public void process(QueryParameters query, String parameter, CommandSender sender);

    public void defaultTo(QueryParameters query, CommandSender sender);

    /**
     * Complete a param after the `:`
     * 
     * @param partialParameter
     *            The partial parameter
     * @param sender
     *            The sender
     * @return List of strings with suggestions or null if not applicable
     */
    public List<String> tabComplete(String partialParameter, CommandSender sender);

    public boolean hasPermission( String parameter, Permissible permissible );
}