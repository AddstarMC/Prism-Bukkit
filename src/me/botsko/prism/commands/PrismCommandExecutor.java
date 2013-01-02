package me.botsko.prism.commands;

import java.util.List;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.Action;
import me.botsko.prism.actions.ActionMessage;
import me.botsko.prism.actions.ActionsQuery;
import me.botsko.prism.appliers.Preview;
import me.botsko.prism.appliers.Restore;
import me.botsko.prism.appliers.Rollback;

import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

public class PrismCommandExecutor implements CommandExecutor {
	
	private Prism plugin;
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public PrismCommandExecutor(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
     * Handles all of the commands.
     * 
     * 
     * @param sender
     * @param command
     * @param label
     * @param args
     * @return
     */
	public boolean onCommand(CommandSender sender, Command cmd, String commandLabel, String[] args){
		
		// Is a player issue this command?
		Player player = null;
    	if (sender instanceof Player) {
    		player = (Player) sender;
    		
    		
    		/**
    		 * Arguments supported:
    		 * 
    		 * a:[action]
    		 * b:[block-id]:[sub-id]
    		 * p:[player]
    		 * r:[radius|world]
    		 * t:[timeframe]
    		 * w:[world]
    		 * 
    		 */
    		if(args.length >= 1){
    		
    			
	    		/**
	    		 * Lookup
	    		 */
	    		if( args[0].equalsIgnoreCase("lookup") || args[0].equalsIgnoreCase("l") ){
	    			if( player.hasPermission("prism.*") || player.hasPermission("prism.lookup") ){
	    			
		    			ActionsQuery aq = new ActionsQuery(plugin);
		    			List<Action> results = aq.lookup( player, args );
		    			if(!results.isEmpty()){
		    				player.sendMessage( plugin.playerHeaderMsg("Search Results:") );
		    				for(Action a : results){
		    					ActionMessage am = new ActionMessage(a);
		    					player.sendMessage( plugin.playerMsg( am.getMessage() ) );
		    				}
		    			} else {
		    				// @todo no results
		    				player.sendMessage( plugin.playerError( "No results found." ) );
		    			}
	    			} else {
	    				player.sendMessage( plugin.msgNoPermission() );
	    			}
		    			
		    		return true;
	    			
	    		}
	    		
	    		
	    		/**
	    		 * Inspector
	    		 */
	    		if( args[0].equalsIgnoreCase("inspect") || args[0].equalsIgnoreCase("i") ){
	    			if( player.hasPermission("prism.*") || player.hasPermission("prism.lookup") ){
	    				// If already on, turn it off
	    				if(plugin.playersWithActiveTools.contains(player.getName())){
	    					plugin.playersWithActiveTools.remove(player.getName());
	    					player.sendMessage( plugin.playerHeaderMsg("Inspection mode disabled.") );
	    				} else {
	    					plugin.playersWithActiveTools.add(player.getName());
	    					player.sendMessage( plugin.playerHeaderMsg("Inspection mode enabled.") );
	    				}
	    			} else {
	    				player.sendMessage( plugin.msgNoPermission() );
	    			}
		    		return true;
	    		}
	    		
	    		
	    		/**
	    		 * Preview
	    		 */
	    		if( args[0].equalsIgnoreCase("preview") ){
	    			if( player.hasPermission("prism.*") || player.hasPermission("prism.preview") ){
	    				
	    				
	    				// Cancel or Apply
	    				if( args.length == 2){
	    					
	    					// Apply
	    					if(args[1].equalsIgnoreCase("apply") ){
	    						Preview pv = new Preview( plugin, player, null );
	    						pv.apply_preview();
	    						return true;
	    					}
	    					
	    					// Cancel
	    					if(args[1].equalsIgnoreCase("cancel") ){
	    						Preview pv = new Preview( plugin, player, null );
	    						pv.cancel_preview();
	    						return true;
	    					}
	    				}
	    				
	    				
	    				// Ensure user has no current preview
	    				if(plugin.playerActivePreviews.containsKey(player.getName())){
	    					player.sendMessage( plugin.playerError("You have an existing preview pending. Please apply or cancel before moving on.") );
	    					return true;
	    				}
	    			
	    				// Perform preview
		    			ActionsQuery aq = new ActionsQuery(plugin);
		    			List<Action> results = aq.rollback( player, args );
		    			if(!results.isEmpty()){
		    				
		    				player.sendMessage( plugin.playerHeaderMsg("Beginning rollback preview...") );
		    				
		    				Preview pv = new Preview( plugin, player, results );
		    				pv.preview( args );
		    				
		    			} else {
		    				// @todo no results
		    			}
	    			} else {
	    				player.sendMessage( plugin.msgNoPermission() );
	    			}
	    			
	    			return true;
	    			
	    		}
	    		
	    		
	    		/**
	    		 * Rollback
	    		 */
	    		if( args[0].equalsIgnoreCase("rollback") ){
	    			if( player.hasPermission("prism.*") || player.hasPermission("prism.rollback") ){
	    			
		    			ActionsQuery aq = new ActionsQuery(plugin);
		    			List<Action> results = aq.rollback( player, args );
		    			if(!results.isEmpty()){
		    				
		    				player.sendMessage( plugin.playerHeaderMsg("Beginning rollback...") );
		    				Rollback rb = new Rollback( plugin, player, results );
		    				rb.rollback();
		    				
		    			} else {
		    				// @todo no results
		    			}
	    			} else {
	    				player.sendMessage( plugin.msgNoPermission() );
	    			}
	    			
	    			return true;
	    			
	    		}
	    		
	    		
	    		/**
	    		 * Restore
	    		 */
	    		if( args[0].equalsIgnoreCase("restore") ){
	    			if( player.hasPermission("prism.*") || player.hasPermission("prism.restore") ){
	    			
		    			ActionsQuery aq = new ActionsQuery(plugin);
		    			List<Action> results = aq.rollback( player, args );
		    			if(!results.isEmpty()){
		    				
		    				player.sendMessage( plugin.playerHeaderMsg("Restoring changes...") );
		    				
		    				Restore rs = new Restore( plugin, player, results );
		    				rs.restore();
		    				
		    			} else {
		    				// @todo no results
		    			}
	    			} else {
	    				player.sendMessage( plugin.msgNoPermission() );
	    			}
	    			
	    			return true;
	    			
	    		}
	    		
	    		
	    		// Help
    			if(args[0].equalsIgnoreCase("help") || args[0].equalsIgnoreCase("?")){
    				help(player);
    				return true;
    			}
    			
    		} else {
    			// @todo message?
    		}
    	}
    		
    	// Allow certain commands from both players and console
		if(args.length == 1){
			
			/**
			 * Reloads the plugin configuration
			 */
    		if(args[0].equalsIgnoreCase("reload")){
				if ( (sender instanceof Player && (player.hasPermission("prism.*") || player.hasPermission("prism.reload"))) || player == null ){
					plugin.reloadConfig();
					plugin.config = plugin.getConfig();
					sender.sendMessage( plugin.playerMsg("Configuration reloaded successfully.") );
					return true;
				}
			}
		}
		return false;
	}
	
	
	/**
	 * Displays help
	 * @param player
	 */
	protected void help( Player player ){
		
		player.sendMessage( plugin.playerHeaderMsg( "How to use Prism" ) );
		
		player.sendMessage( plugin.playerHelp("/prism (lookup|l) (params)", "Perform a search using (params)"));
		// @todo finish this
		
	}
}