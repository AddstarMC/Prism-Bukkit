package me.botsko.prism.commands;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Action;
import me.botsko.prism.appliers.Preview;
import me.botsko.prism.appliers.Restore;
import me.botsko.prism.appliers.Rollback;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.TypeUtils;

import org.bukkit.ChatColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.command.Command;
import org.bukkit.command.CommandExecutor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

public class PrismCommandExecutor implements CommandExecutor {
	
	/**
	 * 
	 */
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
	    				
	    				// Process and validate all of the arguments
	    				QueryParameters parameters = preprocessArguments( player, args, "lookup" );
	    				if(parameters == null){
	    					return true;
	    				}
	    				parameters.setLimit(1000); // @todo config this, and move the logic to queryparams
	    			
		    			ActionsQuery aq = new ActionsQuery(plugin);
		    			QueryResult results = aq.lookup( player, parameters );
		    			if(!results.getActionResults().isEmpty()){
		    				player.sendMessage( plugin.playerHeaderMsg("Showing "+results.getTotal_results()+" results. Page 1 of "+results.getTotal_pages()) );
		    				for(Action a : results.getPaginatedActionResults()){
		    					ActionMessage am = new ActionMessage(a);
		    					if(parameters.getAllow_no_radius()){
		    						am.hideId(false);
		    					}
		    					player.sendMessage( plugin.playerMsg( am.getMessage() ) );
		    				}
		    			} else {
		    				player.sendMessage( plugin.playerError( "Nothing found." + ChatColor.GRAY + " Either you're missing something, or we are." ) );
		    			}
	    			} else {
	    				player.sendMessage( plugin.msgNoPermission() );
	    			}
		    			
		    		return true;
	    			
	    		}
	    		
	    		
	    		/**
	    		 * Near
	    		 */
	    		if( args[0].equalsIgnoreCase("near") ){
	    			if( player.hasPermission("prism.*") || player.hasPermission("prism.lookup") ){
	    				
	    				// Build params
	    				QueryParameters parameters = new QueryParameters();
	    				parameters.setWorld( player.getWorld().getName() );
	    				parameters.setPlayer_location(player.getLocation().toVector());
	    				parameters.setRadius(5); // @todo config this
	    				
	    				parameters.setLimit(1000); // @todo config this, and move the logic to queryparams
	    			
		    			ActionsQuery aq = new ActionsQuery(plugin);
		    			QueryResult results = aq.lookup( player, parameters );
		    			if(!results.getActionResults().isEmpty()){
		    				player.sendMessage( plugin.playerHeaderMsg("Showing "+results.getTotal_results()+" results. Page 1 of "+results.getTotal_pages()) );
		    				for(Action a : results.getPaginatedActionResults()){
		    					ActionMessage am = new ActionMessage(a);
		    					if(parameters.getAllow_no_radius()){
		    						am.hideId(false);
		    					}
		    					player.sendMessage( plugin.playerMsg( am.getMessage() ) );
		    				}
		    			} else {
		    				player.sendMessage( plugin.playerError( "Couldn't find any nearby changes." ) );
		    			}
	    			} else {
	    				player.sendMessage( plugin.msgNoPermission() );
	    			}
		    			
		    		return true;
	    			
	    		}
	    		
	    		
	    		/**
	    		 * Paginated lookup
	    		 */
	    		if( args[0].equalsIgnoreCase("page") || args[0].equalsIgnoreCase("p") ){
	    			if( player.hasPermission("prism.*") || player.hasPermission("prism.lookup") ){
	    				
	    				if(args.length != 2){
	    					player.sendMessage( plugin.playerError( "Please specify a page number. Like /prism page 2" ) );
	    					return true;
	    				}
	    				
	    				if(!TypeUtils.isNumeric(args[1])){
	    					player.sendMessage( plugin.playerError( "Page numbers need to actually be numbers. Like /prism page 2" ) );
	    					return true;
	    				}
	    				
	    				int page = Integer.parseInt(args[1]);
	    				
	    				// Is anything even cached?
    					if(plugin.cachedQueries.containsKey(player.getName())){
    						QueryResult results = plugin.cachedQueries.get(player.getName());
    						results.setPage(page);
    						
    						// Results?
    						if(!results.getActionResults().isEmpty()){
    							player.sendMessage( plugin.playerHeaderMsg("Showing "+results.getTotal_results()+" results. Page "+page+" of "+results.getTotal_pages()) );
    		    				for(Action a : results.getPaginatedActionResults()){
    		    					ActionMessage am = new ActionMessage(a);
    		    					if(results.getParameters().getAllow_no_radius()){
    		    						am.hideId(false);
    		    					}
    		    					player.sendMessage( plugin.playerMsg( am.getMessage() ) );
    		    				}
    		    			} else {
    		    				player.sendMessage( plugin.playerError( "Nothing found." + ChatColor.GRAY + " Either you're missing something, or we are." ) );
    		    			}
    					} else {
    						player.sendMessage( plugin.playerError( "There's no lookup results to show. They may have expired." ) );
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
	    		 * Teleport
	    		 */
	    		if( args[0].equalsIgnoreCase("tp") ){
	    			if( player.hasPermission("prism.*") || player.hasPermission("prism.tp") ){
	    				if( args.length == 2 ){
	    					if(TypeUtils.isNumeric(args[1])){
	    						
	    						int record_id = Integer.parseInt(args[1]);
	    						if(record_id > 0){
	    							
	    							// Build params
	    							QueryParameters params = new QueryParameters();
	    							params.setWorld( player.getWorld().getName() );
	    							params.setId(record_id);

	    							// Query
	    							ActionsQuery aq = new ActionsQuery(plugin);
	    							QueryResult results = aq.lookup( player, params );
	    							if(!results.getActionResults().isEmpty()){
	    								for(me.botsko.prism.actions.Action a : results.getActionResults()){
	    									
	    									World world = plugin.getServer().getWorld( a.getWorld_name() );
	    									if(world != null){
		    									Location loc = new Location(world,a.getX(),a.getY(),a.getZ());
//		    									if(Teleport.isSafe(world, loc)){
		    										player.teleport(loc);
//		    									} else {
//		    										player.sendMessage( plugin.playerError( "Doesn't appear to be safe to teleport there." ) );
//		    									}
	    									} else {
	    										player.sendMessage( plugin.playerError( "Action record occurred in world we can't find anymore." ) );
	    									}
	    								}
	    							} else {
	    								player.sendMessage( plugin.playerError( "No records exists with this ID." ) );
	    							}
	    						}
	    					}
	    				}
	    			} else {
	    				player.sendMessage( plugin.msgNoPermission() );
	    			}
		    		return true;
	    		}
	    		
	    		
	    		/**
	    		 * Some utility commands for doing stuff
	    		 */
	    		{
	    		
	    			/**
	    			 * Extinguish fire
	    			 */
	    			if( args[0].equalsIgnoreCase("ex") || args[0].equalsIgnoreCase("extinguish")){
	    				int radius = 10;
	    				if(args.length >= 2 && TypeUtils.isNumeric(args[1])){
	    					radius = Integer.parseInt(args[1]);
	    				}
	    				BlockUtils.extinguish(player.getLocation(), radius);
	    				sender.sendMessage(plugin.playerHeaderMsg("Extinguished nearby fire."));
	    				return true;
	    			}
	    			
	    			/**
	    			 * Drain water or lava
	    			 */
	    			if( args[0].equalsIgnoreCase("dr") || args[0].equalsIgnoreCase("drain")){
	    				int radius = 10;
	    				if(args.length >= 2 && TypeUtils.isNumeric(args[1])){
	    					radius = Integer.parseInt(args[1]);
	    				}
	    				BlockUtils.drain(player.getLocation(), radius);
	    				sender.sendMessage(plugin.playerHeaderMsg("Drained nearby fluids."));
	    				return true;
	    			}
	    		
	    		}
	    		
	    		/**
	    		 * Preview
	    		 */
	    		if( args[0].equalsIgnoreCase("preview") ){
	    			if( player.hasPermission("prism.*") || player.hasPermission("prism.preview") ){
	    				
	    				
	    				// Cancel or Apply
	    				if( args.length == 2 ){
	    					
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
	    				
	    				QueryParameters parameters = preprocessArguments( player, args, "rollback" );
	    				if(parameters == null){
	    					return true;
	    				}
	    			
	    				// Perform preview
		    			ActionsQuery aq = new ActionsQuery(plugin);
		    			QueryResult results = aq.lookup( player, parameters );
		    			if(!results.getActionResults().isEmpty()){
		    				
		    				player.sendMessage( plugin.playerHeaderMsg("Beginning rollback preview...") );
		    				
		    				Preview pv = new Preview( plugin, player, results.getActionResults() );
		    				pv.preview( parameters );
		    				
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
	    				
	    				QueryParameters parameters = preprocessArguments( player, args, "rollback" );
	    				if(parameters == null){
	    					return true;
	    				}
	    			
		    			ActionsQuery aq = new ActionsQuery(plugin);
		    			QueryResult results = aq.lookup( player, parameters );
		    			if(!results.getActionResults().isEmpty()){
		    				
		    				player.sendMessage( plugin.playerHeaderMsg("Beginning rollback...") );
		    				Rollback rb = new Rollback( plugin, player, results.getActionResults() );
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
	    				
	    				QueryParameters parameters = preprocessArguments( player, args, "rollback" );
	    				if(parameters == null){
	    					return true;
	    				}
	    			
		    			ActionsQuery aq = new ActionsQuery(plugin);
		    			QueryResult results = aq.lookup( player, parameters );
		    			if(!results.getActionResults().isEmpty()){
		    				
		    				player.sendMessage( plugin.playerHeaderMsg("Restoring changes...") );
		    				
		    				Restore rs = new Restore( plugin, player, results.getActionResults() );
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
		
		player.sendMessage( plugin.playerHelp("prism (lookup|l) (params)", "Perform a search using (params)"));
		player.sendMessage( plugin.playerHelp("prism (inspect|i)", "Toggles the block/space inspector onto your hand."));
		player.sendMessage( plugin.playerHelp("prism near", "Perform a search for all nearby changes."));
//		player.sendMessage( plugin.playerHelp("prism params", "Lists parameter help."));
		player.sendMessage( plugin.playerHelp("prism preview (params)", "Preview a rollback using (params). Only shows you, doesn't apply rollback."));
		player.sendMessage( plugin.playerHelp("prism preview apply", "Applies the last preview you did to the world."));
		player.sendMessage( plugin.playerHelp("prism preview cancels", "Cancels the last preview you ran."));
		player.sendMessage( plugin.playerHelp("prism rollback (params)", "Reverses changes to the world using (params)"));
		player.sendMessage( plugin.playerHelp("prism restore (params)", "Re-applies changes to the world using (params)"));
		player.sendMessage( plugin.playerHelp("prism ?", "This. Helpception!"));
		player.sendMessage( plugin.playerHelp("prism reload", "Reloads config/language files."));
		
	}
	
	
	/**
	 * 
	 * @param args
	 */
	protected QueryParameters preprocessArguments( Player player, String[] args, String lookup_type ){
		
		QueryParameters parameters = new QueryParameters();
		HashMap<String,String> foundArgs = new HashMap<String,String>();
		
		parameters.setLookup_type(lookup_type);
		
		if(args != null){
		
			// Iterate over arguments
			for (int i = 1; i < args.length; i++) {
				
				String arg = args[i];
				if (arg.isEmpty()) continue;
				
				// Verify they're formatting like a:[val]
				if(!arg.contains(":")){
					player.sendMessage( plugin.playerError("Parameter format error for '"+arg+"'. Use /prism ? for a assitance.") );
					return null;
				}
				if (!arg.substring(1,2).equals(":")) {
					player.sendMessage( plugin.playerError("Misplaced ':' (colon) error for '"+arg+"'. Use /prism ? for a assitance.") );
					return null;
				}
				
				// Split parameter and values
				String arg_type = arg.toLowerCase().substring(0,1);
				String val = arg.toLowerCase().substring(2);
				String[] possibleArgs = {"a","r","t","p","w","b","e"};
				if(Arrays.asList(possibleArgs).contains(arg_type)){
					if(!val.isEmpty()){
						plugin.debug("Found arg type " + arg_type + " with value: " + val);
						foundArgs.put(arg_type, val);
						parameters.setFoundArgs(foundArgs);
					} else {
						player.sendMessage( plugin.playerError("You must supply at least one parameter value. Use /prism ? for a assitance.") );
						return null;
					}
				}
				
				// Action
				if(arg_type.equals("a")){
					parameters.setAction_type( val );
				}
				
				// Player
				if(arg_type.equals("p")){
					parameters.setPlayer( val );
				}
				
				// World
				if(arg_type.equals("w")){
					parameters.setWorld( val );
				}
				
				// Radius
				if(arg_type.equals("r")){
					if(TypeUtils.isNumeric(val)){
						int radius = Integer.parseInt(val);
						if(radius > plugin.getConfig().getInt("prism.max-radius-unless-overridden")){
							radius = plugin.getConfig().getInt("prism.max-radius-unless-overridden");
							player.sendMessage( plugin.playerError("Forcing radius to " + radius + " as allowed by config.") );
						}
						parameters.setRadius( radius );
					} else {
						// User has asked for a global radiu
						if(val.equals("global") && parameters.getLookup_type().equals("lookup")){
							parameters.setAllow_no_radius(true);
						} else {
							player.sendMessage( plugin.playerError("Radius must be a number or 'global'. Use /prism ? for a assitance.") );
							return null;
						}
					}
				}
				
				// Entity
				if(arg_type.equals("e")){
					parameters.setEntity( val );
				}
				
				// Block
				if(arg_type.equals("b")){
					
					String[] blocks = val.split(",");
					
					if(blocks.length > 0){
						
						ArrayList<String> _tmp_vals = new ArrayList<String>();
						
						for(String b : blocks){
					
							// if user provided id:subid
							if(b.contains(":") && b.length() >= 3){
								String _tmp_id = b.substring(0,1);
								String _tmp_subid = b.substring(2);
								if(!TypeUtils.isNumeric(_tmp_id) || !TypeUtils.isNumeric(_tmp_subid)){
									_tmp_vals.add(_tmp_id+":"+_tmp_subid);
								}
							} else {
								
								// It's id without a subid
								if(TypeUtils.isNumeric(b)){
									_tmp_vals.add(b+":0");
								} else {
									
									// Are they using a block name?
									// @todo we need better names. defaults suck
									Material m = Material.getMaterial( b.toUpperCase() );
									if(m != null){
										_tmp_vals.add(m.getId()+":0");
									}
								}
							}
						}
						if(_tmp_vals.size() > 0){
							parameters.setBlock( TypeUtils.join(_tmp_vals, ",") );
						}
					}
				}
				
				// Time
				if(arg_type.equals("t")){
					parameters.setTime( val );
				}
			}
			
			// Validate any required args are set
			if(foundArgs.isEmpty()){
				player.sendMessage( plugin.playerError("You're missing parameters. Use /prism ? for a assitance.") );
				return null;
			}
			
			/**
			 * Set defaults
			 */
			// Radius default
			if(!foundArgs.containsKey("r")){
				if(parameters.getAllow_no_radius()){
					// We'll allow no radius.
				} else {
					plugin.debug("Setting default radius to " + plugin.getConfig().getInt("default-radius"));
					parameters.setRadius( plugin.getConfig().getInt("prism.default-radius") );
				}
			}
			// World default
			if(!foundArgs.containsKey("w")){
				parameters.setWorld( player.getWorld().getName() );
			}
			// Player location
			parameters.setPlayer_location( player.getLocation().toVector() );
		}
		return parameters;
	}
}