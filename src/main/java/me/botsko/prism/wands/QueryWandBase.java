package me.botsko.prism.wands;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import me.botsko.prism.MaterialAliases;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionType;
import me.botsko.prism.actionlibs.MatchRule;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.LevenshteinDistance;
import me.botsko.prism.utils.TypeUtils;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

/**
 * A base class for Wands that use 
 * {@link me.botsko.prism.actionlibs.QueryParameters} and show results.
 * This will allow users to specify parameters when creating the wand
 * and have them saved in the Wand for every time they use it until
 * it is disabled.
 */
public abstract class QueryWandBase extends WandBase {
	
	/**
	 * The parameters that are specified. Whenever we do a search
	 * we can clone this and then add the extra stuff. (Location, etc)
	 */
	protected QueryParameters parameters;
	
	/**
	 * Keep an instance of {@link me.botsko.prism.Prism Prism} to use.
	 */
	protected Prism plugin;
	
	/**
	 * When we initialize the class, make the {@link #parameters}
	 * equal to a fresh QueryParameters.
	 */
	public QueryWandBase(Prism plugin){
		parameters = new QueryParameters();
		this.plugin = plugin;
	}
	
	/**
	 * Set the field {@link #parameters} with the parameters here.
	 * This will be using the stuff in <code>/prism params</code>, except we will
	 * ignore the radius and world because we're only checking a single block.
	 * <b>This is only a slightly modified version of
	 * {@link 
	 * me.botsko.prism.commandlibs.PreprocessArgs#process(me.botsko.prism.Prism,
	 * org.bukkit.command.CommandSender, 
	 * java.lang.String[], 
	 * me.botsko.prism.appliers.PrismProcessType, 
	 * int) 
	 * PreprcessArgs's process(...)}.</b> method. At some point we should try to
	 * combine the two methods, and maybe split up process(...) to make it more
	 * usable in other ways.
	 * @param sender The sender of the command.
	 * @param args The arguments from <code>/prism params</code>.
	 * @param argStart What argument to start on.
	 */
	public boolean setParameters(Player sender, String[] args, int argStart){
		
		ConcurrentHashMap<String,String> foundArgs = new ConcurrentHashMap<String,String>();
		
		if(args != null){

			// Iterate over arguments
			for (int i = argStart; i < args.length; i++){

				String arg = args[i];
				if (arg.isEmpty()) continue;
				
				// Split parameter and values
				String[] argEntry = arg.toLowerCase().split(":");
				String arg_type = argEntry[0];
				String val = arg.contains(":") ? arg.replace(argEntry[0] + ":", "") : argEntry[0];
				
				// Verify we have an arg we can match
				String[] possibleArgs = {"a","t","p","b","e","k","before","since","id","-"}; // Removed "w", "r".
				if(!Arrays.asList(possibleArgs).contains(arg_type) && !arg_type.startsWith("-")){
					
					// We support an alternate player syntax so that people can use the tab-complete
					// feature of minecraft. Using p: prevents it.
					Player autoFillPlayer = plugin.getServer().getPlayer(arg_type);
					if( autoFillPlayer != null ){
						MatchRule match = MatchRule.INCLUDE;
						if(arg_type.startsWith("!")){
							match = MatchRule.EXCLUDE;
						}
						parameters.addPlayerName( arg_type.replace("!", ""), match );
					} else {
						respond( sender, Prism.messenger.playerError( "Unrecognized parameter '"+arg+"' - Using only previous parameters. "+ChatColor.ITALIC+"Keep in mind that using parameter wands ignore radius and world parameters."+ChatColor.RED+" Use /prism ? for help.") );
						parameters = new QueryParameters();
						return false;
					}
				}
				
				// Verify no empty val
				if(val.isEmpty()){
					respond( sender, Prism.messenger.playerError("Can't use empty values for '"+arg+"' - Using only previous parameters. Use /prism ? for help.") );
					return false;
				}
				
				// Officially certify we found a valid argument and value!
				if(Arrays.asList(possibleArgs).contains(arg_type)){
					foundArgs.put(arg_type, val);
					parameters.setFoundArgs(foundArgs);
				}
				
				// Action
				if(arg_type.equals("a")){
					String[] actions = val.split(",");
					if(actions.length > 0){
						for(String action : actions){
							// Find all actions that match the action provided - whether the full name or
							// short name.
							ArrayList<ActionType> actionTypes = Prism.getActionRegistry().getActionsByShortname( action.replace("!", "") );
							if(!actionTypes.isEmpty()){
								for(ActionType actionType : actionTypes){
									
									// Ensure the action allows this process type
//									if( (processType.equals(PrismProcessType.ROLLBACK) && !actionType.canRollback()) || (processType.equals(PrismProcessType.RESTORE) && !actionType.canRestore()) ){
										// @todo this is important information but is too spammy with a:place, because vehicle-place doesn't support a rollback etc
//										respond( sender, Prism.messenger.playerError("Ingoring action '"+actionType.getName()+"' because it doesn't support rollbacks.") );
//										continue;
//									}
									// Commented because we check for this later.
									
									// Check match type
									MatchRule match = MatchRule.INCLUDE;
									if(action.startsWith("!")){
										match = MatchRule.EXCLUDE;
									}
									parameters.addActionType( actionType.getName(), match );
								}
							} else {
								respond( sender, Prism.messenger.playerError("Ignoring action '"+action.replace("!", "")+"' because it's unrecognized. Did you mean '" + LevenshteinDistance.getClosestAction(action) +"'? Type '/prism params' for help.") );
							}
						}
						// If none were valid, we end here.
						if(parameters.getActionTypes().isEmpty()){
							return false;
						}
					}
				}
				
				// Player
				if(arg_type.equals("p")){
					String[] playerNames = val.split(",");
					if(playerNames.length > 0){
						for(String playerName : playerNames){
							MatchRule match = MatchRule.INCLUDE;
							if(playerName.startsWith("!")){
								match = MatchRule.EXCLUDE;
								playerName = playerName.replace("!", "");
							}
							else if(playerName.startsWith("~")){
								match = MatchRule.PARTIAL;
								playerName = playerName.replace("~", "");
							}
							parameters.addPlayerName( playerName, match );
						}
					}
				}
				
				// Entity
				if(arg_type.equals("e")){
					String[] entityNames = val.split(",");
					if(entityNames.length > 0){
						for(String entityName : entityNames){
							MatchRule match = MatchRule.INCLUDE;
							if(entityName.startsWith("!")){
								match = MatchRule.EXCLUDE;
							}
							parameters.addEntity( entityName.replace("!", ""), match );
						}
					}
				}
				
				// Block
				if(arg_type.equals("b")){
					
					String[] blocks = val.split(",");
					
					if(blocks.length > 0){
						for(String b : blocks){
					
							// if user provided id:subid
							if(b.contains(":") && b.length() >= 3){
								String[] ids = b.split(":");
								if(ids.length == 2 && TypeUtils.isNumeric(ids[0]) && TypeUtils.isNumeric(ids[1])){
									parameters.addBlockFilter( Integer.parseInt( ids[0] ), Byte.parseByte( ids[1] ) );
								} else {
									respond( sender, Prism.messenger.playerError("Invalid block filter '"+val+"' - Using only previous parameters. Use /prism ? [command] for help.") );
									return false;
								}
							} else {
								
								// It's id without a subid
								if(TypeUtils.isNumeric(b)){
									parameters.addBlockFilter( Integer.parseInt(b), (byte)0 );
								} else {
									
									// Lookup the item name, get the ids
									MaterialAliases items = plugin.getItems();
									ArrayList<int[]> itemIds = items.getItemIdsByAlias( b );
									if(itemIds.size() > 0){
										for(int[] ids : itemIds){
											if(ids.length == 2){
												// If we really care about the sub id because it's a whole different item
												if(BlockUtils.hasSubitems(ids[0])){
													parameters.addBlockFilter( ids[0], (byte) ids[1] );
												} else {
													parameters.addBlockFilter( ids[0], (byte)0 );
												}
											}
										}
									}
								}
							}
						}
					}
				}
				
				// Time
				if(arg_type.equals("before")){
					String date = PreprocessArgs.translateTimeStringToDate(plugin,sender,val);
					if(date != null){
						parameters.setBeforeTime( date );
					} else {
						return false;
					}
				}
				if( arg_type.equals("since") || arg_type.equals("t") ){
					String date = PreprocessArgs.translateTimeStringToDate(plugin,sender,val);
					if(date != null){
						parameters.setSinceTime( date );
					} else {
						return false;
					}
				}
				
				// Keyword
				if(arg_type.equals("k")){
					parameters.setKeyword( val );
				}
				
				// ID
				if(arg_type.equals("id")){
					
					if(TypeUtils.isNumeric( val )){
						
					} else {
						respond( sender, Prism.messenger.playerError("ID must be a number - Using only previous parameters. Use /prism ? for help.") );
						return false;
					}
					parameters.setId( Integer.parseInt(val) );
				}
				
				// Special Flags
				if(arg_type.startsWith("-")){
					try {
						String[] flagComponents = val.substring(1).split("=");
						Flag flag = Flag.valueOf( flagComponents[0].replace("-", "_").toUpperCase() );
						if(!(parameters.hasFlag(flag))){
								
							parameters.addFlag(flag);
							
							// Flag has a value
							if( flagComponents.length > 1 ){
								if(flag.equals(Flag.PER_PAGE)){
									if(TypeUtils.isNumeric(flagComponents[1])){
										parameters.setPerPage( Integer.parseInt(flagComponents[1]) );
									} else {
										respond( sender, Prism.messenger.playerError("Per-page flag value must be a number - Using only previous parameters. Use /prism ? for help.") );
										return false;
									}
								}
							}
						}
					} catch(IllegalArgumentException ex){
						respond( sender, Prism.messenger.playerError("Unrecognized flag '"+val+"' - Using only previous parameters. Use /prism ? [command] for help.") );
						return false;
					}
				}
			}	
		}
		return true;
	}
	
	/**
	 * For use in {@link #setParameters(org.bukkit.entity.Player, java.lang.String[]) setParameters(...)}
	 * @param sender
	 * @param msg
	 */
	private static void respond( CommandSender sender, String msg ){
		if(sender != null){
			sender.sendMessage(msg);
		} else {
//			System.out.print(msg); // @todo let this output to console, is useful for db purge debugging
		}
	}
	
	/**
	 * Get the {@link #parameters} set from {@link #setParameters}.
	 * @return The wand's {@link me.botsko.prism.actionlibs.QueryParameters}.
	 */
	public QueryParameters getParameters(){
		return parameters;
	}
}
