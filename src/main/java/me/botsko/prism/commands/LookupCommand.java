package me.botsko.prism.commands;

import java.util.ArrayList;
import java.util.List;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionMessage;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.appliers.PrismProcessType;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.Flag;
import me.botsko.prism.commandlibs.PreprocessArgs;
import me.botsko.prism.commandlibs.SubHandler;
import org.bukkit.ChatColor;
import org.bukkit.command.CommandSender;

public class LookupCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public LookupCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle( final CallInfo call) {
		
		// Process and validate all of the arguments
		final QueryParameters parameters = PreprocessArgs.process( plugin, call.getSender(), call.getArgs(), PrismProcessType.LOOKUP, 1 );
		if(parameters == null){
			return;
		}
		
		
		/**
		 * Run the lookup itself in an async task so the lookup query isn't done on the main thread
		 */
		plugin.getServer().getScheduler().runTaskAsynchronously(plugin, new Runnable(){
			public void run(){
				
				// determine if defaults were used
				ArrayList<String> defaultsUsed = parameters.getDefaultsUsed();
				String defaultsReminder = "";
				if(!defaultsUsed.isEmpty()){
					defaultsReminder += "Using defaults:";
					for(String d : defaultsUsed){
						defaultsReminder += " " + d;
					}
				}
				
				ActionsQuery aq = new ActionsQuery(plugin);
				QueryResult results = aq.lookup( parameters, call.getSender() );
				String sharingWithPlayers = "";
				for(String sharee : parameters.getSharedPlayers()){ // Probably not the right word, but whatever, it's just a variable.
					sharingWithPlayers += sharee + ", ";
				}
				sharingWithPlayers = sharingWithPlayers.substring(0, sharingWithPlayers.isEmpty() ? 0 : sharingWithPlayers.length() - 2);
				
				parameters.addSharedPlayer(call.getSender().getName());
				
				for(String playerName : parameters.getSharedPlayers()){
					
					boolean isSender = playerName.equals(call.getSender().getName());
					
					CommandSender player = playerName.equalsIgnoreCase("CONSOLE") ? plugin.getServer().getConsoleSender() : plugin.getServer().getPlayer(playerName);
					if(player == null) continue;
					
					if(!isSender){
						player.sendMessage(Prism.messenger.playerHeaderMsg( ChatColor.YELLOW + "" + ChatColor.ITALIC + call.getSender().getName() + ChatColor.GOLD + " shared these Prism lookup logs with you:" ));
					} else if(!sharingWithPlayers.isEmpty()){
						player.sendMessage(Prism.messenger.playerHeaderMsg(ChatColor.GOLD + "Sharing results with players: " + ChatColor.YELLOW + "" + ChatColor.ITALIC + sharingWithPlayers));
					}
					
					if(!results.getActionResults().isEmpty()){
						player.sendMessage( Prism.messenger.playerHeaderMsg("Showing "+results.getTotal_results()+" results. Page 1 of "+results.getTotal_pages()) );
						if(!defaultsReminder.isEmpty() && isSender){
							player.sendMessage( Prism.messenger.playerSubduedHeaderMsg(defaultsReminder) );
						}
						List<Handler> paginated = results.getPaginatedActionResults();
						if(paginated != null){
							for(Handler a : paginated){
								ActionMessage am = new ActionMessage(a);
								if( parameters.allowsNoRadius() || parameters.hasFlag(Flag.EXTENDED) || plugin.getConfig().getBoolean("prism.messenger.always-show-extended") ){
									am.showExtended();
								}
								player.sendMessage( Prism.messenger.playerMsg( am.getMessage() ) );
							}
						} else {
							player.sendMessage( Prism.messenger.playerError( "Pagination can't find anything. Do you have the right page number?" ) );
						}
					} else {
						if(!defaultsReminder.isEmpty()){
							if(isSender){
								player.sendMessage( Prism.messenger.playerSubduedHeaderMsg(defaultsReminder) );
							}
						}
						if(isSender){
							player.sendMessage( Prism.messenger.playerError( "Nothing found." + ChatColor.GRAY + " Either you're missing something, or we are." ) );
						}
					}
				}
			}
		});
	}
}