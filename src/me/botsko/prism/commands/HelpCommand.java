package me.botsko.prism.commands;

import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;

public class HelpCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public HelpCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		help(call.getPlayer());
	}
	
	
//	/**
//	 * Display param help
//	 * @param player
//	 */
//	private void helpParams(Player player) {
//		player.sendMessage(plugin.playerHeaderMsg("Prism parameters"));
//		String actions = "";
//		for(ActionType ac : ActionType.values()){
//			actions += ac.name().replace("_", "-").toLowerCase() + ", ";
//		}
//		actions = actions.substring(0, actions.length() - 2);
//		player.sendMessage( plugin.playerHelp("a:[action]", "Restrict a rollback or search to an action. Actions supported: " + actions));
//		player.sendMessage( plugin.playerHelp("r:[radius]", "Restrict a rollback or search to a radius around you.") );
//		player.sendMessage( plugin.playerHelp("w:[world]", "Restrict a rollback or search to a world.") );
//		player.sendMessage( plugin.playerHelp("t:[time]", "Restrict a rollback to a certain amount of time ago. In the format 0s0m0h0d0w") );
//		player.sendMessage( plugin.playerHelp("p:[player]", "Restrict a rollback to a certain player.") );
//		player.sendMessage( plugin.playerHelp("e:[entity]", "Restrict a rollback to a certain entity's name.") );
//		player.sendMessage( plugin.playerHelp("b:[block-id:sub-id]", "Restrict a rollback to a certain block.") );
//		
//		
//	}


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
}