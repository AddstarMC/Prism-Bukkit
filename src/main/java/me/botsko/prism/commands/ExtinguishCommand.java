package me.botsko.prism.commands;

import java.util.ArrayList;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.events.BlockStateChange;
import me.botsko.prism.events.PrismBlocksExtinguishEvent;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.TypeUtils;

public class ExtinguishCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public ExtinguishCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		int radius = plugin.getConfig().getInt("default-radius");
		if(call.getArgs().length == 2){
			if(TypeUtils.isNumeric(call.getArg(1))){
				int _tmp_radius = Integer.parseInt(call.getArg(1));
				if(_tmp_radius > 0){
					radius = _tmp_radius;
				} else {
					call.getPlayer().sendMessage( plugin.messenger.playerError("Radius must be greater than zero. Or leave it off to use the default. Use /prism ? for help.") );
					return;
				}
			} else {
				call.getPlayer().sendMessage( plugin.messenger.playerError("Radius must be a number. Or leave it off to use the default. Use /prism ? for help.") );
				return;
			}
		}
		
		ArrayList<BlockStateChange> blockStateChanges = BlockUtils.extinguish(call.getPlayer().getLocation(), radius);
		if( blockStateChanges != null && !blockStateChanges.isEmpty() ){
			
			call.getPlayer().sendMessage(plugin.messenger.playerHeaderMsg("Extinguished nearby fire! Cool!"));
			
			// Trigger the event
			PrismBlocksExtinguishEvent event = new PrismBlocksExtinguishEvent(blockStateChanges, call.getPlayer(), radius);
			plugin.getServer().getPluginManager().callEvent(event);
			
		} else {
			call.getPlayer().sendMessage(plugin.messenger.playerError("No fired found within that radius to extinguish."));
		}
	}
}