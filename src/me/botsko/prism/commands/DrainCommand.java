package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.TypeUtils;

public class DrainCommand implements SubHandler {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public DrainCommand(Prism plugin) {
		this.plugin = plugin;
	}
	
	
	/**
	 * Handle the command
	 */
	public void handle(CallInfo call) {
		
		int radius = plugin.getConfig().getInt("default-radius");
		if(call.getArgs().length == 2 && TypeUtils.isNumeric(call.getArg(1))){
			radius = Integer.parseInt(call.getArg(1));
		}
		int changed = BlockUtils.drain(call.getPlayer().getLocation(), radius);
		if(changed > 0){
			call.getPlayer().sendMessage(plugin.playerHeaderMsg("Drained nearby fluids.."));
		} else {
			call.getPlayer().sendMessage(plugin.playerHeaderMsg("Nothing found to drain with that radius."));
		}
	}
}