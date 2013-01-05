package me.botsko.prism.commands;

import me.botsko.prism.Prism;
import me.botsko.prism.commandlibs.CallInfo;
import me.botsko.prism.commandlibs.SubHandler;
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
				radius = Integer.parseInt(call.getArg(1));
			} else {
				call.getPlayer().sendMessage( plugin.playerError("Radius must be a number. Or leave it off to use the default. Use /prism ? for help.") );
				return;
			}
		}
		
		int changed = BlockUtils.extinguish(call.getPlayer().getLocation(), radius);
		if(changed > 0){
			call.getPlayer().sendMessage(plugin.playerHeaderMsg("Extinguished nearby fire! Cool!"));
		} else {
			call.getPlayer().sendMessage(plugin.playerError("No fired found within that radius to extinguish."));
		}
	}
}