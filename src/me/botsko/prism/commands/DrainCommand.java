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
		if(call.getArgs().length == 2){
			if(TypeUtils.isNumeric(call.getArg(1))){
				int _tmp_radius = Integer.parseInt(call.getArg(1));
				if(_tmp_radius > 0){
					radius = _tmp_radius;
				} else {
					call.getPlayer().sendMessage( plugin.playerError("Radius must be greater than zero. Or leave it off to use the default. Use /prism ? for help.") );
					return;
				}
			} else {
				call.getPlayer().sendMessage( plugin.playerError("Radius must be a number. Or leave it off to use the default. Use /prism ? for help.") );
				return;
			}
		}
		
		int changed = BlockUtils.drain(call.getPlayer().getLocation(), radius);
		if(changed > 0){
			call.getPlayer().sendMessage(plugin.playerHeaderMsg("Drained nearby liquids."));
		} else {
			call.getPlayer().sendMessage(plugin.playerError("Nothing found to drain with that radius."));
		}
	}
}