package me.botsko.prism.commands;

import org.bukkit.ChatColor;

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
		
		String drain_type = "";
		int radius = plugin.getConfig().getInt("prism.default-radius");
		if(call.getArgs().length == 3){
			if( call.getArg(1).equalsIgnoreCase("water") || call.getArg(1).equalsIgnoreCase("lava") ){
				drain_type = call.getArg(1);
			} else {
				call.getPlayer().sendMessage( plugin.playerError("Invalid drain type. Must be lava, water, or left out.") );
				return;
			}
			// Validate radius
			radius = validateRadius( call, call.getArg(2) );
		}
		else if(call.getArgs().length == 2){
			if(TypeUtils.isNumeric(call.getArg(1))){
				radius = validateRadius( call, call.getArg(1) );
			} else {
				if( call.getArg(1).equalsIgnoreCase("water") || call.getArg(1).equalsIgnoreCase("lava") ){
					drain_type = call.getArg(1);
				} else {
					call.getPlayer().sendMessage( plugin.playerError("Invalid drain type. Must be lava, water, or left out.") );
					return;
				}
			}
		}
		
		// Build seeking message
		String msg = "Seeking "+drain_type+" within "+radius+" blocks.";
		if(drain_type.equals("water")){
			msg += ChatColor.GRAY + " It's just too wet.";
		}
		else if(drain_type.equals("lava")){
			msg += ChatColor.GRAY + " It's getting hot in here.";
		}
		call.getPlayer().sendMessage(plugin.playerHeaderMsg(msg));
		
		
		int changed = 0;
		if(drain_type.isEmpty()){
			changed = BlockUtils.drain(call.getPlayer().getLocation(), radius);
		}
		else if(drain_type.equals("water")){
			changed = BlockUtils.drainwater(call.getPlayer().getLocation(), radius);
		}
		else if(drain_type.equals("lava")){
			changed = BlockUtils.drainlava(call.getPlayer().getLocation(), radius);
		}
		
		if(changed > 0){
			// @todo remove the extra space in msg
			call.getPlayer().sendMessage(plugin.playerHeaderMsg("Drained "+changed+" "+drain_type+" blocks."));
		} else {
			call.getPlayer().sendMessage(plugin.playerError("Nothing found to drain with that radius."));
		}
	}
	
	
	/**
	 * 
	 * @param call
	 * @return
	 */
	protected int validateRadius( CallInfo call, String radius_arg ){
		int radius = plugin.getConfig().getInt("prism.default-radius");
		if(TypeUtils.isNumeric(radius_arg)){
			int _tmp_radius = Integer.parseInt(radius_arg);
			if(_tmp_radius > 0){
				radius = _tmp_radius;
			} else {
				call.getPlayer().sendMessage( plugin.playerError("Radius must be greater than zero. Or leave it off to use the default. Use /prism ? for help.") );
				return 0;
			}
		} else {
			call.getPlayer().sendMessage( plugin.playerError("Radius must be a number. Or leave it off to use the default. Use /prism ? for help.") );
			return 0;
		}
		return radius;
	}
}