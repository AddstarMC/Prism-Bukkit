package me.botsko.prism.appliers;

import java.util.List;

import org.bukkit.command.CommandSender;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actions.Action;

public class Restore extends Preview {
	
	
	/**
	 * 
	 * @param plugin
	 * @return 
	 */
	public Restore( Prism plugin, CommandSender sender, PrismProcessType processType, List<Action> results, QueryParameters parameters ){
		super(plugin, sender, processType, results, parameters);
	}
	
	
	/**
	 * Set preview move and then do a rollback
	 * @return
	 */
	public void preview(){
		is_preview = true;
		apply();
	}
}