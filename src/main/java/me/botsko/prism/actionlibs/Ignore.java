package me.botsko.prism.actionlibs;

import java.util.List;

import org.bukkit.GameMode;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.entity.Player;

import me.botsko.prism.Prism;
import me.botsko.prism.utils.TypeUtils;

public class Ignore {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	private final List<String> ignore_players;
	
	/**
	 * 
	 */
	private final List<String> ignore_worlds;
	
	/**
	 * 
	 */
	private final boolean ignore_creative;
	
	
	/**
	 * 
	 * @param plugin
	 */
	@SuppressWarnings("unchecked")
	public Ignore( Prism plugin ){
		this.plugin = plugin;
		ignore_players = (List<String>) plugin.getConfig().getList( "prism.ignore.players" );
		ignore_worlds = (List<String>) plugin.getConfig().getList( "prism.ignore.worlds" );
		ignore_creative = plugin.getConfig().getBoolean( "prism.ignore.players-in-creative" );
	}
	
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	public boolean event( String actionTypeName ){

		// Always track Prism actions - it's mainly internal
		// use anyway.
		if(actionTypeName.contains("prism")){
			return true;
		}
		
		// Should we ignore this action type?
		if( (TypeUtils.subStrOccurences(actionTypeName, "-") == 1 && !plugin.getConfig().getBoolean( "prism.tracking." + actionTypeName )) ){
			return false;
		}

		return true;
	}
	
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	public boolean event( String actionTypeName, World world, String player ){
		
		if( !event( actionTypeName, world ) ){
			return false;
		}

		// Should we ignore this player?
		if(ignore_players != null && ignore_players.contains( player )){
			return false;
		}

		return true;
	}
	
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	public boolean event( String actionTypeName, Player player ){
		
		if( !event( actionTypeName, player.getWorld() ) ){
			return false;
		}

		// Should we ignore this player?
		if(ignore_players != null && ignore_players.contains( player.getName() )){
			return false;
		}
		
		// Should we ignore this player for being in creative?
		if( ignore_creative ){
			if( player.getGameMode().equals(GameMode.CREATIVE) ){
				return false;
			}
		}
		return true;
	}
	
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	public boolean event( String actionTypeName, Block block ){
		
		if( !event( actionTypeName, block.getWorld() ) ){
			return false;
		}
		
		return true;
		
	}
	
	
	/**
	 * 
	 * @param a
	 * @return
	 */
	public boolean event( String actionTypeName, World world ){
		
		// Should we ignore this world?
		if(ignore_worlds != null && ignore_worlds.contains( world.getName() )){
			return false;
		}
		
		return event( actionTypeName );
		
	}
}