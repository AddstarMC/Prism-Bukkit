package me.botsko.prism.monitors;

import java.util.ArrayList;

import me.botsko.elixr.TypeUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionsQuery;
import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.actionlibs.QueryResult;

import org.bukkit.GameMode;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.entity.Player;

public class OreMonitor {
	
	/**
	 * 
	 */
	private final int threshhold_max = 100;
	
	/**
	 * 
	 */
	private int threshhold = 1;
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	protected Player player;
	
	/**
	 * 
	 */
	protected Block block;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public OreMonitor( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param player
	 * @param block
	 */
	public void processAlertsFromBlock( final Player player, final Block block ){
		
		if(!plugin.getConfig().getBoolean("prism.alerts.ores.enabled")){
			return;
		}

		if( player.getGameMode().equals(GameMode.CREATIVE) ){
			return;
		}
			
		if(block != null && isWatched(block) && !plugin.alertedBlocks.containsKey( block.getLocation() )){
			
			threshhold = 1;
			
			// identify all ore blocks on same Y axis in x/z direction
			ArrayList<Block> matchingBlocks = new ArrayList<Block>();
			ArrayList<Block> foundores = findNeighborBlocks( block.getType(), block, matchingBlocks );
			if(!foundores.isEmpty()){
				
				// Save the block
				BlockState state = block.getState();
				
				// Set to air to get the light
				block.setType(Material.AIR);
				int light = block.getLightLevel();
				light = (light > 0 ? Math.round(((light) & 0xFF) * 100) / 15 : 0);
				
				// Restore the block
				block.setType( state.getType() );
				
				String count = foundores.size() + (foundores.size() >= threshhold_max ? "+" : "" );
				final String msg = getOreColor(block) + player.getName() + " found " + count + " " + getOreNiceName(block) + " " + light + "% light";
			
				/**
				 * Run the lookup itself in an async task so the lookup query isn't done on the main thread
				 */
				plugin.getServer().getScheduler().runTaskAsynchronously(plugin, new Runnable(){
					public void run(){
			
						// check if block placed
						boolean wasplaced = false;
						
						// Build params
						QueryParameters params = new QueryParameters();
						params.setWorld( player.getWorld().getName() );
						params.addSpecificBlockLocation(block.getLocation());
						params.addActionType("block-place");
						
						ActionsQuery aq = new ActionsQuery(plugin);
						QueryResult results = aq.lookup( params, player );
						if(!results.getActionResults().isEmpty()){
							wasplaced = true;
						}
						
						if(!wasplaced){
								
							// Alert staff
							plugin.alertPlayers( null, TypeUtils.colorize( msg ) );
							
							// Log to console
							if(plugin.getConfig().getBoolean("prism.alerts.ores.log-to-console")){
								Prism.log( msg );
							}
						}
					}
				});
			}
		}
	}
	
	
	/**
	 * 
	 * @param block
	 * @return
	 */
	protected String getOreColor( Block block ){
		if( isWatched( block ) ){
			return Prism.getAlertedOres().get( block.getTypeId() );
		} else {
			return "&f";
		}
	}
	
	
	/**
	 * 
	 * @param block
	 * @return
	 */
	protected String getOreNiceName( Block block ){
		return block.getType().toString().replace("_", " ").toLowerCase().replace("glowing", " ");
	}
	
	
	/**
	 * 
	 * @param block
	 * @return
	 */
	protected boolean isWatched( Block block ){
		if( Prism.getAlertedOres().containsKey( block.getTypeId() + ":" + block.getData() ) || Prism.getAlertedOres().containsKey( block.getTypeId() ) ){
			return true;
		}
		return false;
	}
	
	
	/**
	 * @param currBlock
	 * @param toBeFelled
	 */
    private ArrayList<Block> findNeighborBlocks( Material type, Block currBlock, ArrayList<Block> matchingBlocks ) {

        if(isWatched(currBlock)){

        	matchingBlocks.add(currBlock);
        	java.util.Date date = new java.util.Date();
        	plugin.alertedBlocks.put(currBlock.getLocation(), date.getTime());
        	
        	for(int x = -1; x <= 1; x++){
        		for(int z = -1; z <= 1; z++){
        			for(int y = -1; y <= 1; y++){
	        			Block newblock = currBlock.getRelative(x, y, z);
	        			// ensure it matches the type and wasn't already found
	        			if( newblock.getType() == type && !matchingBlocks.contains(newblock) ){
	        				threshhold++;
	        	        	if( threshhold <= threshhold_max ){
	        	        		findNeighborBlocks( type, newblock, matchingBlocks );
	        	        	}
	        			}
	        		}
        		}
        	}
        }
        
        return matchingBlocks;
        
    }
}
