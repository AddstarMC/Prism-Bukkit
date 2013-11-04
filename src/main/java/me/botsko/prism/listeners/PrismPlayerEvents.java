package me.botsko.prism.listeners;

import java.util.List;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.wands.ProfileWand;
import me.botsko.prism.wands.Wand;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.block.Jukebox;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.Action;
import org.bukkit.event.enchantment.EnchantItemEvent;
import org.bukkit.event.inventory.CraftItemEvent;
import org.bukkit.event.player.AsyncPlayerChatEvent;
import org.bukkit.event.player.PlayerBucketEmptyEvent;
import org.bukkit.event.player.PlayerBucketFillEvent;
import org.bukkit.event.player.PlayerCommandPreprocessEvent;
import org.bukkit.event.player.PlayerDropItemEvent;
import org.bukkit.event.player.PlayerExpChangeEvent;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.event.player.PlayerInteractEvent;
import org.bukkit.event.player.PlayerJoinEvent;
import org.bukkit.event.player.PlayerPickupItemEvent;
import org.bukkit.event.player.PlayerQuitEvent;
import org.bukkit.event.player.PlayerTeleportEvent;
import org.bukkit.event.player.PlayerTeleportEvent.TeleportCause;
import org.bukkit.inventory.ItemStack;

public class PrismPlayerEvents implements Listener {
	
	/**
	 * 
	 */
	private Prism plugin;
	
	/**
	 * 
	 */
	private List<String> illegalCommands;
	
	/**
	 * 
	 */
	private List<String> ignoreCommands;
	
	
	/**
	 * 
	 * @param plugin
	 */
	@SuppressWarnings("unchecked")
	public PrismPlayerEvents( Prism plugin ){
		this.plugin = plugin;
		illegalCommands = (List<String>) plugin.getConfig().getList("prism.alerts.illegal-commands.commands");
		ignoreCommands = (List<String>) plugin.getConfig().getList("prism.do-not-track.commands");
	}
	
	
	 /**
     * Log command use
     * @param event
     */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onCommandPreprocess(PlayerCommandPreprocessEvent event){
		
		Player player = event.getPlayer();
		String cmd = event.getMessage().toLowerCase();
		
		String[] cmdArgs = cmd.split(" ");
		String primaryCmd = cmdArgs[0].substring(1);
		
		if( plugin.getConfig().getBoolean("prism.alerts.illegal-commands.enabled") ){
			if( illegalCommands.contains( primaryCmd) ){
				String msg = player.getName() + " attempted an illegal command: " + primaryCmd + ". Originally: " + cmd;
				player.sendMessage( Prism.messenger.playerError("Sorry, this command is not available in-game.") );
	        	plugin.alertPlayers( null, msg );
	        	event.setCancelled(true);
	        	// Log to console
				if(plugin.getConfig().getBoolean("prism.alerts.illegal-commands.log-to-console")){
					Prism.log( msg );
				}
			}
		}
		
		if( !Prism.getIgnore().event("player-command",player) ) return;

		// Ignore some commands based on config
		if( ignoreCommands.contains( primaryCmd ) ){
			return;
		}
			
		Prism.actionsRecorder.addToQueue( ActionFactory.create("player-command", player, event.getMessage()) );
		
    }
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerJoin(final PlayerJoinEvent event){

		Player player = event.getPlayer();
		if( !Prism.getIgnore().event("player-join",player) ) return;
		
		String ip = null;
		if(plugin.getConfig().getBoolean("prism.track-player-ip-on-join")){
			ip = player.getAddress().getAddress().getHostAddress().toString();
		}
		
		Prism.actionsRecorder.addToQueue( ActionFactory.create("player-join", event.getPlayer(), ip) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL)
	public void onPlayerQuit(final PlayerQuitEvent event){
		
		if( !Prism.getIgnore().event("player-quit",event.getPlayer()) ) return;
		
		Prism.actionsRecorder.addToQueue( ActionFactory.create("player-quit", event.getPlayer(), null) );

		// Remove any active wands for this player
		if(plugin.playersWithActiveTools.containsKey(event.getPlayer().getName())){
			plugin.playersWithActiveTools.remove(event.getPlayer().getName());
		}
		// Remove any active previews for this player, even though they would expire
		// naturally.
		if(plugin.playerActivePreviews.containsKey(event.getPlayer().getName())){
			plugin.playerActivePreviews.remove(event.getPlayer().getName());
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerChat(final AsyncPlayerChatEvent event){
		
		if( !Prism.getIgnore().event("player-chat",event.getPlayer()) ) return;
		
		if( plugin.dependencyEnabled("Herochat") ) return;
		
		Prism.actionsRecorder.addToQueue( ActionFactory.create("player-chat", event.getPlayer(), event.getMessage()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerDropItem(final PlayerDropItemEvent event) {
		if( !Prism.getIgnore().event("item-drop",event.getPlayer()) ) return;
		Prism.actionsRecorder.addToQueue( ActionFactory.create("item-drop", event.getItemDrop().getItemStack(), event.getItemDrop().getItemStack().getAmount(), -1, null, event.getPlayer().getLocation(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerPickupItem(final PlayerPickupItemEvent event) {
		if( !Prism.getIgnore().event("item-pickup",event.getPlayer()) ) return;
		Prism.actionsRecorder.addToQueue( ActionFactory.create("item-pickup", event.getItem().getItemStack(), event.getItem().getItemStack().getAmount(), -1, null, event.getPlayer().getLocation(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerExpChangeEvent(final PlayerExpChangeEvent event) {
		if( !Prism.getIgnore().event("xp-pickup",event.getPlayer()) ) return;
		Prism.actionsRecorder.addToQueue( ActionFactory.create("xp-pickup", event.getPlayer(), ""+event.getAmount()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerBucketEmpty(final PlayerBucketEmptyEvent event){

		Player player = event.getPlayer();
		String cause = (event.getBucket() == Material.LAVA_BUCKET ? "lava-bucket" : "water-bucket");
		
		if( !Prism.getIgnore().event(cause,player) ) return;
		
		Block spot = event.getBlockClicked().getRelative(event.getBlockFace());
		int newId = (cause.equals("lava-bucket") ? 11 : 9);
		Prism.actionsRecorder.addToQueue( ActionFactory.create(cause, spot.getLocation(), spot.getTypeId(), spot.getData(), newId, (byte)0, player.getName()) );

		if(plugin.getConfig().getBoolean("prism.alerts.uses.lava") && event.getBucket() == Material.LAVA_BUCKET && !player.hasPermission("prism.alerts.use.lavabucket.ignore") && !player.hasPermission("prism.alerts.ignore") ){
			plugin.useMonitor.alertOnItemUse(player,"poured lava");
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerBucketFill(final PlayerBucketFillEvent event){
		
		Player player = event.getPlayer();
		if( !Prism.getIgnore().event("bucket-fill",player) ) return;
		Block spot = event.getBlockClicked().getRelative(event.getBlockFace());
		
		String liquid_type = "milk";
		if( spot.getTypeId() == 8 || spot.getTypeId() == 9 ){
			liquid_type = "water";
		} 
		else if( spot.getTypeId() == 10 || spot.getTypeId() == 11 ){
			liquid_type = "lava";
		}
		
		Handler pa = ActionFactory.create("bucket-fill", player, liquid_type);
		
		// Override the location with the area taken
		pa.setX( spot.getX() );
		pa.setY( spot.getY() );
		pa.setZ( spot.getZ() );

		Prism.actionsRecorder.addToQueue( pa );

	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerTeleport(final PlayerTeleportEvent event){
		if( !Prism.getIgnore().event("player-teleport",event.getPlayer()) ) return;
		TeleportCause c = event.getCause();
		if( c.equals(TeleportCause.END_PORTAL) || c.equals(TeleportCause.NETHER_PORTAL) || c.equals(TeleportCause.ENDER_PEARL) ){
			Prism.actionsRecorder.addToQueue( ActionFactory.create("player-teleport", event.getPlayer(), event.getFrom(), event.getTo(), event.getCause()) );
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEnchantItem(final EnchantItemEvent event) {
		if( !Prism.getIgnore().event("enchant-item",event.getEnchanter()) ) return;
		Player player = event.getEnchanter();
		Prism.actionsRecorder.addToQueue( ActionFactory.create("enchant-item", event.getItem(), event.getEnchantsToAdd(), event.getEnchantBlock().getLocation(), player.getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCraftItem(final CraftItemEvent event) {
		Player player = (Player) event.getWhoClicked();
		if( !Prism.getIgnore().event("craft-item",player) ) return;
		ItemStack item = event.getRecipe().getResult();
		Prism.actionsRecorder.addToQueue( ActionFactory.create("craft-item", item, 1, -1, null, player.getLocation(), player.getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.HIGHEST)
	public void onPlayerInteract(final PlayerInteractEvent event) {
		
		Player player = event.getPlayer();
		Block block = event.getClickedBlock();

		// Are they using a wand (or do we always allow it)
		if( plugin.playersWithActiveTools.containsKey(player.getName()) ){
			
			Wand wand = plugin.playersWithActiveTools.get(player.getName());
			
			// The wand will tell us what to use.
			int item_id = wand.getItemId();
			byte item_subid = wand.getItemSubId();
			
//			Prism.debug("Checking active wand for player, Mode: " + wand.getWandMode() + " Item:" + item_id + ":" + item_subid + " Item in hand:" + player.getItemInHand().getTypeId() + ":" + player.getItemInHand().getDurability());

			// Does the player have such item?
			if(wand != null && player.getItemInHand().getTypeId() == item_id && player.getItemInHand().getDurability() == item_subid){
				
				// Left click is for current block
				if (event.getAction() == Action.LEFT_CLICK_BLOCK) {
					wand.playerLeftClick( player, block );
				}
				// Right click is for relative block on blockface
				// except block placements - those will be handled by the blockplace.
				if ( event.getAction() == Action.RIGHT_CLICK_BLOCK) {
					block = block.getRelative(event.getBlockFace());
					wand.playerRightClick( player, block );
				}
				
				if((event.getAction() == Action.RIGHT_CLICK_BLOCK || event.getAction() == Action.LEFT_CLICK_BLOCK)){
					Prism.debug("Cancelling event for wand use.");
					event.setCancelled(true);
					player.updateInventory();
					return;
				}
			}
		}
		
		
		if( event.isCancelled() ) return;
		
		// Doors, buttons, containers, etc may only be opened with a right-click as of 1.4
		if (block != null && event.getAction() == Action.RIGHT_CLICK_BLOCK){

			String coord_key = null;
			switch (block.getType()){
				// MCPC+ start - removed hardcoded check
				/*case FURNACE:
				case DISPENSER:
				case CHEST:
				case ENDER_CHEST:
				case ENCHANTMENT_TABLE:
				case ANVIL:
				case BREWING_STAND:
				case TRAPPED_CHEST:
				case HOPPER:
				case DROPPER:*/
				// MCPC+ end
				case JUKEBOX:
					recordDiscInsert( block, player );
					break;
				case CAKE_BLOCK:
					recordCakeEat( block, player );
					break;
				case WOODEN_DOOR:
				case TRAP_DOOR:
				case FENCE_GATE:
				case LEVER:
				case STONE_BUTTON:
				case WOOD_BUTTON:
					if( !Prism.getIgnore().event("block-use",player) ) return;
					Prism.actionsRecorder.addToQueue( ActionFactory.create("block-use", block, player.getName()) );
					break;
				case LOG:
					recordCocoaPlantEvent( block, player.getItemInHand(), event.getBlockFace(), player.getName() );
					break;
				case CROPS:
				case GRASS:
				case MELON_STEM:
				case PUMPKIN_STEM:
				case SAPLING:
				case CARROT:
				case POTATO:
					recordBonemealEvent( block, player.getItemInHand(), event.getBlockFace(), player.getName() );
					break;
				case RAILS:
				case DETECTOR_RAIL:
				case POWERED_RAIL:
				case ACTIVATOR_RAIL:
					coord_key = block.getX() + ":" + block.getY() + ":" + block.getZ();
					plugin.preplannedVehiclePlacement.put(coord_key, player.getName());
					break;
				case TNT:
					if(player.getItemInHand().getType().equals(Material.FLINT_AND_STEEL)){
						if( !Prism.getIgnore().event("tnt-prime",player) ) return;
						Prism.actionsRecorder.addToQueue( ActionFactory.create("tnt-prime", "tnt", block, player.getName()) );
					}
					break;
				default:
					// MCPC+ start - handle container access
					if( !Prism.getIgnore().event("container-access",player) ) return;
						Prism.actionsRecorder.addToQueue( ActionFactory.create("container-access", block, player.getName()) );
					// MCPC+ end
					break;
			}
			
			// if they're holding a spawner egg
			if( player.getItemInHand().getType().equals(Material.MONSTER_EGG) ){
				recordMonsterEggUse( block, player.getItemInHand(), player.getName() );
			}
			
			// if they're holding a rocket
			if( player.getItemInHand().getType().equals(Material.FIREWORK) ){
				recordRocketLaunch( block, player.getItemInHand(), event.getBlockFace(), player.getName() );
			}
			
			// if they're holding a boat (why they hell can you put boats on anything...)
			if( player.getItemInHand().getType().equals(Material.BOAT) ){
				coord_key = block.getX() + ":" + (block.getY()+1) + ":" + block.getZ();
				plugin.preplannedVehiclePlacement.put(coord_key, player.getName());
			}
		}
		
		// Punching fire
		if (block != null && event.getAction() == Action.LEFT_CLICK_BLOCK){
			Block above = block.getRelative(BlockFace.UP);
			if( above.getType().equals(Material.FIRE) ){
				Prism.actionsRecorder.addToQueue( ActionFactory.create("block-break", above, player.getName()) );
			}
		}
		
		if( !plugin.getConfig().getBoolean("prism.tracking.crop-trample") ) return;

		if (block != null && event.getAction() == Action.PHYSICAL){
			if(block.getType() == Material.SOIL){ // They are stepping on soil
				if( !Prism.getIgnore().event("crop=trample",player) ) return;
				Prism.actionsRecorder.addToQueue( ActionFactory.create("crop-trample", block.getRelative(BlockFace.UP), player.getName()) );
			}
		}
	}
	
	
	/**
	 * 
	 * @param block
	 * @param inhand
	 * @param player
	 */
	protected void recordCocoaPlantEvent( Block block, ItemStack inhand, BlockFace clickedFace, String player ){
		if( !Prism.getIgnore().event("block-place",block) ) return;
		if(block.getType().equals(Material.LOG) && block.getData() >= 3 && inhand.getTypeId() == 351 && inhand.getDurability() == 3){
			Location newLoc = block.getRelative(clickedFace).getLocation();
			Block actualBlock = block.getWorld().getBlockAt(newLoc);
			// This is a lame way to do this
			BlockAction action = new BlockAction();
			action.setActionType("block-place");
			action.setPlayerName(player);
			action.setX( actualBlock.getX() );
			action.setY( actualBlock.getY() );
			action.setZ( actualBlock.getZ() );
			action.setWorldName(newLoc.getWorld().getName());
			action.setBlockId( 127 );
			action.setBlockSubId( (byte)1 );
			Prism.actionsRecorder.addToQueue( action );
		}
	}
	
	
	/**
	 * 
	 * @param block
	 * @param inhand
	 * @param clickedFace
	 * @param player
	 */
	protected void recordBonemealEvent( Block block, ItemStack inhand, BlockFace clickedFace, String player ){
		if( inhand.getTypeId() == 351 && inhand.getDurability() == 15){
			if( !Prism.getIgnore().event("bonemeal-use",block) ) return;
			Prism.actionsRecorder.addToQueue( ActionFactory.create("bonemeal-use", "bonemeal", block, player) );
		}
	}
	
	
	/**
	 * 
	 * @param block
	 * @param inhand
	 * @param player
	 */
	protected void recordMonsterEggUse( Block block, ItemStack inhand, String player ){
		if( !Prism.getIgnore().event("spawnegg-use",block) ) return;
		Prism.actionsRecorder.addToQueue( ActionFactory.create("spawnegg-use", "monster egg", block, player) );
	}
	
	
	/**
	 * 
	 * @param block
	 * @param inhand
	 * @param clickedFace
	 * @param player
	 */
	protected void recordRocketLaunch( Block block, ItemStack inhand, BlockFace clickedFace, String player ){
		if( !Prism.getIgnore().event("firework-launch",block) ) return;
		Prism.actionsRecorder.addToQueue( ActionFactory.create("firework-launch", inhand, null, block.getLocation(), player) );
	}
	
	
	/**
	 * 
	 * @param block
	 * @param player
	 */
	protected void recordCakeEat( Block block, Player player ){
		if( !Prism.getIgnore().event("cake-eat",block) ) return;
		Prism.actionsRecorder.addToQueue( ActionFactory.create("cake-eat", "cake", block, player.getName()) );
	}
	
	
	/**
	 * 
	 * @param block
	 * @param player
	 */
	protected void recordDiscInsert( Block block, Player player ){
		
		// They have to be holding a record
		if( !player.getItemInHand().getType().isRecord() ) return;

		Jukebox jukebox = (Jukebox) block.getState();
		
		// Do we have a disc inside? This will pop it out
		if( !jukebox.getPlaying().equals(Material.AIR) ){
		
			// Record currently playing disc
			ItemStack i = new ItemStack(jukebox.getPlaying(),1);
			Prism.actionsRecorder.addToQueue( ActionFactory.create("item-remove", i, i.getAmount(), 0, null, block.getLocation(), player.getName()) );
			
		} else {

			// Record the insert
			Prism.actionsRecorder.addToQueue( ActionFactory.create("item-insert", player.getItemInHand(), 1, 0, null, block.getLocation(), player.getName()) );
		
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.HIGHEST)
	public void onPlayerEntityInteract(final PlayerInteractEntityEvent event) {
		
		Player player = event.getPlayer();
		Entity entity = event.getRightClicked();

		// Are they using a wand?
		if(plugin.playersWithActiveTools.containsKey(player.getName())){

			// Pull the wand in use
			Wand wand = plugin.playersWithActiveTools.get(player.getName());
			if( wand != null && wand instanceof ProfileWand ){
				
				wand.playerRightClick( player, entity );
				
				// Always cancel
				event.setCancelled(true);
				
			}
		}
	}
}
