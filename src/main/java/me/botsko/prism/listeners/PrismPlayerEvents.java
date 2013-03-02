package me.botsko.prism.listeners;

import java.util.List;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.BlockChangeAction;
import me.botsko.prism.actions.CommandAction;
import me.botsko.prism.actions.EntityTravelAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.actions.PlayerAction;
import me.botsko.prism.actions.UseAction;
import me.botsko.prism.wands.ProfileWand;
import me.botsko.prism.wands.Wand;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
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
				player.sendMessage( plugin.messenger.playerError("Sorry, this command has disabled from in-game use.") );
	        	plugin.alertPlayers( null, msg );
	        	event.setCancelled(true);
	        	plugin.log(msg);
			}
		}
		
		if( !plugin.getConfig().getBoolean("prism.tracking.player-command") ) return;

		// Ignore some commands based on config
		if( ignoreCommands.contains( primaryCmd ) ){
			return;
		}
			
		Prism.actionsRecorder.addToQueue( new CommandAction("player-command", event.getMessage(), player.getLocation(), player.getName()) );
		
    }
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerJoin(final PlayerJoinEvent event){
		
		if( !plugin.getConfig().getBoolean("prism.tracking.player-join") ) return;
		
		Player player = event.getPlayer();
		
		String ip = null;
		if(plugin.getConfig().getBoolean("prism.track-player-ip-on-join")){
			ip = player.getAddress().getAddress().getHostAddress().toString();
		}
		
		Prism.actionsRecorder.addToQueue( new PlayerAction("player-join", event.getPlayer(), ip) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.NORMAL)
	public void onPlayerQuit(final PlayerQuitEvent event){
		
		if( !plugin.getConfig().getBoolean("prism.tracking.player-quit") ) return;
		
		Prism.actionsRecorder.addToQueue( new PlayerAction("player-quit", event.getPlayer(), null) );

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
		
		if( !plugin.getConfig().getBoolean("prism.tracking.player-chat") ) return;
		
		if( plugin.dependencyEnabled("Herochat") ) return;
		
		Prism.actionsRecorder.addToQueue( new CommandAction("player-chat", event.getMessage(), event.getPlayer().getLocation(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerDropItem(final PlayerDropItemEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.item-drop") ) return;
		Prism.actionsRecorder.addToQueue( new ItemStackAction("item-drop", event.getItemDrop().getItemStack(), event.getItemDrop().getItemStack().getAmount(), -1, null, event.getPlayer().getLocation(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerPickupItem(final PlayerPickupItemEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.item-pickup") ) return;
		Prism.actionsRecorder.addToQueue( new ItemStackAction("item-pickup", event.getItem().getItemStack(), event.getItem().getItemStack().getAmount(), -1, null, event.getPlayer().getLocation(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerExpChangeEvent(final PlayerExpChangeEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.xp-pickup") ) return;
		Prism.actionsRecorder.addToQueue( new PlayerAction("xp-pickup", event.getPlayer(), ""+event.getAmount()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerBucketEmpty(final PlayerBucketEmptyEvent event){
		
		if( !plugin.getConfig().getBoolean("prism.tracking.water-bucket")
				&& !plugin.getConfig().getBoolean("prism.tracking.lava-bucket")) return;
		
		Player player = event.getPlayer();
		String cause = (event.getBucket() == Material.LAVA_BUCKET ? "lava-bucket" : "water-bucket");
		
		Block spot = event.getBlockClicked().getRelative(event.getBlockFace());
		int newId = (cause.equals("lava-bucket") ? 11 : 9);
		Prism.actionsRecorder.addToQueue( new BlockChangeAction(cause, spot.getLocation(), spot.getTypeId(), spot.getData(), newId, (byte)0, player.getName()) );

		if(plugin.getConfig().getBoolean("prism.alerts.uses.lava") && event.getBucket() == Material.LAVA_BUCKET){
			plugin.useMonitor.alertOnItemUse(player,"poured lava");
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerBucketFill(final PlayerBucketFillEvent event){
		
		if( !plugin.getConfig().getBoolean("prism.tracking.bucket-fill") ) return;
		
		Player player = event.getPlayer();
		Block spot = event.getBlockClicked().getRelative(event.getBlockFace());
		
		String liquid_type = "milk";
		if( spot.getTypeId() == 8 || spot.getTypeId() == 9 ){
			liquid_type = "water";
		} 
		else if( spot.getTypeId() == 10 || spot.getTypeId() == 11 ){
			liquid_type = "lava";
		}
		
		PlayerAction pa = new PlayerAction("bucket-fill", player, liquid_type);
		
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
	public void onPlayerTeleport(final PlayerTeleportEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.player-teleport") ) return;
		TeleportCause c = event.getCause();
		if( c.equals(TeleportCause.END_PORTAL) || c.equals(TeleportCause.NETHER_PORTAL) || c.equals(TeleportCause.ENDER_PEARL) ){
			Prism.actionsRecorder.addToQueue( new EntityTravelAction("player-teleport", event.getPlayer(), event.getFrom(), event.getTo(), event.getCause()) );
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEnchantItem(final EnchantItemEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.enchant-item") ) return;
		Player player = event.getEnchanter();
		Prism.actionsRecorder.addToQueue( new ItemStackAction("enchant-item", event.getItem(), event.getEnchantsToAdd(), event.getEnchantBlock().getLocation(), player.getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCraftItem(final CraftItemEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.craft-item") ) return;
		Player player = (Player) event.getWhoClicked();
		ItemStack item = event.getRecipe().getResult();
		Prism.actionsRecorder.addToQueue( new ItemStackAction("craft-item", item, 1, -1, null, player.getLocation(), player.getName()) );
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
			
//			plugin.debug("Checking active wand for player, Mode: " + wand.getWandMode() + " Item:" + item_id + ":" + item_subid + " Item in hand:" + player.getItemInHand().getTypeId() + ":" + player.getItemInHand().getDurability());

			// Does the player have such item?
			if(wand != null && player.getItemInHand().getTypeId() == item_id && player.getItemInHand().getDurability() == item_subid){
				
				plugin.debug("Wand in use.");
				
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
					plugin.debug("Cancelling event for wand use.");
					event.setCancelled(true);
					return;
				}
			}
		}
		

		// Doors, buttons, containers, etc may only be opened with a right-click as of 1.4
		if (block != null && event.getAction() == Action.RIGHT_CLICK_BLOCK){

			switch (block.getType()){
				case FURNACE:
				case DISPENSER:
				case CHEST:
				case ENDER_CHEST:
				case ANVIL:
				case BREWING_STAND:
					Prism.actionsRecorder.addToQueue( new BlockAction("container-access", block, player.getName()) );
					break;
				case WOODEN_DOOR:
				case TRAP_DOOR:
				case FENCE_GATE:
				case LEVER:
				case STONE_BUTTON:
				case WOOD_BUTTON:
					Prism.actionsRecorder.addToQueue( new BlockAction("block-use", block, player.getName()) );
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
				case TNT:
					if(player.getItemInHand().getType().equals(Material.FLINT_AND_STEEL)){
						Prism.actionsRecorder.addToQueue( new UseAction("tnt-prime", "tnt", block, player.getName()) );
					}
					break;
				default:
					break;
			}
			
			
			// if they're holding a spawner egg
			if( player.getItemInHand().getType().equals(Material.MONSTER_EGG) ){
				recordMonsterEggUse( block, player.getItemInHand(), event.getBlockFace(), player.getName() );
			}
			
		}
		
		if( !plugin.getConfig().getBoolean("prism.tracking.crop-trample") ) return;

		if (block != null && event.getAction() == Action.PHYSICAL){
			if(block.getType() == Material.SOIL){ // They are stepping on soil
				Prism.actionsRecorder.addToQueue( new BlockAction("crop-trample", block.getRelative(BlockFace.UP), player.getName()) );
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
		if(block.getType().equals(Material.LOG) && block.getData() >= 3 && inhand.getTypeId() == 351 && inhand.getDurability() == 3){
			Location newLoc = block.getRelative(clickedFace).getLocation();
			Block actualBlock = block.getWorld().getBlockAt(newLoc);
			// This is a lame way to do this
			BlockAction action = new BlockAction("block-place", null, player);
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
			Prism.actionsRecorder.addToQueue( new UseAction("bonemeal-use", "bonemeal", block, player) );
		}
	}
	
	
	/**
	 * 
	 * @param block
	 * @param inhand
	 * @param clickedFace
	 * @param player
	 */
	protected void recordMonsterEggUse( Block block, ItemStack inhand, BlockFace clickedFace, String player ){
		Prism.actionsRecorder.addToQueue( new UseAction("spawnegg-use", "monster egg", block, player) );
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
