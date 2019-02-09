package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.Handler;
import me.botsko.prism.players.PlayerIdentification;
import me.botsko.prism.utils.InventoryUtils;
import me.botsko.prism.utils.MiscUtils;
import me.botsko.prism.wands.ProfileWand;
import me.botsko.prism.wands.Wand;

import org.bukkit.DyeColor;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.TreeSpecies;
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
import org.bukkit.event.entity.EntityPickupItemEvent;
import org.bukkit.event.inventory.CraftItemEvent;
import org.bukkit.event.player.*;
import org.bukkit.event.player.PlayerTeleportEvent.TeleportCause;
import org.bukkit.inventory.EquipmentSlot;
import org.bukkit.inventory.ItemStack;
import org.bukkit.material.Colorable;
import org.bukkit.material.Wood;

import java.util.List;

public class PrismPlayerEvents implements Listener {

    /**
	 * 
	 */
    private final Prism plugin;

    /**
	 * 
	 */
    private final List<String> illegalCommands;

    /**
	 * 
	 */
    private final List<String> ignoreCommands;

    /**
     * 
     * @param plugin
     */
    @SuppressWarnings("unchecked")
    public PrismPlayerEvents(Prism plugin) {
        this.plugin = plugin;
        illegalCommands = (List<String>) plugin.getConfig().getList( "prism.alerts.illegal-commands.commands" );
        ignoreCommands = (List<String>) plugin.getConfig().getList( "prism.do-not-track.commands" );
    }

    /**
     * Log command use
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onCommandPreprocess(PlayerCommandPreprocessEvent event) {

        final Player player = event.getPlayer();
        final String cmd = event.getMessage().toLowerCase();

        final String[] cmdArgs = cmd.split( " " );
        final String primaryCmd = cmdArgs[0].substring( 1 );

        if( plugin.getConfig().getBoolean( "prism.alerts.illegal-commands.enabled" ) ) {
            if( illegalCommands.contains( primaryCmd ) ) {
                final String msg = player.getName() + " attempted an illegal command: " + primaryCmd + ". Originally: "
                        + cmd;
                player.sendMessage( Prism.messenger.playerError( "Sorry, this command is not available in-game." ) );
                plugin.alertPlayers( null, msg );
                event.setCancelled( true );
                // Log to console
                if( plugin.getConfig().getBoolean( "prism.alerts.illegal-commands.log-to-console" ) ) {
                    Prism.log( msg );
                }

                // Log to commands
                List<String> commands = plugin.getConfig().getStringList("prism.alerts.illegal-commands.log-commands");
                MiscUtils.dispatchAlert(msg, commands);
            }
        }

        if( !Prism.getIgnore().event( "player-command", player ) )
            return;

        // Ignore some commands based on config
        if( ignoreCommands.contains( primaryCmd ) ) { return; }

        RecordingQueue.addToQueue( ActionFactory.createPlayer("player-command", player, event.getMessage()) );

    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerJoin(final PlayerJoinEvent event) {

        final Player player = event.getPlayer();

        // Lookup player for cache reasons
        PlayerIdentification.cachePrismPlayer( player );

        // Track the join event
        if( !Prism.getIgnore().event( "player-join", player ) )
            return;

        String ip = null;
        if( plugin.getConfig().getBoolean( "prism.track-player-ip-on-join" ) ) {
            ip = player.getAddress().getAddress().getHostAddress().toString();
        }

        RecordingQueue.addToQueue( ActionFactory.createPlayer("player-join", event.getPlayer(), ip) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.NORMAL)
    public void onPlayerQuit(final PlayerQuitEvent event) {

        // Remove from primary key cache
        Prism.prismPlayers.remove( event.getPlayer().getName() );

        // Track player quit
        if( !Prism.getIgnore().event( "player-quit", event.getPlayer() ) )
            return;

        RecordingQueue.addToQueue( ActionFactory.createPlayer("player-quit", event.getPlayer(), null) );

        // Remove any active wands for this player
        if( Prism.playersWithActiveTools.containsKey( event.getPlayer().getName() ) ) {
            Prism.playersWithActiveTools.remove( event.getPlayer().getName() );
        }
        // Remove any active previews for this player, even though they would
        // expire
        // naturally.
        if( plugin.playerActivePreviews.containsKey( event.getPlayer().getName() ) ) {
            plugin.playerActivePreviews.remove( event.getPlayer().getName() );
        }
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerChat(final AsyncPlayerChatEvent event) {

        if( !Prism.getIgnore().event( "player-chat", event.getPlayer() ) )
            return;

        if( plugin.dependencyEnabled( "Herochat" ) )
            return;

        RecordingQueue.addToQueue( ActionFactory.createPlayer("player-chat", event.getPlayer(), event.getMessage()) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerDropItem(final PlayerDropItemEvent event) {
        if( !Prism.getIgnore().event( "item-drop", event.getPlayer() ) )
            return;
        RecordingQueue.addToQueue( ActionFactory.createItemStack("item-drop", event.getItemDrop().getItemStack(), event
                .getItemDrop().getItemStack().getAmount(), -1, null, event.getPlayer().getLocation(), event.getPlayer()) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerPickupItem(final EntityPickupItemEvent event) {
    	if(event.getEntity() instanceof Player) {
    		Player p = (Player) event.getEntity();
	        if( !Prism.getIgnore().event( "item-pickup", p ) )
	            return;
	        RecordingQueue.addToQueue( ActionFactory.createItemStack("item-pickup", event.getItem().getItemStack(), event.getItem()
	                .getItemStack().getAmount(), -1, null, p.getLocation(), p) );
	    	}
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerExpChangeEvent(final PlayerExpChangeEvent event) {
        if( !Prism.getIgnore().event( "xp-pickup", event.getPlayer() ) )
            return;
        RecordingQueue.addToQueue( ActionFactory.createPlayer("xp-pickup", event.getPlayer(), "" + event.getAmount()) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerBucketEmpty(final PlayerBucketEmptyEvent event) {

        final Player player = event.getPlayer();
        final String cause = ( event.getBucket() == Material.LAVA_BUCKET ? "lava-bucket" : "water-bucket" );

        if( !Prism.getIgnore().event( cause, player ) )
            return;

        final Block spot = event.getBlockClicked().getRelative( event.getBlockFace() );
        final Material newMat = ( cause.equals( "lava-bucket" ) ? Material.STATIONARY_LAVA : Material.STATIONARY_WATER );
        
        // TODO: 1.13
        @SuppressWarnings("deprecation")
		byte data = spot.getData();
        
        RecordingQueue.addToQueue( ActionFactory.createBlockChange(cause, spot.getLocation(), spot.getType(), data,
        		newMat, (byte) 0, player) );

        if( plugin.getConfig().getBoolean( "prism.alerts.uses.lava" ) && event.getBucket() == Material.LAVA_BUCKET
                && !player.hasPermission( "prism.alerts.use.lavabucket.ignore" )
                && !player.hasPermission( "prism.alerts.ignore" ) ) {
            plugin.useMonitor.alertOnItemUse( player, "poured lava" );
        }
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerBucketFill(final PlayerBucketFillEvent event) {

        final Player player = event.getPlayer();
        if( !Prism.getIgnore().event( "bucket-fill", player ) )
            return;
        final Block spot = event.getBlockClicked().getRelative( event.getBlockFace() );

        String liquid_type = "milk";
        if( spot.getType() == Material.WATER || spot.getType() == Material.STATIONARY_WATER ) {
            liquid_type = "water";
        } else if( spot.getType() == Material.LAVA || spot.getType() == Material.STATIONARY_LAVA ) {
            liquid_type = "lava";
        }

        final Handler pa = ActionFactory.createPlayer("bucket-fill", player, liquid_type);

        // Override the location with the area taken
        pa.setX( spot.getX() );
        pa.setY( spot.getY() );
        pa.setZ( spot.getZ() );

        RecordingQueue.addToQueue( pa );

    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerTeleport(final PlayerTeleportEvent event) {
        if( !Prism.getIgnore().event( "player-teleport", event.getPlayer() ) )
            return;
        final TeleportCause c = event.getCause();
        if( c.equals( TeleportCause.END_PORTAL ) || c.equals( TeleportCause.NETHER_PORTAL )
                || c.equals( TeleportCause.ENDER_PEARL ) ) {
            RecordingQueue.addToQueue( ActionFactory.createEntityTravel("player-teleport", event.getPlayer(), event.getFrom(),
                    event.getTo(), event.getCause()) );
        }
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEnchantItem(final EnchantItemEvent event) {
        if( !Prism.getIgnore().event( "enchant-item", event.getEnchanter() ) )
            return;
        final Player player = event.getEnchanter();
        RecordingQueue.addToQueue( ActionFactory.createItemStack("enchant-item", event.getItem(), event.getEnchantsToAdd(),
                event.getEnchantBlock().getLocation(), player) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onCraftItem(final CraftItemEvent event) {
        final Player player = (Player) event.getWhoClicked();
        if( !Prism.getIgnore().event( "craft-item", player ) )
            return;
        final ItemStack item = event.getRecipe().getResult();
        RecordingQueue.addToQueue( ActionFactory.createItemStack("craft-item", item, 1, -1, null, player.getLocation(),
                player) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.HIGHEST)
    public void onPlayerInteract(final PlayerInteractEvent event) {

        final Player player = event.getPlayer();
        Block block = event.getClickedBlock();

        ItemStack hand = player.getInventory().getItemInMainHand();

        // Are they using a wand (or do we always allow it)
        if( Prism.playersWithActiveTools.containsKey( player.getName() ) ) {

            final Wand wand = Prism.playersWithActiveTools.get( player.getName() );

            // The wand will tell us what to use.
            final Material item_mat = wand.getItem();
            final byte item_subid = wand.getItemSubId();

            // Prism.debug("Checking active wand for player, Mode: " +
            // wand.getWandMode() + " Item:" + item_id + ":" + item_subid +
            // " Item in hand:" + player.getItemInHand().getTypeId() + ":" +
            // player.getItemInHand().getDurability());

            // In Spigot 1.10 and newer, the durability value of an ItemStack representing
            // an empty hand is 0 instead of -1.
            short itemInHandDurability = hand.getDurability();
            if (hand.getType() == Material.AIR && itemInHandDurability == 0) {
                itemInHandDurability = -1;
            }

            // Does the player have such item?
            if( wand != null && hand.getType() == item_mat
                    && itemInHandDurability == item_subid ) {

                // Left click is for current block
                if( event.getAction() == Action.LEFT_CLICK_BLOCK ) {
                    wand.playerLeftClick( player, block.getLocation() );
                }
                // Right click is for relative block on blockface
                // except block placements - those will be handled by the
                // blockplace.
                if( event.getAction() == Action.RIGHT_CLICK_BLOCK && event.getHand() == EquipmentSlot.HAND ) {
                    block = block.getRelative( event.getBlockFace() );
                    wand.playerRightClick( player, block.getLocation() );
                }

                if( ( event.getAction() == Action.RIGHT_CLICK_BLOCK || event.getAction() == Action.LEFT_CLICK_BLOCK ) ) {
                    Prism.debug( "Cancelling event for wand use." );
                    event.setCancelled( true );
                    InventoryUtils.updateInventory(player);
                    return;
                }
            }
        }

        if( event.isCancelled() )
            return;

        // Doors, buttons, containers, etc may only be opened with a right-click
        // as of 1.4
        if( block != null && event.getAction() == Action.RIGHT_CLICK_BLOCK ) {

            String coord_key;
            switch ( block.getType() ) {
                case FURNACE:
                case DISPENSER:
                case CHEST:
                case ENDER_CHEST:
                case ENCHANTMENT_TABLE:
                case ANVIL:
                case BREWING_STAND:
                case TRAPPED_CHEST:
                case HOPPER:
                case DROPPER:
                    if( !Prism.getIgnore().event( "container-access", player ) )
                        return;
                    RecordingQueue.addToQueue( ActionFactory.createBlock("container-access", block, player) );
                    break;
                case JUKEBOX:
                    recordDiscInsert( block, player );
                    break;
                case CAKE_BLOCK:
                    recordCakeEat( block, player );
                    break;
                case WOODEN_DOOR:
                case ACACIA_DOOR:
                case BIRCH_DOOR:
                case DARK_OAK_DOOR:
                case JUNGLE_DOOR:
                case SPRUCE_DOOR:
                case TRAP_DOOR:
                case FENCE_GATE:
                case LEVER:
                case STONE_BUTTON:
                case WOOD_BUTTON:
                    if( !Prism.getIgnore().event( "block-use", player ) )
                        return;
                    RecordingQueue.addToQueue( ActionFactory.createBlock("block-use", block, player) );
                    break;
                case LOG:
                    recordCocoaPlantEvent( block, hand, event.getBlockFace(), player );
                    break;
                case CROPS:
                case GRASS:
                case MELON_STEM:
                case PUMPKIN_STEM:
                case SAPLING:
                case CARROT:
                case POTATO:
                    recordBonemealEvent( block, hand, event.getBlockFace(), player );
                    break;
                case RAILS:
                case DETECTOR_RAIL:
                case POWERED_RAIL:
                case ACTIVATOR_RAIL:
                    coord_key = block.getX() + ":" + block.getY() + ":" + block.getZ();
                    plugin.preplannedVehiclePlacement.put( coord_key, player.getUniqueId().toString() );
                    break;
                case TNT:
                    if( hand.getType().equals( Material.FLINT_AND_STEEL ) ) {
                        if( !Prism.getIgnore().event( "tnt-prime", player ) )
                            return;
                        RecordingQueue.addToQueue( ActionFactory.createUse("tnt-prime", "tnt", block, player) );
                    }
                    break;
                default:
                    break;
            }

            // if they're holding a spawner egg
            if( hand.getType() == Material.MONSTER_EGG ) {
                recordMonsterEggUse( block, hand, player );
            }

            // if they're holding a rocket
            if( hand.getType() == Material.FIREWORK ) {
                recordRocketLaunch( block, hand, event.getBlockFace(), player );
            }

            // if they're holding a boat (why they hell can you put boats on
            // anything...)
            if( hand.getType() == Material.BOAT ) {
                coord_key = block.getX() + ":" + ( block.getY() + 1 ) + ":" + block.getZ();
                plugin.preplannedVehiclePlacement.put( coord_key, player.getUniqueId().toString() );
            }
        }

        // Punching fire
        if( block != null && event.getAction() == Action.LEFT_CLICK_BLOCK ) {
            final Block above = block.getRelative( BlockFace.UP );
            if( above.getType().equals( Material.FIRE ) ) {
                RecordingQueue.addToQueue( ActionFactory.createBlock("block-break", above, player) );
            }
        }

        if( !plugin.getConfig().getBoolean( "prism.tracking.crop-trample" ) )
            return;

        if( block != null && event.getAction() == Action.PHYSICAL ) {
            if( block.getType() == Material.SOIL ) { // They are stepping on
                                                     // soil
                if( !Prism.getIgnore().event( "crop-trample", player ) )
                    return;
                RecordingQueue.addToQueue( ActionFactory.createBlock("crop-trample", block.getRelative(BlockFace.UP),
                        player) );
            }
        }
    }

    /**
     * 
     * @param block
     * @param inhand
     * @param player
     */
    protected void recordCocoaPlantEvent(Block block, ItemStack inhand, BlockFace clickedFace, Player player) {
        if( !Prism.getIgnore().event( "block-place", block ) )
            return;
        
        if( block.getType() == Material.LOG && inhand.getType() == Material.INK_SACK ) {
        	Wood w = (Wood)block.getState().getData();
        	Colorable c = (Colorable)inhand.getData();
        	
        	if(w.getSpecies() == TreeSpecies.JUNGLE && c.getColor() == DyeColor.BROWN) {
	            final Location newLoc = block.getRelative( clickedFace ).getLocation();
	            final Block actualBlock = block.getWorld().getBlockAt( newLoc );
	            // This is a lame way to do this
	            final BlockAction action = new BlockAction();
	            action.setActionType( "block-place" );
	            action.setPlayer( player );
	            action.setUUID( player.getUniqueId() );
	            action.setX( actualBlock.getX() );
	            action.setY( actualBlock.getY() );
	            action.setZ( actualBlock.getZ() );
	            action.setWorldName( newLoc.getWorld().getName() );
	            action.setBlock( Material.COCOA );
	            action.setBlockSubId( (byte) 1 );
	            RecordingQueue.addToQueue( action );
        	}
        }
    }

    /**
     * 
     * @param block
     * @param inhand
     * @param clickedFace
     * @param player
     */
    protected void recordBonemealEvent(Block block, ItemStack inhand, BlockFace clickedFace, Player player) {
        if( inhand.getType() == Material.INK_SACK && inhand.getDurability() == 15 ) {
            if( !Prism.getIgnore().event( "bonemeal-use", block ) )
                return;
            RecordingQueue.addToQueue( ActionFactory.createUse("bonemeal-use", "bonemeal", block, player) );
        }
    }

    /**
     * 
     * @param block
     * @param inhand
     * @param player
     */
    protected void recordMonsterEggUse(Block block, ItemStack inhand, Player player) {
        if( !Prism.getIgnore().event( "spawnegg-use", block ) )
            return;
        RecordingQueue.addToQueue( ActionFactory.createUse("spawnegg-use", "monster egg", block, player) );
    }

    /**
     * 
     * @param block
     * @param inhand
     * @param clickedFace
     * @param player
     */
    protected void recordRocketLaunch(Block block, ItemStack inhand, BlockFace clickedFace, Player player) {
        if( !Prism.getIgnore().event( "firework-launch", block ) )
            return;
        RecordingQueue
                .addToQueue( ActionFactory.createItemStack("firework-launch", inhand, null, block.getLocation(), player) );
    }

    /**
     * 
     * @param block
     * @param player
     */
    protected void recordCakeEat(Block block, Player player) {
        if( !Prism.getIgnore().event( "cake-eat", block ) )
            return;
        RecordingQueue.addToQueue( ActionFactory.createUse("cake-eat", "cake", block, player) );
    }

    /**
     * 
     * @param block
     * @param player
     */
    protected void recordDiscInsert(Block block, Player player) {
    	ItemStack hand = player.getInventory().getItemInMainHand();
        // They have to be holding a record
        if( !hand.getType().isRecord() )
            return;

        final Jukebox jukebox = (Jukebox) block.getState();

        // Do we have a disc inside? This will pop it out
        if( !jukebox.getPlaying().equals( Material.AIR ) ) {

            // Record currently playing disc
            final ItemStack i = new ItemStack( jukebox.getPlaying(), 1 );
            RecordingQueue.addToQueue( ActionFactory.createItemStack("item-remove", i, i.getAmount(), 0, null,
                    block.getLocation(), player) );

        } else {

            // Record the insert
            RecordingQueue.addToQueue( ActionFactory.createItemStack("item-insert", hand, 1, 0, null,
                    block.getLocation(), player) );

        }
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.HIGHEST)
    public void onPlayerEntityInteract(final PlayerInteractEntityEvent event) {

        final Player player = event.getPlayer();
        final Entity entity = event.getRightClicked();

        // Are they using a wand?
        if( Prism.playersWithActiveTools.containsKey( player.getName() ) ) {

            // Pull the wand in use
            final Wand wand = Prism.playersWithActiveTools.get( player.getName() );
            if( wand != null && wand instanceof ProfileWand ) {

                wand.playerRightClick( player, entity );

                // Always cancel
                event.setCancelled( true );

            }
        }
    }
}
