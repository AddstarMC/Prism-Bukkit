package me.botsko.prism.listeners;

import me.botsko.prism.utils.DeathUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.MiscUtils;
import me.botsko.prism.utils.WandUtils;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.entity.*;
import org.bukkit.entity.minecart.PoweredMinecart;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.EntityBlockFormEvent;
import org.bukkit.event.entity.*;
import org.bukkit.event.entity.EntityDamageEvent.DamageCause;
import org.bukkit.event.hanging.HangingBreakByEntityEvent;
import org.bukkit.event.hanging.HangingBreakEvent;
import org.bukkit.event.hanging.HangingBreakEvent.RemoveCause;
import org.bukkit.event.hanging.HangingPlaceEvent;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.event.player.PlayerShearEntityEvent;
import org.bukkit.event.player.PlayerUnleashEntityEvent;
import org.bukkit.inventory.ItemStack;
import org.bukkit.material.Door;
import org.bukkit.potion.PotionEffect;
import org.bukkit.projectiles.ProjectileSource;

import java.util.Collection;
import java.util.HashMap;
import java.util.UUID;

public class PrismEntityEvents implements Listener {

    /**
	 *
	 */
    private final Prism plugin;

    /**
     * 
     * @param plugin
     */
    public PrismEntityEvents(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEntityDamageEvent(final EntityDamageByEntityEvent event) {

        if( !( event.getDamager() instanceof Player ) )
            return;

        final Entity entity = event.getEntity();
        final Player player = (Player) event.getDamager();

        // Cancel the event if a wand is in use
        if( WandUtils.playerUsesWandOnClick( player, entity.getLocation() ) ) {
            event.setCancelled( true );
            return;
        }

        if( entity instanceof ItemFrame ) {
            final ItemFrame frame = (ItemFrame) event.getEntity();
            // Frame is empty but an item is held
            if( !frame.getItem().getType().equals( Material.AIR ) ) {
                if( Prism.getIgnore().event( "item-remove", player ) ) {
                    RecordingQueue.addToQueue( ActionFactory.createItemStack("item-remove", frame.getItem(), 1, 0, null,
                            entity.getLocation(), player) );
                }
            }
        }
    }
    

    private HashMap<UUID, Boolean> chested = new HashMap<>();
    // Spigot (git-Spigot-9b8bba4-bdcc7c7) calls the death event after removing the chest of
    // chested horses, so we have to work around that. Sad.
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void checkChestedHorseDamage(final EntityDamageEvent event) {
    	if(event.getEntity() instanceof ChestedHorse
    			&& event.getFinalDamage() >= ((LivingEntity)event.getEntity()).getHealth()) {
    		chested.put(event.getEntity().getUniqueId(), ((ChestedHorse)event.getEntity()).isCarryingChest());
    	}
    }
    

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEntityDeath(final EntityDeathEvent event) {
    	Boolean isChested = chested.remove(event.getEntity().getUniqueId());
    	boolean oldChested = false;
    	// Set before check
    	if(isChested != null) {
    		oldChested = ((ChestedHorse)event.getEntity()).isCarryingChest();
    		((ChestedHorse)event.getEntity()).setCarryingChest(isChested.booleanValue());
    	}
    	
        final Entity entity = event.getEntity();

        // Mob Death
        if( !( entity instanceof Player ) ) {
            if( entity.getLastDamageCause() instanceof EntityDamageByEntityEvent ) {

                if( entity instanceof ChestedHorse ) {
                    final ChestedHorse horse = (ChestedHorse) entity;
                    if( horse.isCarryingChest() ) {
                        // Log item drops
                        if( Prism.getIgnore().event( "item-drop", entity.getWorld() ) ) {
                            for ( final ItemStack i : horse.getInventory().getContents() ) {
                                if( i == null )
                                    continue;
                                RecordingQueue.addToQueue( ActionFactory.createItemStack("item-drop", i, i.getAmount(), -1,
                                        null, entity.getLocation(), "horse") );
                            }
                        }
                    }
                }

                // Mob killed by player
                final EntityDamageByEntityEvent entityDamageByEntityEvent = (EntityDamageByEntityEvent) entity
                        .getLastDamageCause();
                if( entityDamageByEntityEvent.getDamager() instanceof Player ) {
                    final Player player = (Player) entityDamageByEntityEvent.getDamager();
                    if( !Prism.getIgnore().event( "player-kill", player ) )
                        return;
                    RecordingQueue.addToQueue( ActionFactory.createEntity("player-kill", entity, player) );

                }
                // Mob shot by an arrow from a player
                else if( entityDamageByEntityEvent.getDamager() instanceof Arrow ) {
                    final Arrow arrow = (Arrow) entityDamageByEntityEvent.getDamager();
                    if( arrow.getShooter() instanceof Player ) {

                        final Player player = (Player) arrow.getShooter();
                        if( !Prism.getIgnore().event( "player-kill", player ) )
                            return;
                        RecordingQueue.addToQueue( ActionFactory.createEntity("player-kill", entity, player) );

                    }
                } else {
                    // Mob died by another mob
                    final Entity damager = entityDamageByEntityEvent.getDamager();
                    String name = "unknown";
                    if( damager != null ) {
                        name = damager.getType().name().toLowerCase();
                    }
                    if( name == null )
                        name = "unknown";
                    if( !Prism.getIgnore().event( "entity-kill", entity.getWorld() ) )
                        return;
                    RecordingQueue.addToQueue( ActionFactory.createEntity("entity-kill", entity, name) );
                }
            } else {

                if( !Prism.getIgnore().event( "entity-kill", entity.getWorld() ) )
                    return;

                String killer = "unknown";
                final EntityDamageEvent damage = entity.getLastDamageCause();
                if( damage != null ) {
                    final DamageCause cause = damage.getCause();
                    if( cause != null ) {
                        killer = cause.name().toLowerCase();
                    }
                }

                // Record the death as natural
                RecordingQueue.addToQueue( ActionFactory.createEntity("entity-kill", entity, killer) );

            }
        } else {

            // Determine who died and what the exact cause was
            final Player p = (Player) event.getEntity();
            if( Prism.getIgnore().event( "player-death", p ) ) {
                final String cause = DeathUtils.getCauseNiceName( p );
                String attacker = DeathUtils.getAttackerName( p );
                if( attacker.equals( "pvpwolf" ) ) {
                    final String owner = DeathUtils.getTameWolfOwner( event );
                    attacker = owner + "'s wolf";
                }
                RecordingQueue.addToQueue( ActionFactory.createPlayerDeath("player-death", p, cause, attacker) );
            }

            // Log item drops
            if( Prism.getIgnore().event( "item-drop", p ) ) {
                if( !event.getDrops().isEmpty() ) {
                    for ( final ItemStack i : event.getDrops() ) {
                        RecordingQueue.addToQueue( ActionFactory.createItemStack("item-drop", i, i.getAmount(), -1, null,
                                p.getLocation(), p) );
                    }
                }
            }
        }
        
        // Return to old value
        if(isChested != null) {
    		((ChestedHorse)event.getEntity()).setCarryingChest(oldChested);
    	}
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onCreatureSpawn(final CreatureSpawnEvent event) {
        if( !Prism.getIgnore().event( "entity-spawn", event.getEntity().getWorld() ) )
            return;
        final String reason = event.getSpawnReason().name().toLowerCase().replace( "_", " " );
        if( reason.equals( "natural" ) )
            return;
        RecordingQueue.addToQueue( ActionFactory.createEntity("entity-spawn", event.getEntity(), reason) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEntityTargetEvent(final EntityTargetEvent event) {
        if( !Prism.getIgnore().event( "entity-follow", event.getEntity().getWorld() ) )
            return;
        if( event.getTarget() instanceof Player ) {
            if( event.getEntity().getType().equals( EntityType.CREEPER ) ) {
                final Player player = (Player) event.getTarget();
                RecordingQueue
                        .addToQueue( ActionFactory.createEntity("entity-follow", event.getEntity(), player) );
            }
        }
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerShearEntity(final PlayerShearEntityEvent event) {
        if( !Prism.getIgnore().event( "entity-shear", event.getPlayer() ) )
            return;
        RecordingQueue
                .addToQueue( ActionFactory.createEntity("entity-shear", event.getEntity(), event.getPlayer()) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerInteractEntityEvent(final PlayerInteractEntityEvent event) {

        final Player p = event.getPlayer();
        final Entity e = event.getRightClicked();
        final ItemStack hand = p.getInventory().getItemInMainHand();
        // @todo right clicks should technically follow blockface
        // Cancel the event if a wand is in use
        if( WandUtils.playerUsesWandOnClick( p, e.getLocation() ) ) {
            event.setCancelled( true );
            return;
        }

        if( e instanceof ItemFrame ) {

            final ItemFrame frame = (ItemFrame) e;

            // If held item doesn't equal existing item frame object type
            if( !frame.getItem().getType().equals( Material.AIR ) ) {
                RecordingQueue.addToQueue( ActionFactory.createPlayer("item-rotate", event.getPlayer(), frame.getRotation()
                        .name().toLowerCase()) );
            }

            // Frame is empty but an item is held
            if( frame.getItem().getType().equals( Material.AIR ) && hand != null ) {
                if( Prism.getIgnore().event( "item-insert", p ) ) {
                    RecordingQueue.addToQueue( ActionFactory.createItemStack("item-insert", hand, 1, 0, null,
                            e.getLocation(), p) );
                }
            }
        }

        // if they're holding coal (or charcoal, a subitem) and they click a
        // powered minecart
        if( hand.getType().equals( Material.COAL ) && e instanceof PoweredMinecart ) {
            if( !Prism.getIgnore().event( "item-insert", p ) )
                return;
            RecordingQueue.addToQueue( ActionFactory.createItemStack("item-insert", hand, 1, 0, null,
                    e.getLocation(), p) );
        }

        if( !Prism.getIgnore().event( "entity-dye", p ) )
            return;
        // Only track the event on sheep, when player holds dye
        if( hand.getType() == Material.INK_SACK && e.getType().equals( EntityType.SHEEP ) ) {
            final String newColor = Prism.getItems().getAlias( hand.getType(),
                    (byte) hand.getDurability() );
            RecordingQueue.addToQueue( ActionFactory.createEntity("entity-dye", event.getRightClicked(), event.getPlayer()
                    , newColor) );
        }
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEntityBreakDoor(final EntityBreakDoorEvent event) {
        if( !Prism.getIgnore().event( "entity-break", event.getEntity().getWorld() ) )
            return;
        RecordingQueue.addToQueue( ActionFactory.createBlock("entity-break", event.getBlock(), event.getEntityType()
                .name().toLowerCase()) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerEntityLeash(final PlayerLeashEntityEvent event) {
        if( !Prism.getIgnore().event( "entity-leash", event.getPlayer() ) )
            return;
        RecordingQueue
                .addToQueue( ActionFactory.createEntity("entity-leash", event.getEntity(), event.getPlayer()) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPlayerEntityUnleash(final PlayerUnleashEntityEvent event) {
        if( !Prism.getIgnore().event( "entity-unleash", event.getPlayer() ) )
            return;
        RecordingQueue.addToQueue( ActionFactory.createEntity("entity-unleash", event.getEntity(), event.getPlayer() ) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEntityUnleash(final EntityUnleashEvent event) {
        if( !Prism.getIgnore().event( "entity-unleash" ) )
            return;
        RecordingQueue.addToQueue( ActionFactory.createEntity("entity-unleash", event.getEntity(), event.getReason()
                .toString().toLowerCase()) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onPotionSplashEvent(final PotionSplashEvent event) {

        final ProjectileSource source = event.getPotion().getShooter();

        // Ignore from non-players for the time being
        if( !( source instanceof Player ) )
            return;

        final Player player = (Player) source;

        if( !Prism.getIgnore().event( "potion-splash", player ) )
            return;

        // What type?
        // Right now this won't support anything with multiple effects
        final Collection<PotionEffect> potion = event.getPotion().getEffects();
        String name = "";
        for ( final PotionEffect eff : potion ) {
            name = eff.getType().getName().toLowerCase();
        }

        RecordingQueue.addToQueue( ActionFactory.createPlayer("potion-splash", player, name) );

    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onHangingPlaceEvent(final HangingPlaceEvent event) {
        // Cancel the event if a wand is in use
        if( WandUtils.playerUsesWandOnClick( event.getPlayer(), event.getEntity().getLocation() ) ) {
            event.setCancelled( true );
            return;
        }
        if( !Prism.getIgnore().event( "hangingitem-place", event.getPlayer() ) )
            return;
        RecordingQueue.addToQueue( ActionFactory.createHangingItem("hangingitem-place", event.getEntity(), event.getPlayer() ) );
    }

    /**
     * Hanging items broken by a player fall under the HangingBreakByEntityEvent
     * events. This is merely here to capture cause = physics for when they
     * detach from a block.
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onHangingBreakEvent(final HangingBreakEvent event) {

        // Ignore other causes. Entity cause already handled.
        if( !event.getCause().equals( RemoveCause.PHYSICS ) ) { return; }

        if( !Prism.getIgnore().event( "hangingitem-break", event.getEntity().getWorld() ) )
            return;

        final Hanging e = event.getEntity();

        // Check for planned hanging item breaks
        final String coord_key = e.getLocation().getBlockX() + ":" + e.getLocation().getBlockY() + ":"
                + e.getLocation().getBlockZ();
        
        String value = plugin.preplannedBlockFalls.remove( coord_key );
        UUID uuid = null;
        try{ uuid = UUID.fromString(value); } catch(Exception e2){}
        final Player player = uuid != null ? Bukkit.getPlayer( uuid ) : null;

        // Track the hanging item break
        if(player != null)
        	RecordingQueue.addToQueue( ActionFactory.createHangingItem("hangingitem-break", e, player) );
        else {
        	RecordingQueue.addToQueue( ActionFactory.createHangingItem("hangingitem-break", e, value) );
        }
        	
        plugin.preplannedBlockFalls.remove( coord_key );

        if( !Prism.getIgnore().event( "item-remove", event.getEntity().getWorld() ) )
            return;

        // If an item frame, track it's contents
        if( e instanceof ItemFrame ) {
            final ItemFrame frame = (ItemFrame) e;
            if( frame.getItem() != null ) {
            	if(player != null)
	                RecordingQueue.addToQueue( ActionFactory.createItemStack("item-remove", frame.getItem(), frame.getItem()
	                        .getAmount(), -1, null, e.getLocation(), player) );
            	else
            		RecordingQueue.addToQueue( ActionFactory.createItemStack("item-remove", frame.getItem(), frame.getItem()
	                        .getAmount(), -1, null, e.getLocation(), value) );
            }
        }
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onHangingBreakByEntityEvent(final HangingBreakByEntityEvent event) {

        final Entity entity = event.getEntity();
        final Entity remover = event.getRemover();
        Player player = null;
        if( remover instanceof Player )
            player = (Player) remover;

        // Cancel the event if a wand is in use
        if( player != null && WandUtils.playerUsesWandOnClick( player, event.getEntity().getLocation() ) ) {
            event.setCancelled( true );
            return;
        }

        if( !Prism.getIgnore().event( "hangingitem-break", event.getEntity().getWorld() ) )
            return;

        String breaking_name = remover.getType().name().toLowerCase();
        if( player != null )
        	RecordingQueue.addToQueue( ActionFactory.createHangingItem("hangingitem-break", event.getEntity(), player) );
        else
        	RecordingQueue.addToQueue( ActionFactory.createHangingItem("hangingitem-break", event.getEntity(), breaking_name) );

        if( !Prism.getIgnore().event( "item-remove", event.getEntity().getWorld() ) )
            return;

        // If an item frame, track it's contents
        if( event.getEntity() instanceof ItemFrame ) {
            final ItemFrame frame = (ItemFrame) event.getEntity();
            if( frame.getItem() != null ) {
                RecordingQueue.addToQueue( ActionFactory.createItemStack("item-remove", frame.getItem(), frame.getItem()
                        .getAmount(), -1, null, entity.getLocation(), breaking_name) );
            }
        }
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEntityChangeBlock(final EntityChangeBlockEvent event) {
        final String entity = MiscUtils.getEntityName(event.getEntity());

        // Technically I think that I really should name it "entity-eat" for
        // better consistency and
        // in case other mobs ever are made to eat. But that's not as fun
        Material to = event.getTo();
        Material from = event.getBlock().getType();
        if( from == Material.GRASS && to == Material.DIRT) {
            if( event.getEntityType() != EntityType.SHEEP )
                return;
            if( !Prism.getIgnore().event( "sheep-eat", event.getBlock() ) )
                return;
            RecordingQueue.addToQueue( ActionFactory.createBlock("sheep-eat", event.getBlock(), entity) );
        } else if (to == Material.AIR ^ from == Material.AIR && event.getEntity() instanceof Enderman) {
            if (from == Material.AIR) {
                if (!Prism.getIgnore().event("enderman-place", event.getBlock()))
                    return;
                BlockState state = event.getBlock().getState();
                state.setType(to);
                RecordingQueue.addToQueue(ActionFactory.createBlock("enderman-place", state, entity));
            } else {
                if (!Prism.getIgnore().event("enderman-pickup", event.getBlock()))
                    return;
                final Enderman enderman = (Enderman) event.getEntity();
                if (enderman.getCarriedMaterial() != null) {
                    BlockState state = event.getBlock().getState();
                    state.setData(enderman.getCarriedMaterial());
                    RecordingQueue.addToQueue(ActionFactory.createBlock("enderman-pickup", state, entity));
                }
            }
        } else if (to == Material.AIR && event.getEntity() instanceof Wither) {
            if (!Prism.getIgnore().event("entity-break", event.getBlock()))
                return;
            RecordingQueue.addToQueue(ActionFactory.createBlock("block-break", event.getBlock(), event.getEntityType().name().toLowerCase()));
        }
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEntityBlockForm(final EntityBlockFormEvent event) {
        if( !Prism.getIgnore().event( "entity-form", event.getBlock() ) )
            return;
        final Block block = event.getBlock();
        final Location loc = block.getLocation();
        final BlockState newState = event.getNewState();
        final String entity = event.getEntity().getType().name().toLowerCase();
        
        // TODO: 1.13
        @SuppressWarnings("deprecation")
		byte oldData = block.getData();
        @SuppressWarnings("deprecation")
		byte newData = newState.getData().getData();
        
        RecordingQueue.addToQueue( ActionFactory.createBlockChange("entity-form", loc, block.getType(), oldData,
                newState.getType(), newData, entity) );
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEntityExplodeChangeBlock(final EntityExplodeEvent event) {

        if( event.blockList() == null || event.blockList().isEmpty() )
            return;

        String name;
        String action = "entity-explode";
        if( event.getEntity() != null ) {
            if( event.getEntity() instanceof Creeper ) {
                if( !Prism.getIgnore().event( "creeper-explode", event.getEntity().getWorld() ) )
                    return;
                action = "creeper-explode";
                name = "creeper";
            } else if( event.getEntity() instanceof TNTPrimed ) {
                if( !Prism.getIgnore().event( "tnt-explode", event.getEntity().getWorld() ) )
                    return;
                action = "tnt-explode";
                Entity source = ((TNTPrimed) event.getEntity()).getSource();
                name = followTNTTrail(source);
            } else if( event.getEntity() instanceof EnderDragon ) {
                if( !Prism.getIgnore().event( "dragon-eat", event.getEntity().getWorld() ) )
                    return;
                action = "dragon-eat";
                name = "enderdragon";
            } else {
                if( !Prism.getIgnore().event( "entity-explode", event.getLocation().getWorld() ) )
                    return;
                try {
                    name = event.getEntity().getType().name().toLowerCase().replace( "_", " " );
                    name = name.length() > 15 ? name.substring( 0, 15 ) : name; // I
                                                                                // don't
                                                                                // think
                                                                                // this
                                                                                // can
                                                                                // happen,
                                                                                // but
                                                                                // just
                                                                                // in
                                                                                // case.
                                                                                // Might
                                                                                // look
                                                                                // weird,
                                                                                // but
                                                                                // that's
                                                                                // better
                                                                                // than
                                                                                // breaking
                                                                                // stuff.
                } catch ( final NullPointerException e ) {
                    name = "unknown";
                }
            }
        } else {
            if( !Prism.getIgnore().event( "entity-explode", event.getLocation().getWorld() ) )
                return;
            name = "magic";
        }
        // Also log item-removes from chests that are blown up
        final PrismBlockEvents be = new PrismBlockEvents( plugin );
        for ( Block block : event.blockList() ) {

            // don't bother record upper doors.
            if( BlockUtils.isDoor(block.getType()) ) {
                if( ((Door)block.getState().getData()).isTopHalf() ) {
                    continue;
                }
            }

            // Change handling a bit if it's a long block
            final Block sibling = BlockUtils.getSiblingForDoubleLengthBlock( block );
            if( sibling != null && !block.getType().equals( Material.CHEST )
                    && !block.getType().equals( Material.TRAPPED_CHEST ) ) {
                block = sibling;
            }

            // log items removed from container
            // note: done before the container so a "rewind" for rollback will
            // work properly
            final Block b2 = block;
            final String source = name;
            be.forEachItem(block, (i,s) -> {
            	RecordingQueue.addToQueue( ActionFactory.createItemStack("item-remove", i, i.getAmount(), 0, null,
                        b2.getLocation(), source) );
            });
            //be.logItemRemoveFromDestroyedContainer( name, block );
            RecordingQueue.addToQueue( ActionFactory.createBlock(action, block, source) );
            // look for relationships
            be.logBlockRelationshipsForBlock( source, block );

        }
    }

    private String followTNTTrail(Entity initial) {
        int counter = 10000000;

        while (initial != null) {
            if (initial instanceof Player) {
                return ((Player) initial).getName();
            } else if (initial instanceof TNTPrimed) {
                initial = (((TNTPrimed) initial).getSource());
                if (counter < 0) {
                    Location last = initial.getLocation();
                    plugin.getLogger().warning("TnT chain has exceeded one million, will not continue!");
                    plugin.getLogger().warning("Last Tnt was at " + last.getX() + ", " + last.getY() + ". " + last.getZ() + " in world " + last.getWorld());
                    return "tnt";
                }
                counter--;
            } else {
                return initial.getType().name();
            }
        }

        return "tnt";
    }
}