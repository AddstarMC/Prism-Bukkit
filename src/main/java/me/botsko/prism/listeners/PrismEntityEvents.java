package me.botsko.prism.listeners;

import java.util.Collection;

import me.botsko.elixr.DeathUtils;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.utils.BlockUtils;
import me.botsko.prism.utils.WandUtils;
import me.botsko.prism.wands.Wand;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
import org.bukkit.entity.Arrow;
import org.bukkit.entity.Creeper;
import org.bukkit.entity.EnderDragon;
import org.bukkit.entity.Enderman;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Hanging;
import org.bukkit.entity.Horse;
import org.bukkit.entity.ItemFrame;
import org.bukkit.entity.Player;
import org.bukkit.entity.TNTPrimed;
import org.bukkit.entity.minecart.PoweredMinecart;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.EntityBlockFormEvent;
import org.bukkit.event.entity.CreatureSpawnEvent;
import org.bukkit.event.entity.EntityBreakDoorEvent;
import org.bukkit.event.entity.EntityChangeBlockEvent;
import org.bukkit.event.entity.EntityDamageByEntityEvent;
import org.bukkit.event.entity.EntityDamageEvent;
import org.bukkit.event.entity.EntityDamageEvent.DamageCause;
import org.bukkit.event.entity.EntityDeathEvent;
import org.bukkit.event.entity.EntityExplodeEvent;
import org.bukkit.event.entity.EntityTargetEvent;
import org.bukkit.event.entity.EntityUnleashEvent;
import org.bukkit.event.entity.PlayerLeashEntityEvent;
import org.bukkit.event.entity.PotionSplashEvent;
import org.bukkit.event.hanging.HangingBreakByEntityEvent;
import org.bukkit.event.hanging.HangingBreakEvent;
import org.bukkit.event.hanging.HangingBreakEvent.RemoveCause;
import org.bukkit.event.hanging.HangingPlaceEvent;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.event.player.PlayerShearEntityEvent;
import org.bukkit.event.player.PlayerUnleashEntityEvent;
import org.bukkit.inventory.ItemStack;
import org.bukkit.potion.PotionEffect;

public class PrismEntityEvents implements Listener {

	/**
	 * 
	 */
	private Prism plugin;
	
	
	/**
	 * 
	 * @param plugin
	 */
	public PrismEntityEvents( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEntityDamageEvent( final EntityDamageByEntityEvent event ){

		if( !(event.getDamager() instanceof Player) ) return;
		
		Entity entity = event.getEntity();
		Player player = (Player) event.getDamager();
		
		// Cancel the event if a wand is in use
		if( WandUtils.playerUsesWandOnClick( player, entity.getLocation() ) ){
			event.setCancelled(true);
			return;
		}
		
		if( entity instanceof ItemFrame ){
			ItemFrame frame = (ItemFrame) event.getEntity();
			// Frame is empty but an item is held
			if( !frame.getItem().getType().equals(Material.AIR) ){
				if( Prism.getIgnore().event("item-remove",player) ){
					RecordingQueue.addToQueue( ActionFactory.create("item-remove", frame.getItem(), 1, 0, null, entity.getLocation(), player.getName()) );
				}
			}
		}
	}

	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEntityDeath(final EntityDeathEvent event) {

		Entity entity = event.getEntity();
		
		// Mob Death
		if(!(entity instanceof Player)){
			if(entity.getLastDamageCause() instanceof EntityDamageByEntityEvent){
				
				if( entity instanceof Horse ){
					Horse horse = (Horse) entity;
					if( horse.isCarryingChest() ){;
						// Log item drops
				        if( Prism.getIgnore().event("item-drop",entity.getWorld()) ){
					        for( ItemStack i : horse.getInventory().getContents() ){
					        	if( i == null ) continue;
					        	RecordingQueue.addToQueue( ActionFactory.create("item-drop", i, i.getAmount(), -1, null, entity.getLocation(), "horse") );
					        }
				        }
					}
				}
				
				// Mob killed by player
				EntityDamageByEntityEvent entityDamageByEntityEvent = (EntityDamageByEntityEvent) entity.getLastDamageCause();
				if(entityDamageByEntityEvent.getDamager() instanceof Player){
					Player player = (Player) entityDamageByEntityEvent.getDamager();
					if( !Prism.getIgnore().event("player-kill", player) ) return;
					RecordingQueue.addToQueue( ActionFactory.create("player-kill", entity, player.getName()) );
					return;
		        	
				}
				// Mob shot by an arrow from a player
				else if(entityDamageByEntityEvent.getDamager() instanceof Arrow){
					Arrow arrow = (Arrow) entityDamageByEntityEvent.getDamager();
					if(arrow.getShooter() instanceof Player){
						
						Player player = (Player) arrow.getShooter();
						if( !Prism.getIgnore().event("player-kill", player) ) return;
						RecordingQueue.addToQueue( ActionFactory.create("player-kill", entity, player.getName()) );
						return;
			        	
					}
				} else {
					// Mob died by another mob
					Entity damager = entityDamageByEntityEvent.getDamager();
					String name = "unknown";
					if(damager != null){
						name = damager.getType().getName();
					}
					if(name == null) name = "unknown";
					if( !Prism.getIgnore().event("entity-kill",entity.getWorld()) ) return;
					RecordingQueue.addToQueue( ActionFactory.create("entity-kill", entity, name) );
				}
			} else {
				
				String killer = "unknown";
				EntityDamageEvent damage = entity.getLastDamageCause();
				if(damage != null){
					DamageCause cause = damage.getCause();
					if(cause != null){
						killer = cause.name().toLowerCase();
					}
				}
				
				// Record the death as natural
				RecordingQueue.addToQueue( ActionFactory.create("entity-kill", entity, killer) );
				
			}
		} else {
			
			// Determine who died and what the exact cause was
	        Player p = (Player)event.getEntity();
	        if( Prism.getIgnore().event("player-death",p) ){
		        String cause = DeathUtils.getCauseNiceName( p );
		        String attacker = DeathUtils.getAttackerName( p );
		        if(attacker == "pvpwolf"){
	            	String owner = DeathUtils.getTameWolfOwner(event);
	            	attacker = owner+"'s wolf";
	            }
		        RecordingQueue.addToQueue( ActionFactory.create("player-death", p, cause, attacker) );
	        }
	        
	        // Log item drops
	        if( Prism.getIgnore().event("item-drop",p) ){
		        if( !event.getDrops().isEmpty() ){
		        	for(ItemStack i : event.getDrops()){
		        		RecordingQueue.addToQueue( ActionFactory.create("item-drop", i, i.getAmount(), -1, null, p.getLocation(), p.getName()) );
		        	}
		        }
	        }
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onCreatureSpawn(final CreatureSpawnEvent event){
		if( !Prism.getIgnore().event("entity-spawn",event.getEntity().getWorld()) ) return;
		String reason = event.getSpawnReason().name().toLowerCase().replace("_", " ");
		if(reason.equals("natural")) return;
		RecordingQueue.addToQueue( ActionFactory.create("entity-spawn", event.getEntity(), reason) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEntityTargetEvent(final EntityTargetEvent event) {
		if( !Prism.getIgnore().event("entity-follow",event.getEntity().getWorld()) ) return;
        if (event.getTarget() instanceof Player) {
        	if(event.getEntity().getType().equals(EntityType.CREEPER)){
	            Player player = (Player) event.getTarget();
	            RecordingQueue.addToQueue( ActionFactory.create("entity-follow", event.getEntity(), player.getName()) );
        	}
        }
    }
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerShearEntity(final PlayerShearEntityEvent event) {
		if( !Prism.getIgnore().event("entity-shear",event.getPlayer()) ) return;
		RecordingQueue.addToQueue( ActionFactory.create("entity-shear", event.getEntity(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerInteractEntityEvent(final PlayerInteractEntityEvent event) {
		
		Player p = event.getPlayer();
		Entity e = event.getRightClicked();
		
		// @todo right clicks should technically follow blockface
		// Cancel the event if a wand is in use
		if( WandUtils.playerUsesWandOnClick( p, e.getLocation() ) ){
			event.setCancelled(true);
			return;
		}
		
		if( e instanceof ItemFrame ){
			
			ItemFrame frame = (ItemFrame) e;
			
			// If held item doesn't equal existing item frame object type
			if( !frame.getItem().getType().equals(Material.AIR) ){
				RecordingQueue.addToQueue( ActionFactory.create("item-rotate", event.getPlayer(), frame.getRotation().name().toLowerCase()) );
			}
			
			// Frame is empty but an item is held
			if( frame.getItem().getType().equals(Material.AIR) && p.getItemInHand() != null ){
				if( Prism.getIgnore().event("item-insert",p) ){
					RecordingQueue.addToQueue( ActionFactory.create("item-insert", p.getItemInHand(), 1, 0, null, e.getLocation(), p.getName()) );
				}
			}
		}
		
		// if they're holding coal (or charcoal, a subitem) and they click a powered minecart
		if( p.getItemInHand().getType().equals(Material.COAL) && e instanceof PoweredMinecart ){
			if( !Prism.getIgnore().event("item-insert",p) ) return;
			RecordingQueue.addToQueue( ActionFactory.create("item-insert", p.getItemInHand(), 1, 0, null, e.getLocation(), p.getName()) );
		}
		
		if( !Prism.getIgnore().event("entity-dye",p) ) return;
		// Only track the event on sheep, when player holds dye
		if( p.getItemInHand().getTypeId() == 351 && e.getType().equals(EntityType.SHEEP) ){
			String newColor = plugin.getItems().getAlias(p.getItemInHand().getTypeId(), (byte)p.getItemInHand().getDurability());
			RecordingQueue.addToQueue( ActionFactory.create("entity-dye", event.getRightClicked(), event.getPlayer().getName(), newColor) );
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEntityBreakDoor(final EntityBreakDoorEvent event) {
		if( !Prism.getIgnore().event("entity-break",event.getEntity().getWorld()) ) return;
		RecordingQueue.addToQueue( ActionFactory.create("entity-break", event.getBlock(), event.getEntityType().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerEntityLeash(final PlayerLeashEntityEvent event){
		if( !Prism.getIgnore().event("entity-leash", event.getPlayer() ) ) return;
		RecordingQueue.addToQueue( ActionFactory.create("entity-leash", event.getEntity(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerEntityUnleash(final PlayerUnleashEntityEvent event){
		if( !Prism.getIgnore().event("entity-unleash", event.getPlayer() ) ) return;
		RecordingQueue.addToQueue( ActionFactory.create("entity-unleash", event.getEntity(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEntityUnleash(final EntityUnleashEvent event){
		if( !Prism.getIgnore().event("entity-unleash" ) ) return;
		RecordingQueue.addToQueue( ActionFactory.create("entity-unleash", event.getEntity(), event.getReason().toString().toLowerCase()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPotionSplashEvent(final PotionSplashEvent event){
		
		Entity entity = event.getPotion().getShooter();
		
		// Ignore from non-players for the time being
		if(!(entity instanceof Player)) return;
		
		Player player = (Player) entity;
		
		if( !Prism.getIgnore().event("potion-splash",player ) ) return;
		
		// What type?
		// Right now this won't support anything with multiple effects
		Collection<PotionEffect> potion = event.getPotion().getEffects();
		String name = "";
		for(PotionEffect eff : potion){
			name = eff.getType().getName().toLowerCase();
		}
		
		RecordingQueue.addToQueue( ActionFactory.create("potion-splash", player, name) );
		
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onHangingPlaceEvent(final HangingPlaceEvent event) {
		// Cancel the event if a wand is in use
		if( WandUtils.playerUsesWandOnClick( event.getPlayer(), event.getEntity().getLocation() ) ){
			event.setCancelled(true);
			return;
		}
		if( !Prism.getIgnore().event("hangingitem-place",event.getPlayer()) ) return;
		RecordingQueue.addToQueue( ActionFactory.create("hangingitem-place", event.getEntity(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * Hanging items broken by a player fall under the HangingBreakByEntityEvent
	 * events. This is merely here to capture cause = physics for when they
	 * detach from a block.
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onHangingBreakEvent(final HangingBreakEvent event){
		
		// Ignore other causes. Entity cause already handled.
		if( !event.getCause().equals(RemoveCause.PHYSICS) ){
			return;
		}
		
		if( !Prism.getIgnore().event("hangingitem-break",event.getEntity().getWorld()) ) return;
		
		Hanging e = event.getEntity();

		// Check for planned hanging item breaks
		String coord_key = e.getLocation().getBlockX() + ":" + e.getLocation().getBlockY() + ":" + e.getLocation().getBlockZ();
		if(plugin.preplannedBlockFalls.containsKey(coord_key)){
			
			String player = plugin.preplannedBlockFalls.get(coord_key);
			
			// Track the hanging item break
			RecordingQueue.addToQueue( ActionFactory.create("hangingitem-break", e, player) );
			plugin.preplannedBlockFalls.remove(coord_key);

			if( !Prism.getIgnore().event("item-remove",event.getEntity().getWorld()) ) return;
			
			// If an item frame, track it's contents
			if( e instanceof ItemFrame ){
				ItemFrame frame = (ItemFrame) e;
				if( frame.getItem() != null ){
					RecordingQueue.addToQueue( ActionFactory.create("item-remove", frame.getItem(), frame.getItem().getAmount(), -1, null, e.getLocation(), player) );
				}
			}
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onHangingBreakByEntityEvent(final HangingBreakByEntityEvent event) {
		
		if( !Prism.getIgnore().event("hangingitem-break",event.getEntity().getWorld()) ) return;
		String breaking_name = "";
		Entity e = event.getRemover();
		if(e instanceof Player){
			Player player = (Player)e;
			breaking_name = player.getName();
		} else {
			breaking_name = e.getType().getName();
		}
		RecordingQueue.addToQueue( ActionFactory.create("hangingitem-break", event.getEntity(), breaking_name) );
		
		if( !Prism.getIgnore().event("item-remove",event.getEntity().getWorld()) ) return;
		
		// If an item frame, track it's contents
		if( event.getEntity() instanceof ItemFrame ){
			ItemFrame frame = (ItemFrame) event.getEntity();
			if( frame.getItem() != null ){
				RecordingQueue.addToQueue( ActionFactory.create("item-remove", frame.getItem(), frame.getItem().getAmount(), -1, null, e.getLocation(), breaking_name) );
			}
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEntityChangeBlock(final EntityChangeBlockEvent event) {
		String entity = event.getEntityType().getName().toLowerCase();
	
		// Technically I think that I really should name it "entity-eat" for better consistency and 
		// in case other mobs ever are made to eat. But that's not as fun
		if(event.getEntityType().equals(EntityType.SHEEP)){
			if( !Prism.getIgnore().event("sheep-eat",event.getBlock()) ) return;
			RecordingQueue.addToQueue( ActionFactory.create("sheep-eat", event.getBlock(), entity) );
		} else {
			
			if(event.getEntity() instanceof Enderman){
	
				if (event.getTo() == Material.AIR){
					if( !Prism.getIgnore().event("enderman-place",event.getBlock()) ) return;
					RecordingQueue.addToQueue( ActionFactory.create("enderman-place", event.getBlock(), entity) );
				} else {
					if( !Prism.getIgnore().event("enderman-pickup",event.getBlock()) ) return;
					Enderman enderman = (Enderman) event.getEntity();
					if (enderman.getCarriedMaterial() != null) {
						RecordingQueue.addToQueue( ActionFactory.create("enderman-pickup", event.getBlock(), entity) );
					}
				}
			}
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEntityBlockForm(final EntityBlockFormEvent event) {
		if( !Prism.getIgnore().event("entity-form",event.getBlock()) ) return;
		Block block = event.getBlock();
		Location loc = block.getLocation();
		BlockState newState = event.getNewState();
		String entity = event.getEntity().getType().name().toLowerCase();
		RecordingQueue.addToQueue( ActionFactory.create("entity-form", loc, block.getTypeId(), (byte)block.getData(), newState.getTypeId(), (byte)newState.getRawData(), entity) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEntityExplodeChangeBlock(final EntityExplodeEvent event) {
		
		if( event.blockList() == null || event.blockList().isEmpty() ) return;
		
		String name;
		String action = "entity-explode";
		if(event.getEntity() != null){
			if(event.getEntity() instanceof Creeper){
				if( !Prism.getIgnore().event("creeper-explode",event.getEntity().getWorld()) ) return;
				action = "creeper-explode";
				name = "creeper";
			}
			else if(event.getEntity() instanceof TNTPrimed){
				if( !Prism.getIgnore().event("tnt-explode",event.getEntity().getWorld()) ) return;
				action = "tnt-explode";
				name = "tnt";
			} else if(event.getEntity() instanceof EnderDragon){
				if( !Prism.getIgnore().event("dragon-eat", event.getEntity().getWorld()) ) return;
				action = "dragon-eat";
				name = "enderdragon";
			} else {
				if( !Prism.getIgnore().event("entity-explode", event.getLocation().getWorld()) ) return;
				try {
					name = event.getEntity().getType().getName().replace("_", " ");
					name = name.length() > 15 ? name.substring(0, 15) : name; // I don't think this can happen, but just in case. Might look weird, but that's better than breaking stuff.
				} catch ( NullPointerException e ){
					name = "unknown";
				}
			}
		} else {
			if( !Prism.getIgnore().event("entity-explode", event.getLocation().getWorld()) ) return;
			name = "magic";
		}
		// Also log item-removes from chests that are blown up
		PrismBlockEvents be = new PrismBlockEvents(plugin);
		for(Block block : event.blockList()){
			
			// don't bother record upper doors.
			if( block.getType().equals(Material.WOODEN_DOOR) || block.getType().equals(Material.IRON_DOOR_BLOCK) ){
				if(block.getData() >= 4){
					continue;
				}
			}
			
			// Change handling a bit if it's a long block
			Block sibling = BlockUtils.getSiblingForDoubleLengthBlock(block);
			if( sibling != null && !block.getType().equals(Material.CHEST) ){
				block = sibling;
			}
			
			// log items removed from container
			// note: done before the container so a "rewind" for rollback will work properly
			be.logItemRemoveFromDestroyedContainer( name, block );
			RecordingQueue.addToQueue( ActionFactory.create(action, block, name));
			// look for relationships
			be.logBlockRelationshipsForBlock( name, block );
			
		}
	}
}