package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.EntityAction;
import me.botsko.prism.actions.HangingItemAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.actions.PlayerDeathAction;
import me.botsko.prism.utils.DeathUtils;

import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.entity.Arrow;
import org.bukkit.entity.Creeper;
import org.bukkit.entity.Enderman;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Hanging;
import org.bukkit.entity.Player;
import org.bukkit.entity.TNTPrimed;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.entity.CreatureSpawnEvent;
import org.bukkit.event.entity.EntityBreakDoorEvent;
import org.bukkit.event.entity.EntityChangeBlockEvent;
import org.bukkit.event.entity.EntityDamageByEntityEvent;
import org.bukkit.event.entity.EntityDamageEvent;
import org.bukkit.event.entity.EntityDamageEvent.DamageCause;
import org.bukkit.event.entity.EntityDeathEvent;
import org.bukkit.event.entity.EntityExplodeEvent;
import org.bukkit.event.entity.EntityTargetEvent;
import org.bukkit.event.hanging.HangingBreakByEntityEvent;
import org.bukkit.event.hanging.HangingBreakEvent;
import org.bukkit.event.hanging.HangingBreakEvent.RemoveCause;
import org.bukkit.event.hanging.HangingPlaceEvent;
import org.bukkit.event.player.PlayerShearEntityEvent;
import org.bukkit.inventory.ItemStack;

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
	public void onEntityDeath(final EntityDeathEvent event) {

		Entity entity = event.getEntity();
		
		// Mob Death
		if(!(entity instanceof Player)){
			if(entity.getLastDamageCause() instanceof EntityDamageByEntityEvent){
				
				// Mob killed by player
				EntityDamageByEntityEvent entityDamageByEntityEvent = (EntityDamageByEntityEvent) entity.getLastDamageCause();
				if(entityDamageByEntityEvent.getDamager() instanceof Player){
					
					Player player = (Player) entityDamageByEntityEvent.getDamager();
					plugin.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_KILL, entity, player.getName()) );
					return;
		        	
				}
				// Mob shot by an arrow
				else if(entityDamageByEntityEvent.getDamager() instanceof Arrow){
					Arrow arrow = (Arrow) entityDamageByEntityEvent.getDamager();
					if(arrow.getShooter() instanceof Player){
						
						Player player = (Player) arrow.getShooter();
						plugin.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_KILL, entity, player.getName()) );
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
					plugin.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_KILL, entity, name) );
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
				plugin.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_KILL, entity, killer) );
				
			}
		} else {
			
			// Determine who died and what the exact cause was
	        Player p = (Player)event.getEntity();
	        String cause = DeathUtils.getCauseOfDeath( event, p );
	        String attacker = DeathUtils.getAttacker(event, p);
	        if(attacker == "pvpwolf"){
            	String owner = DeathUtils.getTameWolfOwner(event);
            	attacker = owner+"'s wolf";
            }
	        plugin.actionsRecorder.addToQueue( new PlayerDeathAction(ActionType.PLAYER_DEATH, p, cause, attacker) );
	        
	        // Log item drops
	        if( !event.getDrops().isEmpty() ){
	        	for(ItemStack i : event.getDrops()){
	        		plugin.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_DROP, i, i.getAmount(), p.getLocation(), p.getName()) );
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
		String reason = event.getSpawnReason().name().toLowerCase().replace("_", " ");
		if(reason.equals("natural")) return;
		plugin.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_SPAWN, event.getEntity(), reason) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEntityTargetEvent(final EntityTargetEvent event) {
        if (event.getTarget() instanceof Player) {
        	if(event.getEntity().getType().equals(EntityType.CREEPER)){
	            Player player = (Player) event.getTarget();
	            plugin.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_FOLLOW, event.getEntity(), player.getName()) );
        	}
        }
    }
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerShearEntity(final PlayerShearEntityEvent event) {
		plugin.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_SHEAR, event.getEntity(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEntityBreakDoor(final EntityBreakDoorEvent event) {
		plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.ENTITY_BREAK, event.getBlock(), event.getEntityType().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onHangingPlaceEvent(final HangingPlaceEvent event) {
		plugin.actionsRecorder.addToQueue( new HangingItemAction(ActionType.HANGINGITEM_PLACE, event.getEntity(), event.getPlayer().getName()) );
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
		// @todo should we worry about explosion/obstruction causes?
		if( !event.getCause().equals(RemoveCause.PHYSICS) ){
			return;
		}
		
		Hanging e = event.getEntity();

		// Check for planned hanging item breaks
		String coord_key = e.getLocation().getBlockX() + ":" + e.getLocation().getBlockY() + ":" + e.getLocation().getBlockZ();
		if(plugin.preplannedBlockFalls.containsKey(coord_key)){
			String player = plugin.preplannedBlockFalls.get(coord_key);
			plugin.actionsRecorder.addToQueue( new HangingItemAction(ActionType.HANGINGITEM_BREAK, e, player) );
			plugin.preplannedBlockFalls.remove(coord_key);
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onHangingBreakByEntityEvent(final HangingBreakByEntityEvent event) {
		String breaking_name = "";
		Entity e = event.getRemover();
		if(e instanceof Player){
			Player player = (Player)e;
			breaking_name = player.getName();
		} else {
			breaking_name = e.getType().getName();
		}
		plugin.actionsRecorder.addToQueue( new HangingItemAction(ActionType.HANGINGITEM_BREAK, event.getEntity(), breaking_name) );
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
			plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.SHEEP_EAT, event.getBlock(), entity) );
		} else {
			
			if(event.getEntity() instanceof Enderman){
	
				if (event.getTo() == Material.AIR){
					plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.ENDERMAN_PLACE, event.getBlock(), entity) );
				} else {
					Enderman enderman = (Enderman) event.getEntity();
					if (enderman.getCarriedMaterial() != null) {
						plugin.actionsRecorder.addToQueue( new BlockAction(ActionType.ENDERMAN_PICKUP, event.getBlock(), entity) );
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
	public void onEntityExplodeChangeBlock(final EntityExplodeEvent event) {
		String name;
		ActionType action = ActionType.ENTITY_EXPLODE;
		if(event.getEntity() != null){
			name = event.getEntityType().getName().toLowerCase();
			if(event.getEntity() instanceof Creeper){
				action = ActionType.CREEPER_EXPLODE;
				name = "creeper";
			}
			else if(event.getEntity() instanceof TNTPrimed){
				action = ActionType.TNT_EXPLODE;
				name = "tnt";
			}
		} else {
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
			block = be.properlyLogDoubleLengthBlocks(block);
			// Log items from chests
			be.logItemRemoveFromDestroyedContainer(name, block);
			// Record blocks explode
			plugin.actionsRecorder.addToQueue( new BlockAction(action, block, name) );
			// look for relationships
			be.logBlockRelationshipsForBlock( name, block );
		}
	}
}