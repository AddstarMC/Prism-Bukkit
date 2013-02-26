package me.botsko.prism.listeners;

import java.util.Collection;

import me.botsko.prism.Prism;
import me.botsko.prism.actions.ActionType;
import me.botsko.prism.actions.BlockAction;
import me.botsko.prism.actions.BlockChangeAction;
import me.botsko.prism.actions.EntityAction;
import me.botsko.prism.actions.HangingItemAction;
import me.botsko.prism.actions.ItemStackAction;
import me.botsko.prism.actions.PlayerAction;
import me.botsko.prism.actions.PlayerDeathAction;
import me.botsko.prism.utils.DeathUtils;

import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockState;
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
import org.bukkit.event.entity.PotionSplashEvent;
import org.bukkit.event.hanging.HangingBreakByEntityEvent;
import org.bukkit.event.hanging.HangingBreakEvent;
import org.bukkit.event.hanging.HangingBreakEvent.RemoveCause;
import org.bukkit.event.hanging.HangingPlaceEvent;
import org.bukkit.event.player.PlayerInteractEntityEvent;
import org.bukkit.event.player.PlayerShearEntityEvent;
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
	public void onEntityDeath(final EntityDeathEvent event) {

		Entity entity = event.getEntity();
		
		// Mob Death
		if(!(entity instanceof Player)){
			if( !plugin.getConfig().getBoolean("prism.tracking.entity-kill") && !plugin.getConfig().getBoolean("prism.tracking.player-kill") ) return;
			if(entity.getLastDamageCause() instanceof EntityDamageByEntityEvent){
				
				// Mob killed by player
				EntityDamageByEntityEvent entityDamageByEntityEvent = (EntityDamageByEntityEvent) entity.getLastDamageCause();
				if(entityDamageByEntityEvent.getDamager() instanceof Player){
					
					Player player = (Player) entityDamageByEntityEvent.getDamager();
					Prism.actionsRecorder.addToQueue( new EntityAction(ActionType.PLAYER_KILL, entity, player.getName()) );
					return;
		        	
				}
				// Mob shot by an arrow from a player
				else if(entityDamageByEntityEvent.getDamager() instanceof Arrow){
					Arrow arrow = (Arrow) entityDamageByEntityEvent.getDamager();
					if(arrow.getShooter() instanceof Player){
						
						Player player = (Player) arrow.getShooter();
						Prism.actionsRecorder.addToQueue( new EntityAction(ActionType.PLAYER_KILL, entity, player.getName()) );
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
					Prism.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_KILL, entity, name) );
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
				Prism.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_KILL, entity, killer) );
				
			}
		} else {
			
			if( !plugin.getConfig().getBoolean("prism.tracking.player-death") && !plugin.getConfig().getBoolean("prism.tracking.item-drop") ) return;
			
			// Determine who died and what the exact cause was
	        Player p = (Player)event.getEntity();
	        String cause = DeathUtils.getCauseOfDeath( event, p );
	        String attacker = DeathUtils.getAttacker(event, p);
	        if(attacker == "pvpwolf"){
            	String owner = DeathUtils.getTameWolfOwner(event);
            	attacker = owner+"'s wolf";
            }
	        Prism.actionsRecorder.addToQueue( new PlayerDeathAction(ActionType.PLAYER_DEATH, p, cause, attacker) );
	        
	        // Log item drops
	        if( !event.getDrops().isEmpty() ){
	        	for(ItemStack i : event.getDrops()){
	        		Prism.actionsRecorder.addToQueue( new ItemStackAction(ActionType.ITEM_DROP, i, i.getAmount(), null, p.getLocation(), p.getName()) );
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
		if( !plugin.getConfig().getBoolean("prism.tracking.entity-spawn") ) return;
		String reason = event.getSpawnReason().name().toLowerCase().replace("_", " ");
		if(reason.equals("natural")) return;
		Prism.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_SPAWN, event.getEntity(), reason) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onEntityTargetEvent(final EntityTargetEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.entity-follow") ) return;
        if (event.getTarget() instanceof Player) {
        	if(event.getEntity().getType().equals(EntityType.CREEPER)){
	            Player player = (Player) event.getTarget();
	            Prism.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_FOLLOW, event.getEntity(), player.getName()) );
        	}
        }
    }
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerShearEntity(final PlayerShearEntityEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.entity-shear") ) return;
		Prism.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_SHEAR, event.getEntity(), event.getPlayer().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPlayerInteractEntityEvent(final PlayerInteractEntityEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.entity-dye") ) return;
		Player p = event.getPlayer();
		Entity e = event.getRightClicked();
		// Only track the event on sheep, when player holds dye
		if( p.getItemInHand().getTypeId() == 351 && e.getType().equals(EntityType.SHEEP) ){
			String newColor = plugin.getItems().getItemStackAliasById(p.getItemInHand().getTypeId(), (byte)p.getItemInHand().getDurability());
			Prism.actionsRecorder.addToQueue( new EntityAction(ActionType.ENTITY_DYE, event.getRightClicked(), event.getPlayer().getName(), newColor) );
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEntityBreakDoor(final EntityBreakDoorEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.entity-break") ) return;
		Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.ENTITY_BREAK, event.getBlock(), event.getEntityType().getName()) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onPotionSplashEvent(final PotionSplashEvent event){
		
		if( !plugin.getConfig().getBoolean("prism.tracking.potion-splash") ) return;
		
		Entity entity = event.getPotion().getShooter();
		
		// Ignore from non-players for the time being
		if(!(entity instanceof Player)) return;
		
		// What type?
		// Right now this won't support anything with multiple effects
		Collection<PotionEffect> potion = event.getPotion().getEffects();
		String name = "";
		for(PotionEffect eff : potion){
			name = eff.getType().getName().toLowerCase();
		}
		
		Prism.actionsRecorder.addToQueue( new PlayerAction(ActionType.POTION_SPLASH, (Player)entity, name) );
		
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onHangingPlaceEvent(final HangingPlaceEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.hangingitem-place") ) return;
		Prism.actionsRecorder.addToQueue( new HangingItemAction(ActionType.HANGINGITEM_PLACE, event.getEntity(), event.getPlayer().getName()) );
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
		
		if( !plugin.getConfig().getBoolean("prism.tracking.hangingitem-break") ) return;
		
		// Ignore other causes. Entity cause already handled.
		if( !event.getCause().equals(RemoveCause.PHYSICS) ){
			return;
		}
		
		Hanging e = event.getEntity();

		// Check for planned hanging item breaks
		String coord_key = e.getLocation().getBlockX() + ":" + e.getLocation().getBlockY() + ":" + e.getLocation().getBlockZ();
		if(plugin.preplannedBlockFalls.containsKey(coord_key)){
			String player = plugin.preplannedBlockFalls.get(coord_key);
			Prism.actionsRecorder.addToQueue( new HangingItemAction(ActionType.HANGINGITEM_BREAK, e, player) );
			plugin.preplannedBlockFalls.remove(coord_key);
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onHangingBreakByEntityEvent(final HangingBreakByEntityEvent event) {
		if( !plugin.getConfig().getBoolean("prism.tracking.hangingitem-break") ) return;
		String breaking_name = "";
		Entity e = event.getRemover();
		if(e instanceof Player){
			Player player = (Player)e;
			breaking_name = player.getName();
		} else {
			breaking_name = e.getType().getName();
		}
		Prism.actionsRecorder.addToQueue( new HangingItemAction(ActionType.HANGINGITEM_BREAK, event.getEntity(), breaking_name) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEntityChangeBlock(final EntityChangeBlockEvent event) {
		String entity = event.getEntityType().getName().toLowerCase();
		
		if( !plugin.getConfig().getBoolean("prism.tracking.sheep-eat")
				&& !plugin.getConfig().getBoolean("prism.tracking.enderman-place")
				&& !plugin.getConfig().getBoolean("prism.tracking.enderman-break")) return;
		
		// Technically I think that I really should name it "entity-eat" for better consistency and 
		// in case other mobs ever are made to eat. But that's not as fun
		if(event.getEntityType().equals(EntityType.SHEEP)){
			Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.SHEEP_EAT, event.getBlock(), entity) );
		} else {
			
			if(event.getEntity() instanceof Enderman){
	
				if (event.getTo() == Material.AIR){
					Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.ENDERMAN_PLACE, event.getBlock(), entity) );
				} else {
					Enderman enderman = (Enderman) event.getEntity();
					if (enderman.getCarriedMaterial() != null) {
						Prism.actionsRecorder.addToQueue( new BlockAction(ActionType.ENDERMAN_PICKUP, event.getBlock(), entity) );
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
		if( !plugin.getConfig().getBoolean("prism.tracking.entity-form")) return;
		Block block = event.getBlock();
		Location loc = block.getLocation();
		BlockState newState = event.getNewState();
		String entity = event.getEntity().getType().name().toLowerCase();
		Prism.actionsRecorder.addToQueue( new BlockChangeAction(ActionType.ENTITY_FORM, loc, block.getTypeId(), (byte)block.getData(), newState.getTypeId(), (byte)newState.getRawData(), entity) );
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onEntityExplodeChangeBlock(final EntityExplodeEvent event) {
		
		if( !plugin.getConfig().getBoolean("prism.tracking.entity-explode")
				&& !plugin.getConfig().getBoolean("prism.tracking.creeper-explode")
				&& !plugin.getConfig().getBoolean("prism.tracking.tnt-explode")) return;
		
		String name;
		ActionType action = ActionType.ENTITY_EXPLODE;
		if(event.getEntity() != null){
			name = "";
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
			Prism.actionsRecorder.addToQueue( new BlockAction(action, block, name) );
			// look for relationships
			be.logBlockRelationshipsForBlock( name, block );
		}
	}
}