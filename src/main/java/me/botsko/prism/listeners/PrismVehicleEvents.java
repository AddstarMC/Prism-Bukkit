package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;

import org.bukkit.Location;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;
import org.bukkit.entity.Vehicle;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.vehicle.VehicleCreateEvent;
import org.bukkit.event.vehicle.VehicleDestroyEvent;
import org.bukkit.event.vehicle.VehicleEnterEvent;
import org.bukkit.event.vehicle.VehicleExitEvent;

public class PrismVehicleEvents implements Listener {
	
	/**
	 * 
	 */
	private final Prism plugin;

	
	/**
	 * 
	 * @param plugin
	 */
	public PrismVehicleEvents( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onVehicleCreate(final VehicleCreateEvent event){
		
		Vehicle vehicle = event.getVehicle();
		Location loc = vehicle.getLocation();
		
		String coord_key = loc.getBlockX() + ":" + loc.getBlockY() + ":" + loc.getBlockZ();
		String player = plugin.preplannedVehiclePlacement.get(coord_key);
		if(player != null){
			if( !Prism.getIgnore().event("vehicle-place",loc.getWorld(),player) ) return;
			RecordingQueue.addToQueue( ActionFactory.create("vehicle-place", vehicle, player) );
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onVehicleDestroy(final VehicleDestroyEvent event){
		
		Vehicle vehicle = event.getVehicle();
		Entity attacker = event.getAttacker();
		// Was it broken by an attack
		if( attacker != null ){
			if( attacker instanceof Player ){
				if( !Prism.getIgnore().event("vehicle-break",((Player)attacker)) ) return;
				RecordingQueue.addToQueue( ActionFactory.create("vehicle-break", vehicle, ((Player)attacker).getName()) );
			} else {
				if( !Prism.getIgnore().event("vehicle-break",attacker.getWorld()) ) return;
				RecordingQueue.addToQueue( ActionFactory.create("vehicle-break", vehicle, attacker.getType().name().toLowerCase()) );
			}
		} else {
			
			// Otherwise its driver was reckless
			Entity passenger = vehicle.getPassenger();
			if( passenger != null && passenger instanceof Player ){
				if( !Prism.getIgnore().event("vehicle-break",((Player)passenger)) ) return;
				RecordingQueue.addToQueue( ActionFactory.create("vehicle-break", vehicle, ((Player)passenger).getName()) );
			}
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onVehicleEnter(final VehicleEnterEvent event){
		
		Vehicle vehicle = event.getVehicle();
		
		Entity entity = event.getEntered();
		if( entity instanceof Player ){
			if( !Prism.getIgnore().event("vehicle-enter", ((Player)entity) ) ) return;
			RecordingQueue.addToQueue( ActionFactory.create("vehicle-enter", vehicle, ((Player)entity).getName() ) );
		} else {
			if( !Prism.getIgnore().event("vehicle-enter", entity.getWorld() ) ) return;
			RecordingQueue.addToQueue( ActionFactory.create("vehicle-enter", vehicle, entity.getType().name().toLowerCase() ) );
		}
	}
	
	
	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onVehicleExit(final VehicleExitEvent event){
		
		Vehicle vehicle = event.getVehicle();
		
		Entity entity = event.getExited();
		if( entity instanceof Player ){
			if( !Prism.getIgnore().event("vehicle-enter", ((Player)entity) ) ) return;
			RecordingQueue.addToQueue( ActionFactory.create("vehicle-exit", vehicle, ((Player)entity).getName() ) );
		} else {
			if( !Prism.getIgnore().event("vehicle-enter", entity.getWorld() ) ) return;
			RecordingQueue.addToQueue( ActionFactory.create("vehicle-exit", vehicle, entity.getType().name().toLowerCase() ) );
		}
	}
}