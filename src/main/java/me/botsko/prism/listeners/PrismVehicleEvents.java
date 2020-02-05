package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;

import java.util.List;
import java.util.UUID;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.OfflinePlayer;
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
	public PrismVehicleEvents(Prism plugin) {
		this.plugin = plugin;
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onVehicleCreate(final VehicleCreateEvent event) {

		final Vehicle vehicle = event.getVehicle();
		final Location loc = vehicle.getLocation();

		final String coord_key = loc.getBlockX() + ":" + loc.getBlockY() + ":" + loc.getBlockZ();
		String value = plugin.preplannedVehiclePlacement.get(coord_key);
		UUID uuid = null;
		try {
			uuid = UUID.fromString(value);
		} catch (Exception ignored) {
		}
		final OfflinePlayer player = uuid != null ? Bukkit.getOfflinePlayer(uuid) : null;
		if (player != null) {
			// TODO: name ref
			if (!Prism.getIgnore().event("vehicle-place", loc.getWorld(), player.getName()))
				return;
			RecordingQueue.addToQueue(ActionFactory.createVehicle("vehicle-place", vehicle, player));
		}
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onVehicleDestroy(final VehicleDestroyEvent event) {

		final Vehicle vehicle = event.getVehicle();
		final Entity attacker = event.getAttacker();
		// Was it broken by an attack
		if (attacker != null) {
			if (attacker instanceof Player) {
				if (!Prism.getIgnore().event("vehicle-break", ((Player) attacker)))
					return;
				RecordingQueue.addToQueue(ActionFactory.createVehicle("vehicle-break", vehicle, (Player) attacker));
			}
			else {
				if (!Prism.getIgnore().event("vehicle-break", attacker.getWorld()))
					return;
				RecordingQueue.addToQueue(
						ActionFactory.createVehicle("vehicle-break", vehicle, attacker.getType().name().toLowerCase()));
			}
		}
		else {

			// Otherwise its driver was reckless
			final List<Entity> passengers = vehicle.getPassengers();
			if (!passengers.isEmpty()) {
				Entity passenger = passengers.get(0);
				if (passenger instanceof Player) {
					if (!Prism.getIgnore().event("vehicle-break", ((Player) passenger)))
						return;
					RecordingQueue.addToQueue(ActionFactory.createVehicle("vehicle-break", vehicle, (Player) passenger));
				} else {
					if (!Prism.getIgnore().event("vehicle-break", passenger.getWorld()))
						return;
					RecordingQueue.addToQueue(ActionFactory.createVehicle("vehicle-break", vehicle, passenger.getType().name().toLowerCase()));
				}
			}
		}
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onVehicleEnter(final VehicleEnterEvent event) {

		final Vehicle vehicle = event.getVehicle();

		final Entity entity = event.getEntered();
		if (entity instanceof Player) {
			if (!Prism.getIgnore().event("vehicle-enter", ((Player) entity)))
				return;
			RecordingQueue.addToQueue(ActionFactory.createVehicle("vehicle-enter", vehicle, (Player) entity));
		}
		else {
			if (!Prism.getIgnore().event("vehicle-enter", entity.getWorld()))
				return;
			RecordingQueue.addToQueue(
					ActionFactory.createVehicle("vehicle-enter", vehicle, entity.getType().name().toLowerCase()));
		}
	}

	/**
	 * 
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onVehicleExit(final VehicleExitEvent event) {

		final Vehicle vehicle = event.getVehicle();

		final Entity entity = event.getExited();
		if (entity instanceof Player) {
			if (!Prism.getIgnore().event("vehicle-enter", ((Player) entity)))
				return;
			RecordingQueue.addToQueue(ActionFactory.createVehicle("vehicle-exit", vehicle, (Player) entity));
		}
		else {
			if (!Prism.getIgnore().event("vehicle-enter", entity.getWorld()))
				return;
			RecordingQueue.addToQueue(
					ActionFactory.createVehicle("vehicle-exit", vehicle, entity.getType().name().toLowerCase()));
		}
	}
}