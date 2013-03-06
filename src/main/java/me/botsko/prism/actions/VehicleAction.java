package me.botsko.prism.actions;

import org.bukkit.entity.PoweredMinecart;
import org.bukkit.entity.StorageMinecart;
import org.bukkit.entity.Vehicle;

public class VehicleAction extends GenericAction {
	
	
	/**
	 * 
	 * @param vehicle
	 */
	public void setVehicle( Vehicle vehicle ){
		
		if( vehicle instanceof PoweredMinecart ){
			this.data = "powered minecart";
		}
		else if( vehicle instanceof StorageMinecart ){
			this.data = "storage minecart";
		} else {
			this.data = vehicle.getType().name().toLowerCase();
		}
	}

	
	/**
	 * 
	 * @return
	 */
	public String getNiceName(){
		return this.data;
	}
}