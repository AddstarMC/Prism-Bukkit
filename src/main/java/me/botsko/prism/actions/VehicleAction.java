package me.botsko.prism.actions;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;

import org.bukkit.entity.Boat;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Minecart;
import org.bukkit.entity.Player;
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
	
	
	/**
	 * 
	 */
	@Override
	public ChangeResult applyRollback( Player player, QueryParameters parameters, boolean is_preview ){
		
		Entity vehicle = null;
		if( this.data.equals("powered minecart") ){
			vehicle = getWorld().spawn( getLoc(), PoweredMinecart.class );
		}
		else if( this.data.equals("storage minecart") ){
			vehicle = getWorld().spawn( getLoc(), StorageMinecart.class );
		}
		else if( this.data.equals("minecart") ){
			vehicle = getWorld().spawn( getLoc(), Minecart.class );
		}
		else if( this.data.equals("boat") ){
			vehicle = getWorld().spawn( getLoc(), Boat.class );
		}
		if( vehicle != null ){
			return new ChangeResult( ChangeResultType.APPLIED, null );
		}
		return new ChangeResult( ChangeResultType.SKIPPED, null );
	}
}