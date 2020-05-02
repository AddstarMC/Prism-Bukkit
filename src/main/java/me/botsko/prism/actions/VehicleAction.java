package me.botsko.prism.actions;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.appliers.ChangeResult;
import me.botsko.prism.appliers.ChangeResultType;
import org.bukkit.entity.Boat;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Minecart;
import org.bukkit.entity.Player;
import org.bukkit.entity.minecart.ExplosiveMinecart;
import org.bukkit.entity.minecart.HopperMinecart;
import org.bukkit.entity.minecart.PoweredMinecart;
import org.bukkit.entity.minecart.SpawnerMinecart;
import org.bukkit.entity.minecart.StorageMinecart;

public class VehicleAction extends GenericAction {
    private String vehicleName;

    /**
     * @param vehicle
     */
    public void setVehicle(Entity vehicle) {

        if (vehicle instanceof PoweredMinecart) {
            vehicleName = "powered minecart";
        } else if (vehicle instanceof HopperMinecart) {
            vehicleName = "minecart hopper";
        } else if (vehicle instanceof SpawnerMinecart) {
            vehicleName = "spawner minecart";
        } else if (vehicle instanceof ExplosiveMinecart) {
            vehicleName = "tnt minecart";
        } else if (vehicle instanceof StorageMinecart) {
            vehicleName = "storage minecart";
        } else {
            vehicleName = vehicle.getType().name().toLowerCase();
        }
    }

    /**
     * @return
     */
    @Override
    public String getNiceName() {
        return vehicleName;
    }

    @Override
    public boolean hasExtraData() {
        return vehicleName != null;
    }

    @Override
    public String serialize() {
        return vehicleName;
    }

    @Override
    public void deserialize(String data) {
        vehicleName = data;
    }

    /**
     *
     */
    @Override
    public ChangeResult applyRollback(Player player, QueryParameters parameters, boolean isPreview) {
        Entity vehicle = null;
        switch (vehicleName) {
            case "powered minecart":
                vehicle = getWorld().spawn(getLoc(), PoweredMinecart.class);
                break;
            case "storage minecart":
                vehicle = getWorld().spawn(getLoc(), StorageMinecart.class);
                break;
            case "tnt minecart":
                vehicle = getWorld().spawn(getLoc(), ExplosiveMinecart.class);
                break;
            case "spawner minecart":
                vehicle = getWorld().spawn(getLoc(), SpawnerMinecart.class);
                break;
            case "minecart hopper":
                vehicle = getWorld().spawn(getLoc(), HopperMinecart.class);
                break;
            case "minecart":
                vehicle = getWorld().spawn(getLoc(), Minecart.class);
                break;
            case "boat":
                vehicle = getWorld().spawn(getLoc(), Boat.class);
                break;
        }
        if (vehicle != null) {
            return new ChangeResult(ChangeResultType.APPLIED, null);
        }
        return new ChangeResult(ChangeResultType.SKIPPED, null);
    }
}