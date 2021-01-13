package me.botsko.prism.events;

import me.botsko.prism.api.BlockStateChange;
import me.botsko.prism.api.PrismApi;
import me.botsko.prism.api.PrismParameters;
import me.botsko.prism.api.objects.ApplierResult;
import org.bukkit.entity.Player;

import java.util.ArrayList;
import java.util.List;

/**
 * Created for the Prism-Bukkit Project.
 * Created by Narimm on 13/01/2021.
 */
public class EventHelper {

    public static PrismRollBackEvent createRollBackEvent(List<BlockStateChange> blockStateChanges,
                                                         Player onBehalfOf, PrismParameters parameters,
                                                         ApplierResult result) {
        return new PrismRollBackEvent(blockStateChanges, onBehalfOf, parameters, result);
    }

    public static PrismLoadedEvent createLoadEvent(PrismApi api) {
        return new PrismLoadedEvent(api);
    }

    public static PrismUnloadEvent createUnLoadEvent() {
        return new PrismUnloadEvent();
    }

    public static PrismDrainEvent createDrainEvent(ArrayList<BlockStateChange> blockStateChanges, Player onBehalfOf, int radius) {
        return new PrismDrainEvent(blockStateChanges, onBehalfOf, radius);
    }

    public static PrismExtinguishEvent createExtinguishEvent(ArrayList<BlockStateChange> blockStateChanges, Player onBehalfOf, int radius) {
        return new PrismExtinguishEvent(blockStateChanges,onBehalfOf,radius);
    }
}
