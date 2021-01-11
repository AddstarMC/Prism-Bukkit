package me.botsko.prism.api.events;


import me.botsko.prism.api.BlockStateChange;
import org.bukkit.event.Event;

import java.util.List;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 11/01/2021.
 */
public abstract class PrismRollBackEvent extends Event {

    /**
     * ArrayList.
     *
     * @return the originalBlock  List BlockStateChange
     */
    public abstract List<BlockStateChange> getBlockStateChanges();
}
