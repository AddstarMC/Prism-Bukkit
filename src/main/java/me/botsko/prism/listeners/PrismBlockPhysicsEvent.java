package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;

import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockPhysicsEvent;
import org.bukkit.material.Attachable;
import me.botsko.prism.utils.BlockUtils;
import org.bukkit.craftbukkit.v1_7_R4.CraftWorld;


//Cauldron start - temporary workaround for lack of break events fired by mods
public class PrismBlockPhysicsEvent implements Listener {

    /**
     *
     */
    private final Prism plugin;

    /**
     * 
     * @param plugin
     */
    public PrismBlockPhysicsEvent(Prism plugin) {
        this.plugin = plugin;
    }

    /**
     * 
     * @param event
     */
    @EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
    public void onBlockPhysics(final BlockPhysicsEvent event) {

        if( !Prism.getIgnore().event( "block-break", event.getBlock() ) ) return;
        
        final Block b = event.getBlock();

        // If it's an attachable item, we need to look for detachment
        // at the sides.
        // http://jd.bukkit.org/doxygen/d1/d0b/interfaceorg_1_1bukkit_1_1material_1_1Attachable.html#details
        if( !(b.getState().getData() instanceof Attachable) ) {

            Block block = event.getBlock();
            if (!((CraftWorld)block.getWorld()).getHandle().isEmpty(block.getX(), block.getY(), block.getZ()) && !Prism.getIllegalPhysicsBlocks().contains( block.getTypeId()))
            {
                String te_data = BlockUtils.compressTileEntityData(event.getBlock());
                if (te_data != null)
                {
                    RecordingQueue.addToQueue( ActionFactory.createBlock("block-break", block, "", te_data) );
                }
                else
                {
                    RecordingQueue.addToQueue( ActionFactory.createBlock("block-break", block, ""));
                }
            }
        }
    }
}
//Cauldron end