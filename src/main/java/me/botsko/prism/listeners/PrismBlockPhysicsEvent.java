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
// Cauldron start
import me.botsko.prism.utils.BlockUtils;
import org.bukkit.craftbukkit.v1_7_R4.CraftWorld;
// Cauldron end

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

        // Record that a block fell, associated with the player who broke the
        // base block.
        final Block b = event.getBlock();
        if( me.botsko.elixr.BlockUtils.isFallingBlock( b ) ) {
            if( !Prism.getIgnore().event( "block-fall", event.getBlock() ) )
                return;
            // Only record a block-fall if there's air below.
            if( b.getRelative( BlockFace.DOWN ).getType().equals( Material.AIR ) ) {
                final String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
                if( plugin.preplannedBlockFalls.containsKey( coord_key ) ) {
                    final String player = plugin.preplannedBlockFalls.get( coord_key );
                    RecordingQueue.addToQueue( ActionFactory.createBlock("block-fall", b, player) );
                    plugin.preplannedBlockFalls.remove( coord_key );
                }
            }
        }

        if( !Prism.getIgnore().event( "block-break", event.getBlock() ) )
            return;

        // If it's an attachable item, we need to look for detachment
        // at the sides.
        // http://jd.bukkit.org/doxygen/d1/d0b/interfaceorg_1_1bukkit_1_1material_1_1Attachable.html#details
        if( b.getState().getData() instanceof Attachable ) {
            final Attachable a = (Attachable) b.getState().getData();
            if( a == null )
                return;
            if( a.getAttachedFace() == null )
                return;
            final Block attachedBlock = b.getRelative( a.getAttachedFace() );
            if( attachedBlock != null ) {
                // If it's lost an attached block
                if( me.botsko.elixr.BlockUtils.materialMeansBlockDetachment( attachedBlock.getType() ) ) {
                    final String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
                    if( plugin.preplannedBlockFalls.containsKey( coord_key ) ) {
                        final String player = plugin.preplannedBlockFalls.get( coord_key );
                        RecordingQueue.addToQueue( ActionFactory.createBlock("block-break", b, player) );
                        plugin.preplannedBlockFalls.remove( coord_key );
                    }
                }
            }
        }
        // Otherwise we need to look for detachment at the bottom.
        else {

            final Block attachedBlock = b.getRelative( BlockFace.DOWN );
            // If it's lost a supporting block
            if( me.botsko.elixr.BlockUtils.materialMeansBlockDetachment( attachedBlock.getType() ) ) {
                final String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
                if( plugin.preplannedBlockFalls.containsKey( coord_key ) ) {
                    final String player = plugin.preplannedBlockFalls.get( coord_key );
                    RecordingQueue.addToQueue( ActionFactory.createBlock("block-break", b, player) );
                    plugin.preplannedBlockFalls.remove( coord_key );
                }
            }
            // Cauldron start - temporary workaround for lack of break events fired by mods
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
            // Cauldron end
        }
    }

}
