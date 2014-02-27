package me.botsko.prism.listeners;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import me.botsko.prism.utils.BlockUtils;

import org.bukkit.Material;
import org.bukkit.block.Block;
import org.bukkit.block.BlockFace;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.event.block.BlockPhysicsEvent;
import org.bukkit.material.Attachable;
import org.bukkit.craftbukkit.v1_5_R3.CraftWorld; // MCPC+

public class PrismBlockPhysicsEvent implements Listener {

	/**
	 *
	 */
	private final Prism plugin;


	/**
	 *
	 * @param plugin
	 */
	public PrismBlockPhysicsEvent( Prism plugin ){
		this.plugin = plugin;
	}
	
	
	/**
	 *
	 * @param event
	 */
	@EventHandler(priority = EventPriority.MONITOR, ignoreCancelled = true)
	public void onBlockPhysics(final BlockPhysicsEvent event) {

		// Record that a block fell, associated with the player who broke the base block.
		Block b = event.getBlock();
		if(BlockUtils.isFallingBlock(b)){
			if( !Prism.getIgnore().event("block-fall", event.getBlock()) ) return;
			// Only record a block-fall if there's air below.
			if(b.getRelative(BlockFace.DOWN).getType().equals(Material.AIR)){
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
				if(plugin.preplannedBlockFalls.containsKey(coord_key)){
					String player = plugin.preplannedBlockFalls.get(coord_key);
					RecordingQueue.addToQueue( ActionFactory.create("block-fall", b, player) );
					plugin.preplannedBlockFalls.remove(coord_key);
				}
			}
		}

		if( !Prism.getIgnore().event("block-break", event.getBlock()) ) return;

		// If it's an attachable item, we need to look for detachment
		// at the sides.
		// http://jd.bukkit.org/doxygen/d1/d0b/interfaceorg_1_1bukkit_1_1material_1_1Attachable.html#details
		if (b.getState().getData() instanceof Attachable) {
			Attachable a = (Attachable)	b.getState().getData();
			if(a == null) return;
			if(a.getAttachedFace() == null) return;
			Block attachedBlock = b.getRelative(a.getAttachedFace());
			if(attachedBlock != null){
				// If it's lost an attached block
				if (BlockUtils.materialMeansBlockDetachment(attachedBlock.getType())) {
					String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
					if(plugin.preplannedBlockFalls.containsKey(coord_key)){
						String player = plugin.preplannedBlockFalls.get(coord_key);
						RecordingQueue.addToQueue( ActionFactory.create("block-break", b, player) );
						plugin.preplannedBlockFalls.remove(coord_key);
					}
				}
			}
		}
		// Otherwise we need to look for detachment at the bottom.
		else {

			Block attachedBlock = b.getRelative(BlockFace.DOWN);
			// If it's lost a supporting block
			if (BlockUtils.materialMeansBlockDetachment(attachedBlock.getType())) {
				String coord_key = b.getX() + ":" + b.getY() + ":" + b.getZ();
				if(plugin.preplannedBlockFalls.containsKey(coord_key)){
					String player = plugin.preplannedBlockFalls.get(coord_key);
					RecordingQueue.addToQueue( ActionFactory.create("block-break", b, player) );
					plugin.preplannedBlockFalls.remove(coord_key);
				}
			}
			// MCPC+ start - temporary workaround for lack of break events in Forge
			Block block = event.getBlock();
			if (!((CraftWorld)block.getWorld()).getHandle().isEmpty(block.getX(), block.getY(), block.getZ()) && !Prism.getIllegalPhysicsBlocks().contains( block.getTypeId()))
			{
				String te_data = BlockUtils.compressTileEntityData(event.getBlock());
				if (te_data != null)
				{
					RecordingQueue.addToQueue( ActionFactory.create("block-break", block, "", te_data) );
				}
				else { 
					RecordingQueue.addToQueue( ActionFactory.create("block-break", block, ""));
				}
			}
			// MCPC+ end
		}
	}

}
