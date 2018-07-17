package me.botsko.prism.bridge;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;

import com.sk89q.worldedit.Vector;
import com.sk89q.worldedit.blocks.BaseBlock;
import com.sk89q.worldedit.blocks.BlockType;
import com.sk89q.worldedit.bukkit.BukkitUtil;
import com.sk89q.worldedit.extension.platform.Actor;
import com.sk89q.worldedit.extent.Extent;
import com.sk89q.worldedit.extent.logging.AbstractLoggingExtent;

public class PrismWorldEditLogger extends AbstractLoggingExtent {
	private final Actor player;
	private final World world;

	public PrismWorldEditLogger(Actor player, Extent extent, World world) {
		super(extent);
		this.player = player;
		this.world = world;
	}

	@Override
	protected void onBlockChange(Vector pt, BaseBlock newBlock) {
		if (!Prism.config.getBoolean("prism.tracking.world-edit"))
			return;

		Location loc = BukkitUtil.toLocation(world, pt);
		Block oldBlock = loc.getBlock();

		byte data = oldBlock.getData();

		Material newMaterial = Material.matchMaterial(BlockType.fromID(newBlock.getId()).name());

		RecordingQueue.addToQueue(ActionFactory.createBlockChange("world-edit", loc, oldBlock.getType(), data,
				newMaterial, (byte) newBlock.getData(), Bukkit.getPlayer(player.getUniqueId())));
	}
}