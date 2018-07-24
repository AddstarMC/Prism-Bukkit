package me.botsko.prism.bridge;

import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;

import org.bukkit.Bukkit;
import org.bukkit.Location;
import org.bukkit.Material;
import org.bukkit.World;
import org.bukkit.block.Block;
import org.bukkit.block.data.BlockData;

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

		Material newMaterial = Material.matchMaterial(BlockType.fromID(newBlock.getId()).name());

		// TODO: When worldedit has some way of getting blockdata
		BlockData newData = Bukkit.createBlockData(newMaterial);

		RecordingQueue.addToQueue(ActionFactory.createBlockChange("world-edit", loc, oldBlock.getType(),
				oldBlock.getBlockData(), newMaterial, newData, Bukkit.getPlayer(player.getUniqueId())));
	}
}