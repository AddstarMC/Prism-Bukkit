package me.botsko.prism.wands;

import me.botsko.prism.Prism;
import me.botsko.prism.utils.block.Utilities;
import net.kyori.adventure.audience.Audience;
import org.bukkit.Location;
import org.bukkit.block.Block;
import org.bukkit.block.data.BlockData;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Player;

public class ProfileWand extends WandBase {

    @Override
    public void playerLeftClick(Player player, Location loc) {
        if (loc != null) {
            showLocationProfile(player, loc);
        }
    }

    @Override
    public void playerRightClick(Player player, Location loc) {
        if (loc != null) {
            showLocationProfile(player, loc);
        }
    }

    @Override
    public void playerRightClick(Player player, Entity entity) {
        if (entity != null) {
            Audience sender = Prism.getAudiences().audience(player);
            sender.sendMessage(Prism.messenger.playerHeaderMsg("Entity Profile"));
            sender.sendMessage(Prism.messenger.playerMsg("Name: " + entity.getType().toString().toLowerCase()));
            sender.sendMessage(Prism.messenger.playerMsg("ID: " + entity.getEntityId()));
            sender.sendMessage(Prism.messenger.playerMsg("Coords: " + entity.getLocation().getBlockX() + " "
                    + entity.getLocation().getBlockY() + " " + entity.getLocation().getBlockZ()));
        }
    }

    protected void showLocationProfile(Player player, Location loc) {

        final Block block = loc.getBlock();
        Audience sender = Prism.getAudiences().audience(player);

        sender.sendMessage(Prism.messenger.playerHeaderMsg("Location Profile"));

        BlockData data = block.getBlockData();
        sender.sendMessage(Prism.messenger.playerMsg("Name: " + block.getType().toString().toLowerCase()));
        sender.sendMessage(Prism.messenger.playerMsg("Alias: " + Prism.getItems().getAlias(block.getType(), data)));
        sender.sendMessage(Prism.messenger.playerMsg("ID: " + block.getType() + " " + Utilities.dataString(data)));
        sender.sendMessage(
                Prism.messenger.playerMsg("Coords: " + block.getX() + " " + block.getY() + " " + block.getZ()));

    }

}