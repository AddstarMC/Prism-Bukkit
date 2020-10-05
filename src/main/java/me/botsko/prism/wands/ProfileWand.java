package me.botsko.prism.wands;

import me.botsko.prism.Il8nHelper;
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
            Audience sender = Prism.getAudiences().player(player);
            sender.sendMessage(Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("profile-entity")));
            sender.sendMessage(Il8nHelper.formatMessage("profile-detail",
                    entity.getType().toString().toLowerCase(),
                    entity.getEntityId(),
                    "",
                    entity.getLocation().getBlockX(),
                    entity.getLocation().getBlockY(),
                    entity.getLocation().getBlockZ()
            ));
        }
    }

    protected void showLocationProfile(Player player, Location loc) {

        final Block block = loc.getBlock();
        Audience sender = Prism.getAudiences().player(player);

        sender.sendMessage(Prism.messenger.playerHeaderMsg(Il8nHelper.getMessage("profile-location")));
        BlockData data = block.getBlockData();
        sender.sendMessage(Il8nHelper.formatMessage("profile-detail",
                block.getType().toString().toLowerCase(),
                block.getType() + " " + Utilities.dataString(data),
                Prism.getItems().getAlias(block.getType(), data),
                block.getX(),
                block.getY(),
                block.getZ()
        ));
    }

}