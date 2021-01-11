package me.botsko.prism.listeners;

import io.papermc.paper.event.block.TargetHitEvent;
import io.papermc.paper.event.player.PlayerTradeEvent;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import org.bukkit.block.Block;
import org.bukkit.entity.Player;
import org.bukkit.entity.Projectile;
import org.bukkit.event.EventHandler;
import org.bukkit.event.EventPriority;
import org.bukkit.event.Listener;
import org.bukkit.inventory.ItemStack;
import org.bukkit.projectiles.ProjectileSource;

/**
 * This class carries listeners that will only work currently with paper.
 *
 * @author Narimm on 1/01/2021.
 */
public class PaperListeners implements Listener {

    Prism plugin;
    public PaperListeners(Prism plugin) {
        this.plugin = plugin;
    }

    @EventHandler(priority = EventPriority.MONITOR)
    public void onTargetHitEvent(TargetHitEvent event){
        Projectile projectile = event.getEntity();
        ProjectileSource shooter = projectile.getShooter();
        if(shooter instanceof Player) {
            if (!Prism.getIgnore().event("target-hit", (Player) shooter)) {
                return;
            }
            Block block = event.getHitBlock();
            RecordingQueue.addToQueue(ActionFactory.createBlock("target-hit",block,(Player) shooter));
        }
    }

    @EventHandler(priority = EventPriority.MONITOR)
    public void onPlayerTrade(PlayerTradeEvent event) {
        Player player = event.getPlayer();
        if (!Prism.getIgnore().event("player-trade", player)) {
            return;
        }
        RecordingQueue.addToQueue(ActionFactory.createEntity("player-trade",event.getVillager(),player));
        ItemStack result = event.getTrade().getResult();
        RecordingQueue.addToQueue(ActionFactory.createItemStack("item-receive",result,result.getAmount(),
                -1,result.getEnchantments(),event.getVillager().getLocation(),player));

    }

}
