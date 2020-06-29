package me.botsko.prism.listeners;

import io.papermc.lib.PaperLib;
import me.botsko.prism.Prism;
import me.botsko.prism.actionlibs.ActionFactory;
import me.botsko.prism.actionlibs.RecordingQueue;
import org.bukkit.Material;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Player;
import org.bukkit.entity.Trident;
import org.bukkit.event.EventHandler;
import org.bukkit.event.Listener;
import org.bukkit.event.entity.ProjectileHitEvent;
import org.bukkit.event.entity.ProjectileLaunchEvent;
import org.bukkit.event.player.PlayerPickupArrowEvent;
import org.bukkit.inventory.ItemStack;

import java.util.Collections;
import java.util.List;

/**
 * Created for use for the Add5tar MC Minecraft server
 * Created by benjamincharlton on 29/06/2020.
 */
public class PrismProjectileEvents implements Listener {
    /**
     * Track Projectiles.
     * @param event ProjectileLaunchEvent
     */
    @EventHandler(ignoreCancelled = true)
    public void onProjectileFired(ProjectileLaunchEvent event) {
        if (!Prism.getIgnore().event("projectile-launch")) {
            return;
        }
        if (event.getEntity().getShooter() instanceof Player) {
            Player shooter = (Player) event.getEntity().getShooter();
            ItemStack item;
            if (PaperLib.isPaper()) {
                item = ((Trident) event.getEntity()).getItemStack();
                RecordingQueue.addToQueue(ActionFactory.createItemStack("projectile-launch", item, null,
                        shooter.getLocation(), shooter));
            } else {
                if (event.getEntity().getType() == EntityType.TRIDENT) {
                    item = new ItemStack(Material.TRIDENT, 1);
                } else {
                    item = new ItemStack(Material.ARROW, 1);
                }
                List<String> lore = Collections.singletonList("Empty Item - Use PaperMC to get the real item.");
                item.getItemMeta().setLore(lore);
            }
            RecordingQueue.addToQueue(ActionFactory.createItemStack("projectile-launch", item, null,
                    shooter.getLocation(), shooter));

            // RecordingQueue.addToQueue(ActionFactory.createProjectile("projectile-fire", event.getEntity(),shooter));
        }
    }

    /**
     * Track Projectiles.
     * @param event ProjectileHitEvent
     */
    @EventHandler(ignoreCancelled = true)
    public void onProjectileHit(ProjectileHitEvent event) {
        if (!Prism.getIgnore().event("projectile-hit")) {
            return;
        }
        String projectileName;
        if (event.getEntity().getShooter() instanceof Player) {
            Player shooter = (Player) event.getEntity().getShooter();
            projectileName = shooter.getDisplayName() + "'s " + event.getEntity().getName();
            if (event.getHitEntity() != null) {
                RecordingQueue.addToQueue(ActionFactory.createEntity("projectile-hit-entity",
                        event.getHitEntity(),projectileName));
            }
            if (event.getHitBlock() != null) {
                RecordingQueue.addToQueue(ActionFactory.createBlock("projectile-hit-block",
                        event.getHitBlock(),projectileName));
            }
        }
    }

    /**
     * Track Projectiles .
     * @param event PlayerPickupArrowEvent
     */
    @EventHandler(ignoreCancelled = true)
    public void onProjectilePickup(PlayerPickupArrowEvent event) {
        if (!Prism.getIgnore().event("projectile-pickup")) {
            return;
        }
        Player p = event.getPlayer();
        RecordingQueue.addToQueue(ActionFactory.createItemStack("projectile-pickup", event.getItem().getItemStack(),
                event.getItem().getItemStack().getAmount(), -1, null, p.getLocation(), p));
    }


}

