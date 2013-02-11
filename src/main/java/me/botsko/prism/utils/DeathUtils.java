package me.botsko.prism.utils;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Arrow;
import org.bukkit.entity.Blaze;
import org.bukkit.entity.CaveSpider;
import org.bukkit.entity.Creeper;
import org.bukkit.entity.EnderDragon;
import org.bukkit.entity.Enderman;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Ghast;
import org.bukkit.entity.MagmaCube;
import org.bukkit.entity.PigZombie;
import org.bukkit.entity.Player;
import org.bukkit.entity.Silverfish;
import org.bukkit.entity.Skeleton;
import org.bukkit.entity.Slime;
import org.bukkit.entity.Spider;
import org.bukkit.entity.Wolf;
import org.bukkit.entity.Zombie;
import org.bukkit.event.entity.EntityDamageByEntityEvent;
import org.bukkit.event.entity.EntityDamageEvent;
import org.bukkit.event.entity.EntityDamageEvent.DamageCause;
import org.bukkit.event.entity.EntityDeathEvent;
import org.bukkit.inventory.ItemStack;

public class DeathUtils {

	
	/**
	 * 
	 * http://jd.bukkit.org/apidocs/org/bukkit/event/entity/EntityDamageEvent.DamageCause.html
	 * 
	 * @param event
	 * @param p
	 * @return String
	 */
	public static String getCauseOfDeath(EntityDeathEvent event, Player p){
		
		EntityDamageEvent e = event.getEntity().getLastDamageCause();
		
		if(e == null){
			return "unknown";
		}
		
		// Determine the root cause
		DamageCause damageCause = e.getCause();
        String cause = e.getCause().toString().toLowerCase();
        
        // Detect additional suicide. For example, when you potion
        // yourself with instant damage it doesn't show as suicide.
        if(p.getKiller() instanceof Player){
        	Player killer = p.getKiller();
        	if(killer.getName() == p.getName()){
        		cause = "suicide";
        	}
        }
        
        // translate bukkit events to nicer names
        if(damageCause.equals(DamageCause.ENTITY_ATTACK) && p.getKiller() instanceof Player){
        	cause = "pvp";
        }
        if(damageCause.equals(DamageCause.ENTITY_ATTACK) && !(p.getKiller() instanceof Player)){
        	cause = "mob";
        }
        if(damageCause.equals(DamageCause.PROJECTILE) && !(p.getKiller() instanceof Player)){
        	cause = "skeleton";
        }
        if(damageCause.equals(DamageCause.PROJECTILE) && (p.getKiller() instanceof Player)){
        	cause = "pvp"; // bow and arrow
        }
        if(damageCause.equals(DamageCause.ENTITY_EXPLOSION)){
        	cause = "creeper"; // creeper
        }
        if(damageCause.equals(DamageCause.CONTACT)){
        	cause = "cactus";
        }
        if(damageCause.equals(DamageCause.BLOCK_EXPLOSION)){
        	cause = "tnt";
        }
        if(damageCause.equals(DamageCause.FIRE) || damageCause.equals(DamageCause.FIRE_TICK)){
        	cause = "fire";
        }
        if(damageCause.equals(DamageCause.MAGIC)){
        	cause = "potion";
        }

        return cause;
		
	}
	
	
	/**
	 * Returns the name of the attacker, whether mob or player.
	 * 
	 * @param event
	 * @param p
	 * @return
	 */
	public static String getAttacker(EntityDeathEvent event, Player p){
		
		String attacker = "";
		String cause = getCauseOfDeath(event, p);
        if(p.getKiller() instanceof Player){
        	attacker = p.getKiller().getName();
        } else {
            if(cause == "mob"){
            	
            	Entity killer = ((EntityDamageByEntityEvent)event.getEntity().getLastDamageCause()).getDamager();
            	
            	// @todo clean this up
            	if (killer instanceof Blaze){
            		attacker = "blaze";
            	}
            	if (killer instanceof CaveSpider){
            		attacker = "cave spider";
            	}
            	if (killer instanceof Creeper){
            		attacker = "creeper";
            	}
            	if (killer instanceof EnderDragon){
            		attacker = "ender dragon";
            	}
            	if (killer instanceof Enderman){
            		attacker = "enderman";
            	}
            	if (killer instanceof Ghast){
            		attacker = "ghast";
            	}
            	if (killer instanceof MagmaCube){
            		attacker = "magma cube";
            	}
            	if (killer instanceof PigZombie){
            		attacker = "pig zombie";
            	}
            	if (killer instanceof Silverfish){
            		attacker = "silverfish";
            	}
            	if (killer instanceof Skeleton){
            		attacker = "skeleton";
            	}
            	if (killer instanceof Arrow){
            		attacker = "skeleton";
            	}
            	if (killer instanceof Slime){
            		attacker = "slime";
            	}
            	if (killer instanceof Spider){
            		attacker = "spider";
            	}
            	if (killer instanceof Wolf){
                    Wolf wolf = (Wolf)killer;
                    if(wolf.isTamed()){
                        if(wolf.getOwner() instanceof Player || wolf.getOwner() instanceof OfflinePlayer ){
                            attacker = "pvpwolf";
                        } else {
                        	attacker = "wolf";
                        }
                    } else {
                    	attacker = "wolf";
                    }
            		
            	}
            	if (killer instanceof Zombie){
            		attacker = "zombie";
            	}
            }
        }
        
        return attacker;
        
	}
	
	
	/**
	 * Determines the owner of a tamed wolf.
	 * 
	 * @param event
	 * @return
	 */
	public static String getTameWolfOwner(EntityDeathEvent event){
		String owner = "";
		Entity killer = ((EntityDamageByEntityEvent)event.getEntity().getLastDamageCause()).getDamager();
		if (killer instanceof Wolf){
            Wolf wolf = (Wolf)killer;
            if(wolf.isTamed()){
                if(wolf.getOwner() instanceof Player){
                    owner = ((Player)wolf.getOwner()).getName();
                }
                if(wolf.getOwner() instanceof OfflinePlayer){
                    owner = ((OfflinePlayer)wolf.getOwner()).getName();
                }
            }
    	}
		return owner;
	}
	
	
	/**
	 * Determines the weapon used.
	 * 
	 * @param p
	 * @return
	 */
	public static String getWeapon(Player p){

        String death_weapon = "";
        if(p.getKiller() instanceof Player){
        	ItemStack weapon = p.getKiller().getItemInHand();
        	death_weapon = weapon.getType().toString().toLowerCase();
        	death_weapon = death_weapon.replaceAll("_", " ");
        	if(death_weapon.equalsIgnoreCase("air")){
        		death_weapon = " hands";
        	}
        }
        
        return death_weapon;
        
	}
}