package me.botsko.prism.actions.entity;

import org.bukkit.OfflinePlayer;
import org.bukkit.entity.Ageable;
import org.bukkit.entity.Entity;
import org.bukkit.entity.LivingEntity;
import org.bukkit.entity.Projectile;
import org.bukkit.entity.Sittable;
import org.bukkit.entity.Tameable;
import org.bukkit.entity.Zombie;
import org.bukkit.event.entity.EntityDamageByEntityEvent;
import org.bukkit.event.entity.EntityDamageEvent;

import me.botsko.prism.utils.EntityUtils;
import me.botsko.prism.utils.MiscUtils;

public class EntitySerializer {
	protected Boolean isAdult = null;
	protected Boolean sitting = null;
	protected String entity_name = null;
	protected String custom_name = null;
	protected String taming_owner = null;
	protected String newColor = null;
	protected String custom_desc = null;

	public final String getEntityName() {
		return entity_name;
	}

	public final String customDesc() {
		return custom_desc;
	}

	// Le sigh
	public final void setNewColor(String color) {
		newColor = color;
	}

	public final void serialize(Entity entity) {
		entity_name = entity.getType().name().toLowerCase();

		// Get custom name
		custom_name = entity.getCustomName();

		// Get animal age
		if (entity instanceof Ageable) {
			isAdult = ((Ageable) entity).isAdult();
		}
		else if (entity instanceof Zombie) {
			isAdult = !((Zombie) entity).isBaby();
		}

		// Owner
		if (entity instanceof Tameable) {
			final Tameable mob = (Tameable) entity;
			if (mob.getOwner() != null)
				taming_owner = mob.getOwner().getUniqueId().toString();
			else if (mob.isTamed())
				taming_owner = "-none-";
		}

		// Sitting
		if (entity instanceof Sittable) {
			sitting = ((Sittable) entity).isSitting();
		}

		EntityDamageEvent damageEvent = entity.getLastDamageCause();

		// Saves us the null check
		if (damageEvent instanceof EntityDamageByEntityEvent && !damageEvent.isCancelled()
				&& damageEvent.getDamage() > ((LivingEntity) entity).getHealth()) {
			EntityDamageByEntityEvent e = (EntityDamageByEntityEvent) damageEvent;

			if (e.getDamager() instanceof Projectile) {
				custom_desc = EntityUtils.getCustomProjectileDescription((Projectile) e.getDamager());
			}
		}

		serializer(entity);
	}

	protected void serializer(Entity entity) {
	}

	public final void deserialize(Entity entity) {
		// Get custom name
		if (entity instanceof LivingEntity && custom_name != null) {
			final LivingEntity namedEntity = (LivingEntity) entity;
			namedEntity.setCustomName(custom_name);
		}

		// Get animal age
		if (entity instanceof Ageable) {
			final Ageable age = (Ageable) entity;
			if (Boolean.FALSE.equals(isAdult)) {
				age.setBaby();
			}
			else {
				age.setAdult();
			}
		}
		else if (entity instanceof Zombie) {
			((Zombie) entity).setBaby(Boolean.FALSE.equals(isAdult));
		}

		// Owner
		if (entity instanceof Tameable) {
			((Tameable) entity).setOwner(EntityUtils.offlineOf(taming_owner));
		}

		// Sitting
		if (entity instanceof Sittable) {
			((Sittable) entity).setSitting(Boolean.TRUE.equals(sitting));
		}

		deserializer(entity);
	}

	protected void deserializer(Entity entity) {
	}

	@Override
	public final String toString() {
		StringBuilder sb = new StringBuilder();
		int index = 0;
		if (taming_owner != null) {
			
			OfflinePlayer player = EntityUtils.offlineOf(taming_owner);
			if (player != null) {
				String str = player.getName() + "'s ";
				sb.append(str);
				index = str.length();
			}
		}

		if (Boolean.FALSE.equals(isAdult))
			sb.append("baby ");

		sb.append(MiscUtils.niceName(entity_name));

		if (newColor != null)
			sb.append(' ').append(MiscUtils.niceName(newColor));

		if (custom_name != null)
			sb.append(" named ").append(custom_name);

		niceName(sb, index);
		return sb.toString();
	}

	protected void niceName(StringBuilder sb, int start) {
	}
}
