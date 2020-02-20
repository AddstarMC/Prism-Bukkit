package me.botsko.prism.actions.entity;

import org.bukkit.attribute.Attribute;
import org.bukkit.attribute.AttributeInstance;
import org.bukkit.entity.AbstractHorse;
import org.bukkit.entity.ChestedHorse;
import org.bukkit.entity.Entity;
import org.bukkit.entity.EntityType;
import org.bukkit.entity.Horse;
import org.bukkit.entity.Llama;

import me.botsko.prism.utils.ItemUtils;
import me.botsko.prism.utils.MiscUtils;

public class AbstractHorseSerializer extends EntitySerializer {
	protected String hColor = null;
	protected String style = null;
	protected String saddle = null;
	protected String armor = null;
	protected Boolean chest = null;
	protected int dom = 0;
	protected int maxDom = 20;
	protected double jump = 1.0;
	protected double maxHealth = 20.0;
	protected double movementSpeed = 0.2;

	@Override
	protected void serializer(Entity entity) {
		final AbstractHorse h = (AbstractHorse) entity;

		// TODO: Cleanup
		if (entity.getType() == EntityType.HORSE) {
			Horse horse = (Horse) h;
			hColor = horse.getColor().name();
			style = horse.getStyle().name();
			saddle = ItemUtils.smallString(horse.getInventory().getSaddle());
			armor = ItemUtils.smallString(horse.getInventory().getArmor());
		}
		else if (entity.getType() == EntityType.LLAMA) {
			Llama llama = (Llama) h;
			hColor = llama.getColor().name();
			saddle = ItemUtils.smallString(llama.getInventory().getDecor());
		}
		else if (entity.getType() == EntityType.MULE || entity.getType() == EntityType.DONKEY
				|| entity.getType() == EntityType.ZOMBIE_HORSE || entity.getType() == EntityType.SKELETON_HORSE) {
			// Actually a saddle
			saddle = ItemUtils.smallString(h.getInventory().getItem(0));
		}

		if (entity instanceof ChestedHorse)
			chest = ((ChestedHorse) entity).isCarryingChest();

		dom = h.getDomestication();
		maxDom = h.getMaxDomestication();
		jump = h.getJumpStrength();
		maxHealth = h.getAttribute(Attribute.GENERIC_MAX_HEALTH).getBaseValue();

		AttributeInstance attributeInstance = h.getAttribute(Attribute.GENERIC_MOVEMENT_SPEED);
		if (attributeInstance != null) {
			movementSpeed = attributeInstance.getBaseValue();
		}
	}

	@Override
	protected void deserializer(Entity entity) {
		final AbstractHorse h = (AbstractHorse) entity;

		if (entity.getType() == EntityType.HORSE) {
			Horse horse = (Horse) h;
			Horse.Color color = MiscUtils.getEnum(hColor, Horse.Color.WHITE);
			Horse.Style vstyle = MiscUtils.getEnum(style, Horse.Style.NONE);
			horse.setColor(color);
			horse.setStyle(vstyle);
			horse.getInventory().setSaddle(ItemUtils.itemOf(saddle));
			horse.getInventory().setArmor(ItemUtils.itemOf(armor));
		}
		else if (entity.getType() == EntityType.LLAMA) {
			Llama llama = (Llama) h;
			Llama.Color color = MiscUtils.getEnum(hColor, Llama.Color.CREAMY);
			llama.setColor(color);
			llama.getInventory().setDecor(ItemUtils.itemOf(saddle));
		}
		else if (entity.getType() == EntityType.DONKEY || entity.getType() == EntityType.MULE
				|| entity.getType() == EntityType.ZOMBIE_HORSE || entity.getType() == EntityType.SKELETON_HORSE) {
			h.getInventory().setItem(0, ItemUtils.itemOf(saddle));
		}

		if (entity instanceof ChestedHorse) {
			((ChestedHorse) h).setCarryingChest(Boolean.TRUE.equals(chest));
		}

		maxDom = Math.max(1, maxDom);
		dom = Math.min(Math.max(0, dom), maxDom);
		jump = Math.min(Math.max(0.0, jump), 2.0);

		h.setDomestication(dom);
		h.setMaxDomestication(maxDom);
		h.setJumpStrength(jump);
		h.getAttribute(Attribute.GENERIC_MAX_HEALTH).setBaseValue(maxHealth);

		AttributeInstance attributeInstance =  h.getAttribute(Attribute.GENERIC_MOVEMENT_SPEED);
		if (attributeInstance != null) {
			attributeInstance.setBaseValue(movementSpeed);
		}
	}

	@Override
	protected void niceName(StringBuilder sb, int start) {
		if (hColor != null) {
			sb.insert(start, MiscUtils.niceName(hColor)).insert(start + hColor.length(), ' ');
			start += hColor.length() + 1;
		}

		if (style != null) {
			sb.insert(start, MiscUtils.niceName(style)).insert(start + style.length(), ' ');
			// start += style.length() + 1;
		}
	}
}
