package me.botsko.prism.actionlibs.specificentity;

import org.bukkit.DyeColor;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Wolf;

import me.botsko.prism.utils.MiscUtils;

public class WolfModifier extends EntityModifier {
	protected String color = null;
	
	@Override
	protected void serializer(Entity entity) {
		color = ((Wolf)entity).getCollarColor().name().toLowerCase();
	}
	
	@Override
	protected void deserializer(Entity entity) {
		((Wolf)entity).setCollarColor(MiscUtils.getEnum(color, DyeColor.RED));
	}
	
	@Override
	protected void niceName(StringBuilder sb, int start) {
		if(color != null)
			sb.insert(start, MiscUtils.niceName(color)).insert(start + color.length(), ' ');
	}
}
