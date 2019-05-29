package me.botsko.prism.actions.entity;

import org.bukkit.entity.Cat;
import org.bukkit.entity.Entity;

import me.botsko.prism.utils.MiscUtils;

public class CatSerializer extends EntitySerializer {
	protected String var = null;

	@Override
	protected void serializer(Entity entity) {
		var = ((Cat) entity).getCatType().name().toLowerCase();
	}

	@Override
	protected void deserializer(Entity entity) {
		Cat.Type type = MiscUtils.getEnum(var, Cat.Type.ALL_BLACK);
		((Cat)entity).setCatType(type);
}

	@Override
	protected void niceName(StringBuilder sb, int start) {
		if (var != null)
			sb.insert(start, MiscUtils.niceName(var)).insert(start + var.length(), ' ');
	}
}
