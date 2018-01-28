package me.botsko.prism.actions.entity;

import org.bukkit.entity.Entity;
import org.bukkit.entity.Ocelot;

import me.botsko.prism.utils.MiscUtils;

public class OcelotSerializer extends EntitySerializer {
	protected String var = null;
	
	@Override
	protected void serializer(Entity entity) {
		var = ((Ocelot)entity).getCatType().name().toLowerCase();
	}
	
	@Override
	protected void deserializer(Entity entity) {
		// Tamed vs untamed ocelot
		Ocelot.Type value = taming_owner != null ? Ocelot.Type.BLACK_CAT : Ocelot.Type.WILD_OCELOT;
		value = MiscUtils.getEnum(var, value);
		
		((Ocelot)entity).setCatType(value);
	}
	
	@Override
	protected void niceName(StringBuilder sb, int start) {
		if(var != null)
			sb.insert(start, MiscUtils.niceName(var)).insert(start + var.length(), ' ');
	}
}
