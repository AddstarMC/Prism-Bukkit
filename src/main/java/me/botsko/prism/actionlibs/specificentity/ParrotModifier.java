package me.botsko.prism.actionlibs.specificentity;

import org.bukkit.entity.Entity;
import org.bukkit.entity.Parrot;

import me.botsko.prism.utils.MiscUtils;

public class ParrotModifier extends EntityModifier {
	protected String var = null;
	
	@Override
	protected void serializer(Entity entity) {
		var = ((Parrot)entity).getVariant().name().toLowerCase();
	}
	
	@Override
	protected void deserializer(Entity entity) {
		((Parrot)entity).setVariant(MiscUtils.getEnum(var, Parrot.Variant.RED));
	}

	@Override
	protected void niceName(StringBuilder sb, int start) {
		if(var != null)
			sb.insert(start, MiscUtils.niceName(var)).insert(start + var.length(), ' ');
	}
}
