package me.botsko.prism.actions.entity;

import me.botsko.prism.utils.MiscUtils;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Parrot;

public class ParrotSerializer extends EntitySerializer {
    protected String var = null;

    @Override
    protected void serializer(Entity entity) {
        var = ((Parrot) entity).getVariant().name().toLowerCase();
    }

    @Override
    protected void deserializer(Entity entity) {
        ((Parrot) entity).setVariant(MiscUtils.getEnum(var, Parrot.Variant.RED));
    }

    @Override
    protected void niceName(StringBuilder sb, int start) {
        if (var != null)
            sb.insert(start, MiscUtils.niceName(var)).insert(start + var.length(), ' ');
    }
}
