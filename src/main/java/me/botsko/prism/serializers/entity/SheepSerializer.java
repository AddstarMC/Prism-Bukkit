package me.botsko.prism.serializers.entity;

import me.botsko.prism.utils.MiscUtils;
import org.bukkit.DyeColor;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Sheep;

public class SheepSerializer extends EntitySerializer {
    protected String color = null;

    @Override
    protected void serializer(Entity entity) {
        color = ((Sheep) entity).getColor().name().toLowerCase();
    }

    @Override
    protected void deserializer(Entity entity) {
        ((Sheep) entity).setColor(MiscUtils.getEnum(color, DyeColor.WHITE));
    }

    @Override
    protected void niceName(StringBuilder sb, int start) {
        if (color != null) {
            sb.insert(start, MiscUtils.niceName(color)).insert(start + color.length(), ' ');
        }
    }
}
