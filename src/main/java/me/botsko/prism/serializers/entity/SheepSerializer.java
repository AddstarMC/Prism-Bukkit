package me.botsko.prism.serializers.entity;

import me.botsko.prism.utils.MiscUtils;
import org.bukkit.DyeColor;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Sheep;

import java.util.concurrent.atomic.AtomicReference;

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
    protected void niceName(AtomicReference<String> name) {
        name.set(name.get().replace("<prefix",MiscUtils.niceName(color)));
    }
}
