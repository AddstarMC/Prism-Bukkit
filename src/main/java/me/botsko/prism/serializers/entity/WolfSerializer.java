package me.botsko.prism.serializers.entity;

import me.botsko.prism.utils.MiscUtils;
import org.bukkit.DyeColor;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Wolf;

import java.util.concurrent.atomic.AtomicReference;

public class WolfSerializer extends EntitySerializer {
    protected String color = null;

    @Override
    protected void serializer(Entity entity) {
        color = ((Wolf) entity).getCollarColor().name().toLowerCase();
    }

    @Override
    protected void deserializer(Entity entity) {
        ((Wolf) entity).setCollarColor(MiscUtils.getEnum(color, DyeColor.RED));
    }

    @Override
    protected void niceName(AtomicReference<String> name) {
        name.set(name.get().replace("<prefix",MiscUtils.niceName(color)));
    }
}
