package me.botsko.prism.serializers.entity;

import me.botsko.prism.utils.MiscUtils;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Parrot;

import java.util.concurrent.atomic.AtomicReference;

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
    protected void niceName(AtomicReference<String> name) {
        name.set(name.get().replace("<prefix",MiscUtils.niceName(var)));
    }
}
