package me.botsko.prism.serializers.entity;

import me.botsko.prism.utils.MiscUtils;
import org.bukkit.entity.Cat;
import org.bukkit.entity.Entity;

import java.util.concurrent.atomic.AtomicReference;

public class CatSerializer extends EntitySerializer {
    protected String var = null;

    @Override
    protected void serializer(Entity entity) {
        var = ((Cat) entity).getCatType().name().toLowerCase();
    }

    @Override
    protected void deserializer(Entity entity) {
        Cat.Type type = MiscUtils.getEnum(var, Cat.Type.ALL_BLACK);
        ((Cat) entity).setCatType(type);
    }

    @Override
    protected void niceName(AtomicReference<String> name) {
        name.set(name.get().replace("<prefix",MiscUtils.niceName(var)));
    }
}
