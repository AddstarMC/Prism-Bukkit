package me.botsko.prism.serializers.entity;

import me.botsko.prism.utils.MiscUtils;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Villager.Profession;
import org.bukkit.entity.ZombieVillager;

import java.util.concurrent.atomic.AtomicReference;

public class ZombieVillageSerializer extends EntitySerializer {
    protected String profession = null;

    @Override
    protected void serializer(Entity entity) {
        profession = ((ZombieVillager) entity).getVillagerProfession().name().toLowerCase();
    }

    @Override
    protected void deserializer(Entity entity) {
        ((ZombieVillager) entity).setVillagerProfession(MiscUtils.getEnum(profession, Profession.FARMER));
    }

    @Override
    protected void niceName(AtomicReference<String> name) {
        name.set(name.get()
                .replace("<suffix>","(" + MiscUtils.niceName(profession) + ")")
        );
    }
}
