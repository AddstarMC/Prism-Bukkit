package me.botsko.prism.serializers.entity;

import org.bukkit.Bukkit;
import org.bukkit.entity.Enderman;
import org.bukkit.entity.Entity;

/**
 * Created for the Prism-Bukkit Project.
 * Created by Narimm on 20/10/2020.
 */
public class EndermanSerializer extends EntitySerializer {

    private String blockData;

    @Override
    protected void serializer(Entity entity) {
        super.serializer(entity);
        if (((Enderman) entity).getCarriedBlock() != null) {
            blockData = ((Enderman) entity).getCarriedBlock().toString();
        }
    }

    @Override
    protected void deserializer(Entity entity) {
        super.deserializer(entity);
        if (blockData != null) {
            ((Enderman) entity).setCarriedBlock(Bukkit.createBlockData(blockData));
        }
    }
}
