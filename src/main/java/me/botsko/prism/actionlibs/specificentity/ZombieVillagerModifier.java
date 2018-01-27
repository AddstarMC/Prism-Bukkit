package me.botsko.prism.actionlibs.specificentity;

import org.bukkit.entity.Entity;
import org.bukkit.entity.ZombieVillager;
import org.bukkit.entity.Villager.Profession;

import me.botsko.prism.utils.MiscUtils;

public class ZombieVillagerModifier extends EntityModifier {
	protected String profession = null;
	
	@Override
	protected void serializer(Entity entity) {
		profession = ((ZombieVillager)entity).getVillagerProfession().name().toLowerCase();
	}
	
	@Override
	protected void deserializer(Entity entity) {
		((ZombieVillager)entity).setVillagerProfession(MiscUtils.getEnum(profession, Profession.FARMER));
	}
	
	@Override
	protected void niceName(StringBuilder sb, int start) {
		if(profession != null)
			sb.insert(start, MiscUtils.niceName(profession)).insert(start + profession.length(), ' ');
	}
}
