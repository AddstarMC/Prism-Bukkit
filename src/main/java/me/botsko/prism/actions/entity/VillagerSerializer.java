package me.botsko.prism.actions.entity;

import org.bukkit.configuration.InvalidConfigurationException;
import org.bukkit.configuration.file.YamlConfiguration;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Villager;
import org.bukkit.entity.Villager.Profession;
import org.bukkit.entity.Villager.Type;
import org.bukkit.inventory.ItemStack;
import org.bukkit.inventory.MerchantRecipe;

import me.botsko.prism.utils.MiscUtils;

import java.util.ArrayList;
import java.util.List;

public class VillagerSerializer extends EntitySerializer {
	protected String profession = null;
	protected String type = null;
	protected int level;
	protected int experience;
	protected List<String> recipes = new ArrayList<>();

	@Override
	protected void serializer(Entity entity) {
		Villager villager = (Villager) entity;

		profession = villager.getProfession().name().toLowerCase();
		type = villager.getVillagerType().name().toLowerCase();
		level = villager.getVillagerLevel();
		experience = villager.getVillagerExperience();

		for (MerchantRecipe merchantRecipe : villager.getRecipes()) {
			YamlConfiguration cfg = new YamlConfiguration();
			cfg.set("result", merchantRecipe.getResult());
			cfg.set("ingredientA", merchantRecipe.getIngredients().get(0));
			cfg.set("ingredientB", merchantRecipe.getIngredients().get(1));
			cfg.set("uses", merchantRecipe.getUses());
			cfg.set("maxUses", merchantRecipe.getMaxUses());
			cfg.set("hasReward", merchantRecipe.hasExperienceReward());
			cfg.set("exp", merchantRecipe.getVillagerExperience());
			cfg.set("multiplier", merchantRecipe.getPriceMultiplier());
			recipes.add(cfg.saveToString());
		}
	}

	@Override
	protected void deserializer(Entity entity) {
		Villager villager = (Villager) entity;
		villager.setProfession(MiscUtils.getEnum(profession, Profession.FARMER));
		villager.setVillagerType(MiscUtils.getEnum(type, Type.PLAINS));
		if (level >= 1 && level <= 5) {
			villager.setVillagerLevel(level);
		}
		villager.setVillagerExperience(experience);

		YamlConfiguration yml = new YamlConfiguration();
		List<MerchantRecipe> merchantRecipes = new ArrayList<>();

		for (String recipe : recipes) {
			try {
				yml.loadFromString(recipe);
				ItemStack result = yml.getItemStack("result");
				ItemStack ingredientA = yml.getItemStack("ingredientA");
				ItemStack ingredientB = yml.getItemStack("ingredientB");

				if (result != null && ingredientA != null && ingredientB != null) {
					MerchantRecipe merchantRecipe = new MerchantRecipe(result, yml.getInt("uses"), yml.getInt("maxUses"),
							yml.getBoolean("hasReward"), yml.getInt("exp"), yml.getLong("multiplier"));
					merchantRecipe.addIngredient(ingredientA);
					merchantRecipe.addIngredient(ingredientB);

					merchantRecipes.add(merchantRecipe);
				}
			} catch (InvalidConfigurationException e) {
				e.printStackTrace();
			}
			if (merchantRecipes.size() > 0) {
				villager.setRecipes(merchantRecipes);
			}
		}
	}

	@Override
	protected void niceName(StringBuilder sb, int start) {
		if (profession != null)
			sb.insert(start, MiscUtils.niceName(profession)).insert(start + profession.length(), ' ');
	}
}
