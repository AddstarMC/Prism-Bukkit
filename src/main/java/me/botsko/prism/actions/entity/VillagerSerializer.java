package me.botsko.prism.actions.entity;

import me.botsko.prism.utils.MiscUtils;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Villager;
import org.bukkit.entity.Villager.Profession;
import org.bukkit.inventory.MerchantRecipe;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class VillagerSerializer extends AbstractVillagerSerializer {

    protected String type = null;
    protected String profession = null;
    protected Integer villagerXp = 0;
    protected List<VillagerRecipe> recipes = new ArrayList<>();

    @Override
    protected void serializer(Entity entity) {
        super.serializer(entity);
        type = ((Villager) entity).getVillagerType().name().toLowerCase();
        profession = ((Villager) entity).getProfession().name().toLowerCase();
        villagerXp = ((Villager) entity).getVillagerExperience();
        for (MerchantRecipe recipe : ((Villager) entity).getRecipes()) {
            final VillagerRecipe r = new VillagerRecipe();
            r.result = PrismItemStack.fromBukkit(recipe.getResult());
            r.ingredient = new ArrayList<>();
            recipe.getIngredients().forEach(rec -> r.ingredient.add(PrismItemStack.fromBukkit(rec)));
            r.experienceReward = recipe.hasExperienceReward();
            r.maxUses = recipe.getMaxUses();
            r.currentUses = recipe.getUses();
            r.villagerXp = recipe.getVillagerExperience();
            r.priceMultiplier = recipe.getPriceMultiplier();
            recipes.add(r);
        }
    }

    @Override
    protected void deserializer(Entity entity) {
        super.deserializer(entity);
        ((Villager) entity).setProfession(MiscUtils.getEnum(profession, Profession.FARMER));
        ((Villager) entity).setVillagerExperience(villagerXp);
        ((Villager) entity).setVillagerType(MiscUtils.getEnum(type, Villager.Type.PLAINS));
        List<MerchantRecipe> bukkitRecipes = new ArrayList<>();
        recipes.forEach(villagerRecipe -> bukkitRecipes.add(
              new MerchantRecipe(villagerRecipe.result.toBukkit(), villagerRecipe.currentUses,
              villagerRecipe.maxUses, villagerRecipe.experienceReward, villagerRecipe.villagerXp,
              villagerRecipe.priceMultiplier)));
        ((Villager) entity).setRecipes(bukkitRecipes);
    }

    @Override
    protected void niceName(StringBuilder sb, int start) {
        if (profession != null) {
            String detail = MiscUtils.niceName(type) + " " + MiscUtils.niceName(profession + " ");
            sb.insert(start, detail);
        }
    }

    public static class VillagerRecipe implements Serializable {

        Float priceMultiplier;
        PrismItemStack result;
        List<PrismItemStack> ingredient;
        Boolean experienceReward;
        Integer maxUses;
        Integer currentUses;
        Integer villagerXp;
    }

}
