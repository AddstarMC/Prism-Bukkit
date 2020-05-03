package me.botsko.prism.actions.entity;

import me.botsko.prism.utils.MiscUtils;
import org.bukkit.entity.Entity;
import org.bukkit.entity.Panda;

public class PandaSerializer extends EntitySerializer {
    protected String mainGene = null;
    protected String hiddenGene = null;

    @Override
    protected void serializer(Entity entity) {
        mainGene = ((Panda) entity).getMainGene().name().toLowerCase();
        hiddenGene = ((Panda) entity).getHiddenGene().name().toLowerCase();
    }

    @Override
    protected void deserializer(Entity entity) {
        Panda.Gene mainPandaGene = MiscUtils.getEnum(mainGene, Panda.Gene.NORMAL);
        Panda.Gene hiddenPandaGene = MiscUtils.getEnum(hiddenGene, Panda.Gene.NORMAL);
        ((Panda) entity).setMainGene(mainPandaGene);
        ((Panda) entity).setHiddenGene(hiddenPandaGene);
    }

    @Override
    protected void niceName(StringBuilder sb, int start) {
        if (mainGene != null && hiddenGene != null) {
            String niceName;
            if ("weak".equals(mainGene) && !"weak".equals(hiddenGene)) {
                niceName = "normal";
            } else if ("brown".equals(mainGene) && !"brown".equals(hiddenGene)) {
                niceName = "normal";
            } else {
                niceName = mainGene;
            }
            sb.insert(start, MiscUtils.niceName(niceName)).insert(start + niceName.length(), ' ');
        }
    }
}
