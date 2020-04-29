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
        Panda.Gene mGene = MiscUtils.getEnum(mainGene, Panda.Gene.NORMAL);
        Panda.Gene hGene = MiscUtils.getEnum(hiddenGene, Panda.Gene.NORMAL);
        ((Panda) entity).setMainGene(mGene);
        ((Panda) entity).setHiddenGene(hGene);
}

    @Override
    protected void niceName(StringBuilder sb, int start) {
        if (mainGene != null && hiddenGene != null) {
            String niceName;
            if (mainGene.equals("weak") && !hiddenGene.equals("weak")) {
                niceName = "normal";
            } else if (mainGene.equals("brown") && !hiddenGene.equals("brown")) {
                niceName = "normal";
            } else {
                niceName = mainGene;
            }
            sb.insert(start, MiscUtils.niceName(niceName)).insert(start + niceName.length(), ' ');
        }
    }
}
