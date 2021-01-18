package me.botsko.prism.api.objects;

import me.botsko.prism.api.BlockStateChange;
import me.botsko.prism.api.PrismParameters;
import me.botsko.prism.api.actions.PrismProcessType;
import org.bukkit.entity.Entity;

import java.util.List;
import java.util.Map;

public class ApplierResult {


    private final int changesApplied;

    private final int changesSkipped;

    private final int changesPlanned;

    private final boolean isPreview;

    private final Map<Entity, Integer> entitiesMoved;

    private final List<BlockStateChange> blockStateChanges;

    private final PrismParameters params;

    /**
     * Constructor.
     *
     * @param isPreview         int
     * @param changesApplied    int
     * @param changesSkipped    int
     * @param changesPlanned    int
     * @param blockStateChanges List
     * @param params            Query Params
     * @param entitiesMoved     Map
     */
    public ApplierResult(boolean isPreview, int changesApplied, int changesSkipped, int changesPlanned,
                         List<BlockStateChange> blockStateChanges, PrismParameters params,
                         Map<Entity, Integer> entitiesMoved) {
        this.changesApplied = changesApplied;
        this.changesSkipped = changesSkipped;
        this.changesPlanned = changesPlanned;
        this.isPreview = isPreview;
        this.blockStateChanges = blockStateChanges;
        this.params = params;
        this.entitiesMoved = entitiesMoved;
    }

    public int getChangesApplied() {
        return changesApplied;
    }

    public int getChangesSkipped() {
        return changesSkipped;
    }

    public int getChangesPlanned() {
        return changesPlanned;
    }


    public boolean isPreview() {
        return isPreview;
    }


    public Map<Entity, Integer> getEntitiesMoved() {
        return entitiesMoved;
    }


    public List<BlockStateChange> getBlockStateChanges() {
        return blockStateChanges;
    }

    public PrismProcessType getProcessType() {
        return params.getProcessType();
    }

    public PrismParameters getParameters() {
        return params;
    }
}