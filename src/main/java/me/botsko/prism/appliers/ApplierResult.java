package me.botsko.prism.appliers;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.events.BlockStateChange;
import org.bukkit.entity.Entity;

import java.util.ArrayList;
import java.util.HashMap;

public class ApplierResult {


    private final int changesApplied;

    private final int changesSkipped;

    private final int changesPlanned;

    private final boolean isPreview;

    private final HashMap<Entity, Integer> entitiesMoved;

    private final ArrayList<BlockStateChange> blockStateChanges;

    private final QueryParameters params;

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
                         ArrayList<BlockStateChange> blockStateChanges, QueryParameters params,
                         HashMap<Entity, Integer> entitiesMoved) {
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


    public HashMap<Entity, Integer> getEntitiesMoved() {
        return entitiesMoved;
    }


    public ArrayList<BlockStateChange> getBlockStateChanges() {
        return blockStateChanges;
    }

    public PrismProcessType getProcessType() {
        return params.getProcessType();
    }

    public QueryParameters getParameters() {
        return params;
    }
}