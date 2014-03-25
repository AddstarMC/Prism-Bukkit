package me.botsko.prism.appliers;

import java.util.ArrayList;
import java.util.HashMap;

import org.bukkit.entity.Entity;

import me.botsko.prism.actionlibs.QueryParameters;
import me.botsko.prism.events.BlockStateChange;

public class ApplierResult {

    /**
	 * 
	 */
    private final int changes_applied;

    /**
	 * 
	 */
    private final int changes_skipped;

    /**
	 * 
	 */
    private final int changes_planned;

    /**
	 * 
	 */
    private final boolean is_preview;

    /**
	 * 
	 */
    private final HashMap<Entity, Integer> entities_moved;

    /**
	 * 
	 */
    private final ArrayList<BlockStateChange> blockStateChanges;

    /**
	 * 
	 */
    private final QueryParameters params;

    /**
     * 
     * @param changes_applied
     * @param changes_skipped
     * @param changes_planned
     * @param blockStateChanges
     * @param params
     * @param entities_moved
     */
    public ApplierResult(boolean is_preview, int changes_applied, int changes_skipped, int changes_planned,
            ArrayList<BlockStateChange> blockStateChanges, QueryParameters params,
            HashMap<Entity, Integer> entities_moved) {
        this.changes_applied = changes_applied;
        this.changes_skipped = changes_skipped;
        this.changes_planned = changes_planned;
        this.is_preview = is_preview;
        this.blockStateChanges = blockStateChanges;
        this.params = params;
        this.entities_moved = entities_moved;
    }

    /**
     * @return the changes_applied
     */
    public int getChangesApplied() {
        return changes_applied;
    }

    /**
     * @return the changes_skipped
     */
    public int getChangesSkipped() {
        return changes_skipped;
    }

    /**
     * 
     * @return
     */
    public int getChangesPlanned() {
        return changes_planned;
    }

    /**
     * @return the is_preview
     */
    public boolean isPreview() {
        return is_preview;
    }

    /**
     * 
     * @return
     */
    public HashMap<Entity, Integer> getEntitiesMoved() {
        return entities_moved;
    }

    /**
     * @return the undo
     */
    public ArrayList<BlockStateChange> getBlockStateChanges() {
        return blockStateChanges;
    }

    /**
     * 
     * @return
     */
    public PrismProcessType getProcessType() {
        return params.getProcessType();
    }

    /**
     * 
     * @return
     */
    public QueryParameters getParameters() {
        return params;
    }
}