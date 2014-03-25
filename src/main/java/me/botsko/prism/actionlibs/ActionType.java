package me.botsko.prism.actionlibs;

public class ActionType {

    /**
     * Define associated values
     */
    private final boolean doesCreateBlock;
    private final boolean canRollback;
    private final boolean canRestore;
    private final String handler;
    private final String niceDescription;
    private final String name;

    /**
     * 
     * @param name
     * @param handler
     * @param niceDescription
     */
    public ActionType(String name, String handler, String niceDescription) {
        this( name, false, false, false, handler, niceDescription );
    }

    /**
     * 
     * @param name
     * @param doesCreateBlock
     * @param canRollback
     * @param canRestore
     * @param niceDescription
     */
    public ActionType(String name, boolean doesCreateBlock, boolean canRollback, boolean canRestore, String handler,
            String niceDescription) {
        this.doesCreateBlock = doesCreateBlock;
        this.canRollback = canRollback;
        this.canRestore = canRestore;
        this.handler = handler;
        this.niceDescription = niceDescription;
        this.name = name;
    }

    /**
     * @return the canRollback
     */
    public boolean canRollback() {
        return canRollback;
    }

    /**
     * @return the canRestore
     */
    public boolean canRestore() {
        return canRestore;
    }

    /**
     * @return the niceDescription
     */
    public String getHandler() {
        return handler;
    }

    /**
     * @return the niceDescription
     */
    public String getNiceDescription() {
        return niceDescription;
    }

    /**
     * 
     * @param handler
     * @return
     */
    public boolean requiresHandler(String handler) {
        return ( getHandler() != null && getHandler().equals( handler ) );
    }

    /**
     * 
     * @return
     */
    public boolean doesCreateBlock() {
        return doesCreateBlock;
    }

    /**
     * 
     * @return
     */
    public String getName() {
        return name;
    }

    /**
     * 
     * @return
     */
    public String getFamilyName() {
        final String[] _tmp = this.name.toLowerCase().split( "-(?!.*-.*)" );
        if( _tmp.length == 2 ) { return _tmp[0]; }
        return name;
    }

    /**
     * 
     * @return
     */
    public String getShortName() {
        final String[] _tmp = this.name.toLowerCase().split( "-(?!.*-.*)" );
        if( _tmp.length == 2 ) { return _tmp[1]; }
        return name;
    }

    // /**
    // * Returns whether or not an action type should also
    // * trigger a restore action after an applier.
    // *
    // * This is a pretty inefficient way to define the
    // * relationships but it's really the only way I
    // * can think of atm.
    // *
    // * @param at
    // * @return
    // */
    // public boolean shouldTriggerRestoreFor(String at){
    //
    // // Actions that should trigger sign changes
    // if(at.equals("sign-change")){
    // if( this.name.equals("block-break")
    // || this.name.equals("block-burn")
    // || this.name.equals("creeper-explode")
    // || this.name.equals("enderman-pickup")
    // || this.name.equals("tnt-explode")){
    // return true;
    // }
    // }
    // return false;
    // }

    /**
     * Returns whether or not an action type should also trigger a rollback
     * action after an applier.
     * 
     * This is a pretty inefficient way to define the relationships but it's
     * really the only way I can think of atm.
     * 
     * @param at
     * @return
     */
    public boolean shouldTriggerRollbackFor(String at) {

        // // Actions that should trigger item removal rollback
        // if(at.equals(ActionType.ITEM_REMOVE)){
        // switch(this){
        // case BLOCK_BREAK:
        // case BLOCK_BURN:
        // case CREEPER_EXPLODE:
        // case TNT_EXPLODE:
        // return true;
        // default:
        // return false;
        // }
        // }
        return false;
    }
}