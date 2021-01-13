package me.botsko.prism.actionlibs;

import me.botsko.prism.api.actions.ActionType;
import me.botsko.prism.api.actions.Handler;

public class ActionTypeImpl implements ActionType {

    private final boolean doesCreateBlock;
    private final boolean canRollback;
    private final boolean canRestore;
    private final Class<? extends Handler> handler;
    private final String niceDescription;
    private final String name;

    /**
     * Create An ActionType. -  it cant be restored rolled back and doesnt create a block.
     *
     * @param name            Name
     * @param handler         Handler
     * @param niceDescription nice description.
     */
    @SuppressWarnings("unused")
    public ActionTypeImpl(String name, Class<? extends Handler> handler, String niceDescription) {
        this(name, false, false, false, handler, niceDescription);
    }

    /**
     * Create An ActionType.
     *
     * @param name            Name
     * @param doesCreateBlock true if creates a block
     * @param canRollback     can we roll this back
     * @param canRestore      can it be restored
     * @param handler         Handler
     * @param niceDescription nice description.
     */
    public ActionTypeImpl(String name, boolean doesCreateBlock, boolean canRollback, boolean canRestore,
                          Class<? extends Handler> handler, String niceDescription) {
        this.doesCreateBlock = doesCreateBlock;
        this.canRollback = canRollback;
        this.canRestore = canRestore;
        this.handler = handler;
        this.niceDescription = niceDescription;
        this.name = name;
    }

    public boolean canRollback() {
        return canRollback;
    }

    public boolean canRestore() {
        return canRestore;
    }

    public Class<? extends Handler> getHandler() {
        return handler;
    }

    /**
     * Returns the nice name of the action.
     *
     * @return String
     */
    String getNiceDescription() {
        return niceDescription;
    }

    /**
     *  Checks if an Action requires a specific handler.
     * @param handler Handler
     * @return boolean
     */
    @SuppressWarnings("BooleanMethodIsAlwaysInverted")
    public boolean requiresHandler(Class<? extends Handler> handler) {
        return (getHandler() != null && getHandler().equals(handler));
    }

    public boolean doesCreateBlock() {
        return doesCreateBlock;
    }

    public String getName() {
        return name;
    }

    /**
     * Get the Class family name.
     * @return String
     */
    String getFamilyName() {
        final String[] _tmp = this.name.toLowerCase().split("-(?!.*-.*)");
        if (_tmp.length == 2) {
            return _tmp[0];
        }
        return name;
    }

    /**
     * Get the Class Short name.
     * @return String
     */
    public String getShortName() {
        final String[] _tmp = this.name.toLowerCase().split("-(?!.*-.*)");
        if (_tmp.length == 2) {
            return _tmp[1];
        }
        return name;
    }

}