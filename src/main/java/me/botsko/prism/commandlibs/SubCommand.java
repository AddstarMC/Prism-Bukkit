package me.botsko.prism.commandlibs;

import org.bukkit.entity.Player;

public final class SubCommand {

    private final String[] commandAliases;
    private String[] permissionNodes;
    private int minArgs = 0;
    private boolean allow_console = false;
    private SubHandler handler = null;

    /**
     * 
     * @param commandAliases
     * @param permissionNodes
     */
    public SubCommand(String[] commandAliases, String[] permissionNodes) {
        this.commandAliases = commandAliases;
        this.permissionNodes = permissionNodes;
    }

    /**
     * 
     * @param commandAliases
     * @param permissionNodes
     * @param handler
     */
    public SubCommand(String[] commandAliases, String[] permissionNodes, SubHandler handler) {
        this( commandAliases, permissionNodes );
        this.handler = handler;
    }

    /**
     * 
     * @return
     */
    public SubCommand allowConsole() {
        this.allow_console = true;
        return this;
    }

    /**
     * 
     * @return
     */
    public boolean isConsoleAllowed() {
        return this.allow_console;
    }

    /**
     * 
     * @return
     */
    public int getMinArgs() {
        return minArgs;
    }

    /**
     * 
     * @param minArgs
     * @return
     */
    public SubCommand setMinArgs(int minArgs) {
        this.minArgs = minArgs;
        return this;
    }

    /**
     * 
     * @return
     */
    public SubHandler getHandler() {
        return handler;
    }

    /**
     * 
     * @param handler
     * @return
     */
    public SubCommand setHandler(SubHandler handler) {
        this.handler = handler;
        return this;
    }

    /**
     * 
     * @return
     */
    public boolean playerHasPermission(Player player) {
        for ( String node : permissionNodes ) {
            if( player.hasPermission( node ) ) { return true; }
            // Also check for global nodes
            if( node.contains( "*" ) )
                continue;

            int index = node.lastIndexOf( '.' );
            while ( index != -1 ) {
                node = node.substring( 0, index );
                if( player.hasPermission( node + ".*" ) ) { return true; }
                index = node.lastIndexOf( '.' );
            }
        }
        return false;
    }

    /**
     * 
     * @param permissionNodes
     */
    public void setPermNodes(String[] permissionNodes) {
        this.permissionNodes = permissionNodes;
    }

    /**
     * 
     * @return aliases
     */
    public String[] getAliases() {
        return this.commandAliases;
    }
}