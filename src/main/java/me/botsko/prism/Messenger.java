package me.botsko.prism;

import org.bukkit.ChatColor;

public class Messenger {

    /**
	 * 
	 */
    protected final String plugin_name;

    /**
     * 
     * @param plugin_name
     */
    public Messenger(String plugin_name) {
        this.plugin_name = plugin_name;
    }

    /**
     * 
     * @param msg
     * @return
     */
    public String playerHeaderMsg(String msg) {
        if( msg != null ) { return ChatColor.LIGHT_PURPLE + plugin_name + " // " + ChatColor.WHITE + msg; }
        return "";
    }

    /**
     * 
     * @param msg
     * @return
     */
    public String playerSubduedHeaderMsg(String msg) {
        if( msg != null ) { return ChatColor.LIGHT_PURPLE + plugin_name + " // " + ChatColor.GRAY + msg; }
        return "";
    }

    /**
     * 
     * @param msg
     * @return
     */
    public String playerMsg(String msg) {
        if( msg != null ) { return ChatColor.WHITE + msg; }
        return "";
    }

    /**
     * 
     * @param msg
     * @return
     */
    public String[] playerMsg(String[] msg) {
        if( msg != null ) {
            for ( int i = 0; i < msg.length; i++ ) {
                msg[i] = playerMsg( msg[i] );
            }
        }
        return msg;
    }

    /**
     * 
     * @param cmd
     * @param help
     */
    public String playerHelp(String cmd, String help) {
        return ChatColor.GRAY + "/prism " + ChatColor.LIGHT_PURPLE + cmd + ChatColor.WHITE + " - " + help;
    }

    /**
     * 
     * @param msg
     * @return
     */
    public String playerError(String msg) {
        if( msg != null ) { return ChatColor.LIGHT_PURPLE + plugin_name + " // " + ChatColor.RED + msg; }
        return "";
    }

    /**
     * 
     * @param msg
     * @return
     */
    public String playerSuccess(String msg) {
        if( msg != null ) { return ChatColor.LIGHT_PURPLE + plugin_name + " // " + ChatColor.GREEN + msg; }
        return "";
    }
}