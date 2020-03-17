package me.botsko.prism;

import org.bukkit.ChatColor;

public class Messenger {

    private final String pluginName;

    /**
     * Build the class.
     *
     * @param pluginName String
     */
    @SuppressWarnings("WeakerAccess")
    public Messenger(String pluginName) {
        this.pluginName = pluginName;
    }

    /**
     * Get the header.
     *
     * @param msg the message to prefix.
     * @return String.
     */
    public String playerHeaderMsg(String msg) {
        if (msg != null) {
            return ChatColor.LIGHT_PURPLE + pluginName + " // " + ChatColor.WHITE + msg;
        }
        return "";
    }

    /**
     * Get the subdued header.
     *
     * @param msg the message to prefix.
     * @return String.
     */
    public String playerSubduedHeaderMsg(String msg) {
        if (msg != null) {
            return ChatColor.LIGHT_PURPLE + pluginName + " // " + ChatColor.GRAY + msg;
        }
        return "";
    }

    /**
     * Get the message.
     *
     * @param msg the message to prefix.
     * @return String.
     */
    public String playerMsg(String msg) {
        if (msg != null) {
            return ChatColor.WHITE + msg;
        }
        return "";
    }

    /**
     * Get the message String[].
     *
     * @param msg the message to prefix.
     * @return String[].
     */
    public String[] playerMsg(String[] msg) {
        if (msg != null) {
            for (int i = 0; i < msg.length; i++) {
                msg[i] = playerMsg(msg[i]);
            }
        }
        return msg;
    }

    /**
     * Get the help message.
     *
     * @param cmd  cmd to get help.
     * @param help - a message.
     * @return String.
     */
    public String playerHelp(String cmd, String help) {
        return ChatColor.GRAY + "/prism " + ChatColor.LIGHT_PURPLE + cmd + ChatColor.WHITE + " - " + help;
    }

    /**
     * Get the error message.
     *
     * @param msg the message to prefix.
     * @return String.
     */
    public String playerError(String msg) {
        if (msg != null) {
            return ChatColor.LIGHT_PURPLE + pluginName + " // " + ChatColor.RED + msg;
        }
        return "";
    }

    /**
     * Get the Success message.
     *
     * @param msg the message to prefix.
     * @return String.
     */
    public String playerSuccess(String msg) {
        if (msg != null) {
            return ChatColor.LIGHT_PURPLE + pluginName + " // " + ChatColor.GREEN + msg;
        }
        return "";
    }

}