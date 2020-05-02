package me.botsko.prism.commandlibs;

import org.bukkit.command.CommandSender;
import org.bukkit.entity.Player;

public class CallInfo {

    private final CommandSender sender;
    private final Player player;
    private final String[] args;

    /**
     * Constructor.
     * @param sender CommandSender
     * @param player Player
     * @param args String[]
     */
    public CallInfo(CommandSender sender, Player player, String[] args) {
        this.sender = sender;
        this.player = player;
        this.args = args;
    }

    /**
     * Get Player.
     * @return Player
     */
    public Player getPlayer() {
        return player;
    }

    /**
     * Get Sender.
     * @return CommandSender
     */
    public CommandSender getSender() {
        return sender;
    }

    /**
     * Get arg number n.
     * @param n int
     * @return String.
     */
    public String getArg(int n) {
        return this.args[n];
    }

    /**
     * Get all args.
     * @return String[]
     */
    public String[] getArgs() {
        return this.args;
    }
}
